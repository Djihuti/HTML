namespace SpaceEngine

// =====================================================================================
//  Math3D.fs  -  Vektor-, Quaternionen- und Matrix-Kern der Engine
//
//  Konventionen
//    * Rechtshaendiges Koordinatensystem: +X rechts, +Y oben, -Z vorne (OpenGL).
//    * Winkel in Radiant.
//    * Matrizen column-major (Index = Spalte * 4 + Zeile), direkt WebGL-kompatibel.
//    * Quat.multiply a b  ==  "erst b, dann a" (wie Matrizen: (a*b)*v = a*(b*v)).
//      Lokale Rotation eines Schiffs:  q' = q * delta.
//
//  Allokationsverhalten unter Fable (wichtig fuer den Render-Loop)
//    * JavaScript kennt keine Value-Types. Ein [<Struct>] wird von Fable zu einer
//      normalen JS-Klasse - jede Vec3/Quat-Operation, die einen neuen Wert liefert,
//      ist also ein (sehr kleines, kurzlebiges) Objekt. Das ist fuer eine Handvoll
//      Entitaeten pro Frame (Spieler, Kamera) unkritisch: V8 legt sie im Nursery ab
//      und ersetzt sie nach dem JIT-Warmup meist per Escape-Analysis durch Skalare.
//    * Alles, was pro Frame hundertfach laeuft (Instanzen, Pools, Matrizen), geht
//      ueber die allokationsfreien Pfade: Mat4 ist ein per [<Erase>] entkernter
//      Wrapper um ein Float32Array, alle Mat4-Funktionen schreiben in ein
//      vorab angelegtes Ziel ("out"-Parameter), und Mat4.composeTRS schreibt
//      Skalare direkt in einen Instanz-Buffer.
// =====================================================================================

open Fable.Core

/// Unveraenderlicher 3D-Vektor.
[<Struct>]
type Vec3 =
    val X: float
    val Y: float
    val Z: float
    new(x: float, y: float, z: float) = { X = x; Y = y; Z = z }

    static member inline (+)(a: Vec3, b: Vec3) = Vec3(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    static member inline (-)(a: Vec3, b: Vec3) = Vec3(a.X - b.X, a.Y - b.Y, a.Z - b.Z)
    static member inline (*)(a: Vec3, s: float) = Vec3(a.X * s, a.Y * s, a.Z * s)
    static member inline (*)(s: float, a: Vec3) = Vec3(a.X * s, a.Y * s, a.Z * s)
    static member inline (/)(a: Vec3, s: float) = Vec3(a.X / s, a.Y / s, a.Z / s)
    static member inline (~-)(a: Vec3) = Vec3(-a.X, -a.Y, -a.Z)

[<RequireQualifiedAccess>]
module Vec3 =

    let zero = Vec3(0.0, 0.0, 0.0)
    let one = Vec3(1.0, 1.0, 1.0)
    let unitX = Vec3(1.0, 0.0, 0.0)
    let unitY = Vec3(0.0, 1.0, 0.0)
    let unitZ = Vec3(0.0, 0.0, 1.0)

    /// Lokale Achsen eines Objekts in Engine-Konvention.
    let right = Vec3(1.0, 0.0, 0.0)
    let up = Vec3(0.0, 1.0, 0.0)
    let forward = Vec3(0.0, 0.0, -1.0)

    let inline create (x: float) (y: float) (z: float) = Vec3(x, y, z)

    let inline add (a: Vec3) (b: Vec3) = Vec3(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    let inline sub (a: Vec3) (b: Vec3) = Vec3(a.X - b.X, a.Y - b.Y, a.Z - b.Z)
    let inline scale (s: float) (a: Vec3) = Vec3(a.X * s, a.Y * s, a.Z * s)
    let inline negate (a: Vec3) = Vec3(-a.X, -a.Y, -a.Z)

    /// a + b * s  (ein einziges Ergebnisobjekt statt zwei).
    let inline mulAdd (a: Vec3) (b: Vec3) (s: float) =
        Vec3(a.X + b.X * s, a.Y + b.Y * s, a.Z + b.Z * s)

    let inline dot (a: Vec3) (b: Vec3) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

    let inline cross (a: Vec3) (b: Vec3) =
        Vec3(a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

    let inline lengthSq (a: Vec3) = a.X * a.X + a.Y * a.Y + a.Z * a.Z
    let inline length (a: Vec3) = sqrt (a.X * a.X + a.Y * a.Y + a.Z * a.Z)

    let inline distanceSq (a: Vec3) (b: Vec3) =
        let dx = a.X - b.X
        let dy = a.Y - b.Y
        let dz = a.Z - b.Z
        dx * dx + dy * dy + dz * dz

    let inline distance (a: Vec3) (b: Vec3) = sqrt (distanceSq a b)

    /// Normalisiert; der Nullvektor bleibt der Nullvektor (kein NaN).
    let normalize (a: Vec3) =
        let l2 = a.X * a.X + a.Y * a.Y + a.Z * a.Z

        if l2 > 1e-24 then
            let inv = 1.0 / sqrt l2
            Vec3(a.X * inv, a.Y * inv, a.Z * inv)
        else
            zero

    /// Lineare Interpolation, t = 0 -> a, t = 1 -> b.
    let inline lerp (a: Vec3) (b: Vec3) (t: float) =
        Vec3(a.X + (b.X - a.X) * t, a.Y + (b.Y - a.Y) * t, a.Z + (b.Z - a.Z) * t)

    /// Begrenzt die Laenge auf maxLength.
    let clampLength (maxLength: float) (a: Vec3) =
        let l2 = a.X * a.X + a.Y * a.Y + a.Z * a.Z

        if l2 > maxLength * maxLength then
            let s = maxLength / sqrt l2
            Vec3(a.X * s, a.Y * s, a.Z * s)
        else
            a

    /// Schreibt den Vektor ohne Allokation in ein Float32Array.
    let inline writeTo (target: JS.Float32Array) (offset: int) (a: Vec3) =
        target.[offset] <- float32 a.X
        target.[offset + 1] <- float32 a.Y
        target.[offset + 2] <- float32 a.Z

/// Einheitsquaternion fuer Orientierungen (X, Y, Z = Vektoranteil, W = Skalar).
[<Struct>]
type Quat =
    val X: float
    val Y: float
    val Z: float
    val W: float
    new(x: float, y: float, z: float, w: float) = { X = x; Y = y; Z = z; W = w }

    /// Hamilton-Produkt: (a * b) rotiert erst mit b, dann mit a.
    static member inline (*)(a: Quat, b: Quat) =
        Quat(
            a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
            a.W * b.Y - a.X * b.Z + a.Y * b.W + a.Z * b.X,
            a.W * b.Z + a.X * b.Y - a.Y * b.X + a.Z * b.W,
            a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z
        )

[<RequireQualifiedAccess>]
module Quat =

    let identity = Quat(0.0, 0.0, 0.0, 1.0)

    let inline create (x: float) (y: float) (z: float) (w: float) = Quat(x, y, z, w)

    let inline dot (a: Quat) (b: Quat) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z + a.W * b.W

    /// Renormalisiert (gegen numerische Drift nach vielen Multiplikationen).
    let normalize (q: Quat) =
        let l2 = q.X * q.X + q.Y * q.Y + q.Z * q.Z + q.W * q.W

        if l2 > 1e-24 then
            let inv = 1.0 / sqrt l2
            Quat(q.X * inv, q.Y * inv, q.Z * inv, q.W * inv)
        else
            identity

    /// Inverse einer Einheitsquaternion.
    let inline conjugate (q: Quat) = Quat(-q.X, -q.Y, -q.Z, q.W)

    /// Rotation um eine (beliebig lange) Achse. Nullachse ergibt die Identitaet.
    let fromAxisAngle (axis: Vec3) (angle: float) =
        let l2 = axis.X * axis.X + axis.Y * axis.Y + axis.Z * axis.Z

        if l2 > 1e-24 then
            let half = angle * 0.5
            let s = sin half / sqrt l2
            Quat(axis.X * s, axis.Y * s, axis.Z * s, cos half)
        else
            identity

    /// Hamilton-Produkt: erst b, dann a.
    let inline multiply (a: Quat) (b: Quat) =
        Quat(
            a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
            a.W * b.Y - a.X * b.Z + a.Y * b.W + a.Z * b.X,
            a.W * b.Z + a.X * b.Y - a.Y * b.X + a.Z * b.W,
            a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z
        )

    /// Integriert eine Winkelgeschwindigkeit im LOKALEN Koordinatensystem
    /// (wx = Pitch um +X, wy = Yaw um +Y, wz = Roll um +Z, in rad/s) ueber dt.
    /// Das ist der gimbal-lock-freie Kern der Flugsteuerung: q' = q * delta.
    let integrateLocal (q: Quat) (wx: float) (wy: float) (wz: float) (dt: float) =
        let w2 = wx * wx + wy * wy + wz * wz

        if w2 < 1e-20 then
            q
        else
            let w = sqrt w2
            let half = w * dt * 0.5
            let s = sin half / w
            let dx = wx * s
            let dy = wy * s
            let dz = wz * s
            let dw = cos half
            let x = q.W * dx + q.X * dw + q.Y * dz - q.Z * dy
            let y = q.W * dy - q.X * dz + q.Y * dw + q.Z * dx
            let z = q.W * dz + q.X * dy - q.Y * dx + q.Z * dw
            let ww = q.W * dw - q.X * dx - q.Y * dy - q.Z * dz
            let inv = 1.0 / sqrt (x * x + y * y + z * z + ww * ww)
            Quat(x * inv, y * inv, z * inv, ww * inv)

    /// Rotiert v mit q (optimierte Form von q * v * q^-1, ohne Zwischenobjekte).
    let rotateVec3 (q: Quat) (v: Vec3) =
        let tx = 2.0 * (q.Y * v.Z - q.Z * v.Y)
        let ty = 2.0 * (q.Z * v.X - q.X * v.Z)
        let tz = 2.0 * (q.X * v.Y - q.Y * v.X)

        Vec3(
            v.X + q.W * tx + (q.Y * tz - q.Z * ty),
            v.Y + q.W * ty + (q.Z * tx - q.X * tz),
            v.Z + q.W * tz + (q.X * ty - q.Y * tx)
        )

    /// Lokale +X-Achse in Weltkoordinaten.
    let right (q: Quat) =
        Vec3(
            1.0 - 2.0 * (q.Y * q.Y + q.Z * q.Z),
            2.0 * (q.X * q.Y + q.W * q.Z),
            2.0 * (q.X * q.Z - q.W * q.Y)
        )

    /// Lokale +Y-Achse in Weltkoordinaten.
    let up (q: Quat) =
        Vec3(
            2.0 * (q.X * q.Y - q.W * q.Z),
            1.0 - 2.0 * (q.X * q.X + q.Z * q.Z),
            2.0 * (q.Y * q.Z + q.W * q.X)
        )

    /// Blickrichtung (lokale -Z-Achse) in Weltkoordinaten.
    let forward (q: Quat) =
        Vec3(
            -2.0 * (q.X * q.Z + q.W * q.Y),
            -2.0 * (q.Y * q.Z - q.W * q.X),
            -(1.0 - 2.0 * (q.X * q.X + q.Y * q.Y))
        )

    /// Sphaerische Interpolation auf dem kuerzesten Bogen (t = 0 -> a, t = 1 -> b).
    let slerp (a: Quat) (b: Quat) (t: float) =
        let mutable bx = b.X
        let mutable by = b.Y
        let mutable bz = b.Z
        let mutable bw = b.W
        let mutable cosom = a.X * bx + a.Y * by + a.Z * bz + a.W * bw

        // q und -q beschreiben dieselbe Orientierung: kuerzeren Weg waehlen.
        if cosom < 0.0 then
            cosom <- -cosom
            bx <- -bx
            by <- -by
            bz <- -bz
            bw <- -bw

        let mutable s0 = 1.0 - t
        let mutable s1 = t

        // Nur bei ausreichend grossem Winkel echtes slerp; sonst nlerp
        // (vermeidet Division durch sin(omega) ~ 0).
        if cosom < 0.9995 then
            let omega = acos cosom
            let sinom = sin omega
            s0 <- sin ((1.0 - t) * omega) / sinom
            s1 <- sin (t * omega) / sinom

        let x = s0 * a.X + s1 * bx
        let y = s0 * a.Y + s1 * by
        let z = s0 * a.Z + s1 * bz
        let w = s0 * a.W + s1 * bw
        let inv = 1.0 / sqrt (x * x + y * y + z * z + w * w)
        Quat(x * inv, y * inv, z * inv, w * inv)

    /// Orientierung, deren -Z in Richtung `forward` zeigt und deren +Y moeglichst
    /// nah an `up` liegt (fuer KI-Ausrichtung und Kamera).
    let lookRotation (forward: Vec3) (up: Vec3) =
        // Rueckwaertsachse (+Z) = -forward, normalisiert.
        let fl = sqrt (forward.X * forward.X + forward.Y * forward.Y + forward.Z * forward.Z)

        if fl < 1e-12 then
            identity
        else
            let bx = -forward.X / fl
            let by = -forward.Y / fl
            let bz = -forward.Z / fl

            // Rechtsachse = up x back; bei Parallelitaet Ersatz-Up waehlen.
            let mutable rx = up.Y * bz - up.Z * by
            let mutable ry = up.Z * bx - up.X * bz
            let mutable rz = up.X * by - up.Y * bx
            let mutable rl = sqrt (rx * rx + ry * ry + rz * rz)

            if rl < 1e-6 then
                // Ersatz-Up: die Weltachse, die am wenigsten parallel zu back ist.
                let ux = if abs bx < 0.9 then 1.0 else 0.0
                let uy = if abs bx < 0.9 then 0.0 else 1.0
                rx <- uy * bz
                ry <- -ux * bz
                rz <- ux * by - uy * bx
                rl <- sqrt (rx * rx + ry * ry + rz * rz)

            rx <- rx / rl
            ry <- ry / rl
            rz <- rz / rl

            // Obenachse = back x right.
            let ux = by * rz - bz * ry
            let uy = bz * rx - bx * rz
            let uz = bx * ry - by * rx

            // Rotationsmatrix (Spalten: right, up, back) -> Quaternion.
            let m00 = rx
            let m10 = ry
            let m20 = rz
            let m01 = ux
            let m11 = uy
            let m21 = uz
            let m02 = bx
            let m12 = by
            let m22 = bz
            let trace = m00 + m11 + m22

            if trace > 0.0 then
                let s = sqrt (trace + 1.0) * 2.0
                Quat((m21 - m12) / s, (m02 - m20) / s, (m10 - m01) / s, 0.25 * s)
            elif m00 > m11 && m00 > m22 then
                let s = sqrt (1.0 + m00 - m11 - m22) * 2.0
                Quat(0.25 * s, (m01 + m10) / s, (m02 + m20) / s, (m21 - m12) / s)
            elif m11 > m22 then
                let s = sqrt (1.0 + m11 - m00 - m22) * 2.0
                Quat((m01 + m10) / s, 0.25 * s, (m12 + m21) / s, (m02 - m20) / s)
            else
                let s = sqrt (1.0 + m22 - m00 - m11) * 2.0
                Quat((m02 + m20) / s, (m12 + m21) / s, 0.25 * s, (m10 - m01) / s)

/// 4x4-Matrix, column-major. Per [<Erase>] bleibt im JavaScript nur das nackte
/// Float32Array uebrig: kein Wrapper-Objekt, und die "Konvertierung" fuer
/// gl.uniformMatrix4fv ist ein No-Op.
[<Erase>]
type Mat4 =
    | Mat4 of JS.Float32Array

    /// Das zugrunde liegende Float32Array (16 Elemente, column-major).
    member inline this.Data: JS.Float32Array =
        let (Mat4 d) = this
        d

[<RequireQualifiedAccess>]
module Mat4 =

    /// Schreibt die Identitaet nach `out`.
    let identity (out: Mat4) =
        let o = out.Data
        o.[0] <- 1.0f
        o.[1] <- 0.0f
        o.[2] <- 0.0f
        o.[3] <- 0.0f
        o.[4] <- 0.0f
        o.[5] <- 1.0f
        o.[6] <- 0.0f
        o.[7] <- 0.0f
        o.[8] <- 0.0f
        o.[9] <- 0.0f
        o.[10] <- 1.0f
        o.[11] <- 0.0f
        o.[12] <- 0.0f
        o.[13] <- 0.0f
        o.[14] <- 0.0f
        o.[15] <- 1.0f

    /// Neue Identitaetsmatrix. ALLOKIERT - nur beim Setup verwenden, nie pro Frame.
    let create () : Mat4 =
        let m = Mat4(JS.Constructors.Float32Array.Create 16)
        identity m
        m

    /// Direkter Zugriff auf das Float32Array (No-Op zur Laufzeit).
    let inline toFloat32Array (m: Mat4) : JS.Float32Array = m.Data

    let copy (out: Mat4) (src: Mat4) =
        let o = out.Data
        let s = src.Data

        for i in 0..15 do
            o.[i] <- s.[i]

    /// out = a * b. `out` darf identisch mit a oder b sein.
    let multiply (out: Mat4) (a: Mat4) (b: Mat4) =
        let o = out.Data
        let a = a.Data
        let b = b.Data
        let a00 = float a.[0]
        let a01 = float a.[1]
        let a02 = float a.[2]
        let a03 = float a.[3]
        let a10 = float a.[4]
        let a11 = float a.[5]
        let a12 = float a.[6]
        let a13 = float a.[7]
        let a20 = float a.[8]
        let a21 = float a.[9]
        let a22 = float a.[10]
        let a23 = float a.[11]
        let a30 = float a.[12]
        let a31 = float a.[13]
        let a32 = float a.[14]
        let a33 = float a.[15]

        let mutable b0 = float b.[0]
        let mutable b1 = float b.[1]
        let mutable b2 = float b.[2]
        let mutable b3 = float b.[3]
        o.[0] <- float32 (b0 * a00 + b1 * a10 + b2 * a20 + b3 * a30)
        o.[1] <- float32 (b0 * a01 + b1 * a11 + b2 * a21 + b3 * a31)
        o.[2] <- float32 (b0 * a02 + b1 * a12 + b2 * a22 + b3 * a32)
        o.[3] <- float32 (b0 * a03 + b1 * a13 + b2 * a23 + b3 * a33)

        b0 <- float b.[4]
        b1 <- float b.[5]
        b2 <- float b.[6]
        b3 <- float b.[7]
        o.[4] <- float32 (b0 * a00 + b1 * a10 + b2 * a20 + b3 * a30)
        o.[5] <- float32 (b0 * a01 + b1 * a11 + b2 * a21 + b3 * a31)
        o.[6] <- float32 (b0 * a02 + b1 * a12 + b2 * a22 + b3 * a32)
        o.[7] <- float32 (b0 * a03 + b1 * a13 + b2 * a23 + b3 * a33)

        b0 <- float b.[8]
        b1 <- float b.[9]
        b2 <- float b.[10]
        b3 <- float b.[11]
        o.[8] <- float32 (b0 * a00 + b1 * a10 + b2 * a20 + b3 * a30)
        o.[9] <- float32 (b0 * a01 + b1 * a11 + b2 * a21 + b3 * a31)
        o.[10] <- float32 (b0 * a02 + b1 * a12 + b2 * a22 + b3 * a32)
        o.[11] <- float32 (b0 * a03 + b1 * a13 + b2 * a23 + b3 * a33)

        b0 <- float b.[12]
        b1 <- float b.[13]
        b2 <- float b.[14]
        b3 <- float b.[15]
        o.[12] <- float32 (b0 * a00 + b1 * a10 + b2 * a20 + b3 * a30)
        o.[13] <- float32 (b0 * a01 + b1 * a11 + b2 * a21 + b3 * a31)
        o.[14] <- float32 (b0 * a02 + b1 * a12 + b2 * a22 + b3 * a32)
        o.[15] <- float32 (b0 * a03 + b1 * a13 + b2 * a23 + b3 * a33)

    /// Perspektivische Projektion (OpenGL-Clipspace, z in [-1, 1]).
    /// fovY = vertikaler Oeffnungswinkel in Radiant.
    let perspective (out: Mat4) (fovY: float) (aspect: float) (near: float) (far: float) =
        let o = out.Data
        let f = 1.0 / tan (fovY * 0.5)
        let nf = 1.0 / (near - far)
        o.[0] <- float32 (f / aspect)
        o.[1] <- 0.0f
        o.[2] <- 0.0f
        o.[3] <- 0.0f
        o.[4] <- 0.0f
        o.[5] <- float32 f
        o.[6] <- 0.0f
        o.[7] <- 0.0f
        o.[8] <- 0.0f
        o.[9] <- 0.0f
        o.[10] <- float32 ((far + near) * nf)
        o.[11] <- -1.0f
        o.[12] <- 0.0f
        o.[13] <- 0.0f
        o.[14] <- float32 (2.0 * far * near * nf)
        o.[15] <- 0.0f

    /// View-Matrix: Kamera bei `eye`, Blick auf `target`.
    let lookAt (out: Mat4) (eye: Vec3) (target: Vec3) (up: Vec3) =
        let o = out.Data
        // z = normalisiert(eye - target)  (Kamera schaut entlang -z)
        let mutable z0 = eye.X - target.X
        let mutable z1 = eye.Y - target.Y
        let mutable z2 = eye.Z - target.Z
        let mutable len = sqrt (z0 * z0 + z1 * z1 + z2 * z2)

        if len < 1e-12 then
            identity out
        else
            len <- 1.0 / len
            z0 <- z0 * len
            z1 <- z1 * len
            z2 <- z2 * len

            // x = normalisiert(up x z)
            let mutable x0 = up.Y * z2 - up.Z * z1
            let mutable x1 = up.Z * z0 - up.X * z2
            let mutable x2 = up.X * z1 - up.Y * z0
            len <- sqrt (x0 * x0 + x1 * x1 + x2 * x2)

            if len < 1e-12 then
                x0 <- 0.0
                x1 <- 0.0
                x2 <- 0.0
            else
                len <- 1.0 / len
                x0 <- x0 * len
                x1 <- x1 * len
                x2 <- x2 * len

            // y = z x x
            let y0 = z1 * x2 - z2 * x1
            let y1 = z2 * x0 - z0 * x2
            let y2 = z0 * x1 - z1 * x0

            o.[0] <- float32 x0
            o.[1] <- float32 y0
            o.[2] <- float32 z0
            o.[3] <- 0.0f
            o.[4] <- float32 x1
            o.[5] <- float32 y1
            o.[6] <- float32 z1
            o.[7] <- 0.0f
            o.[8] <- float32 x2
            o.[9] <- float32 y2
            o.[10] <- float32 z2
            o.[11] <- 0.0f
            o.[12] <- float32 (-(x0 * eye.X + x1 * eye.Y + x2 * eye.Z))
            o.[13] <- float32 (-(y0 * eye.X + y1 * eye.Y + y2 * eye.Z))
            o.[14] <- float32 (-(z0 * eye.X + z1 * eye.Y + z2 * eye.Z))
            o.[15] <- 1.0f

    /// Schreibt eine Modellmatrix (Translation * Rotation * Skalierung) aus reinen
    /// Skalaren an `offset` in ein beliebiges Float32Array. Das ist der
    /// allokationsfreie Pfad fuer Instanz-Buffer und Objekt-Pools.
    let composeTRS
        (target: JS.Float32Array)
        (offset: int)
        (px: float)
        (py: float)
        (pz: float)
        (qx: float)
        (qy: float)
        (qz: float)
        (qw: float)
        (sx: float)
        (sy: float)
        (sz: float)
        =
        let x2 = qx + qx
        let y2 = qy + qy
        let z2 = qz + qz
        let xx = qx * x2
        let xy = qx * y2
        let xz = qx * z2
        let yy = qy * y2
        let yz = qy * z2
        let zz = qz * z2
        let wx = qw * x2
        let wy = qw * y2
        let wz = qw * z2
        target.[offset] <- float32 ((1.0 - (yy + zz)) * sx)
        target.[offset + 1] <- float32 ((xy + wz) * sx)
        target.[offset + 2] <- float32 ((xz - wy) * sx)
        target.[offset + 3] <- 0.0f
        target.[offset + 4] <- float32 ((xy - wz) * sy)
        target.[offset + 5] <- float32 ((1.0 - (xx + zz)) * sy)
        target.[offset + 6] <- float32 ((yz + wx) * sy)
        target.[offset + 7] <- 0.0f
        target.[offset + 8] <- float32 ((xz + wy) * sz)
        target.[offset + 9] <- float32 ((yz - wx) * sz)
        target.[offset + 10] <- float32 ((1.0 - (xx + yy)) * sz)
        target.[offset + 11] <- 0.0f
        target.[offset + 12] <- float32 px
        target.[offset + 13] <- float32 py
        target.[offset + 14] <- float32 pz
        target.[offset + 15] <- 1.0f

    /// Modellmatrix = Translation * Rotation * Skalierung.
    let fromRotationTranslationScale (out: Mat4) (rotation: Quat) (translation: Vec3) (scale: Vec3) =
        composeTRS
            out.Data
            0
            translation.X
            translation.Y
            translation.Z
            rotation.X
            rotation.Y
            rotation.Z
            rotation.W
            scale.X
            scale.Y
            scale.Z

    /// View-Matrix direkt aus Kamera-Orientierung und -Position:
    /// die Inverse der starren Transformation T * R, also R^T * T^-1.
    /// Deutlich billiger als eine allgemeine Matrixinversion.
    let viewFromRotationTranslation (out: Mat4) (rotation: Quat) (position: Vec3) =
        let o = out.Data
        let x = rotation.X
        let y = rotation.Y
        let z = rotation.Z
        let w = rotation.W
        let x2 = x + x
        let y2 = y + y
        let z2 = z + z
        let xx = x * x2
        let xy = x * y2
        let xz = x * z2
        let yy = y * y2
        let yz = y * z2
        let zz = z * z2
        let wx = w * x2
        let wy = w * y2
        let wz = w * z2
        // Achsen der Kamera in Weltkoordinaten (Spalten von R)
        let r00 = 1.0 - (yy + zz)
        let r01 = xy + wz
        let r02 = xz - wy
        let r10 = xy - wz
        let r11 = 1.0 - (xx + zz)
        let r12 = yz + wx
        let r20 = xz + wy
        let r21 = yz - wx
        let r22 = 1.0 - (xx + yy)
        // R^T: Zeilen und Spalten vertauscht
        o.[0] <- float32 r00
        o.[1] <- float32 r10
        o.[2] <- float32 r20
        o.[3] <- 0.0f
        o.[4] <- float32 r01
        o.[5] <- float32 r11
        o.[6] <- float32 r21
        o.[7] <- 0.0f
        o.[8] <- float32 r02
        o.[9] <- float32 r12
        o.[10] <- float32 r22
        o.[11] <- 0.0f
        o.[12] <- float32 (-(r00 * position.X + r01 * position.Y + r02 * position.Z))
        o.[13] <- float32 (-(r10 * position.X + r11 * position.Y + r12 * position.Z))
        o.[14] <- float32 (-(r20 * position.X + r21 * position.Y + r22 * position.Z))
        o.[15] <- 1.0f

    /// Allgemeine Inverse. Liefert false (und laesst `out` unveraendert),
    /// wenn die Matrix singulaer ist. `out` darf identisch mit `m` sein.
    let invert (out: Mat4) (m: Mat4) : bool =
        let o = out.Data
        let a = m.Data
        let a00 = float a.[0]
        let a01 = float a.[1]
        let a02 = float a.[2]
        let a03 = float a.[3]
        let a10 = float a.[4]
        let a11 = float a.[5]
        let a12 = float a.[6]
        let a13 = float a.[7]
        let a20 = float a.[8]
        let a21 = float a.[9]
        let a22 = float a.[10]
        let a23 = float a.[11]
        let a30 = float a.[12]
        let a31 = float a.[13]
        let a32 = float a.[14]
        let a33 = float a.[15]
        let b00 = a00 * a11 - a01 * a10
        let b01 = a00 * a12 - a02 * a10
        let b02 = a00 * a13 - a03 * a10
        let b03 = a01 * a12 - a02 * a11
        let b04 = a01 * a13 - a03 * a11
        let b05 = a02 * a13 - a03 * a12
        let b06 = a20 * a31 - a21 * a30
        let b07 = a20 * a32 - a22 * a30
        let b08 = a20 * a33 - a23 * a30
        let b09 = a21 * a32 - a22 * a31
        let b10 = a21 * a33 - a23 * a31
        let b11 = a22 * a33 - a23 * a32
        let det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06

        if abs det < 1e-30 then
            false
        else
            let d = 1.0 / det
            o.[0] <- float32 ((a11 * b11 - a12 * b10 + a13 * b09) * d)
            o.[1] <- float32 ((a02 * b10 - a01 * b11 - a03 * b09) * d)
            o.[2] <- float32 ((a31 * b05 - a32 * b04 + a33 * b03) * d)
            o.[3] <- float32 ((a22 * b04 - a21 * b05 - a23 * b03) * d)
            o.[4] <- float32 ((a12 * b08 - a10 * b11 - a13 * b07) * d)
            o.[5] <- float32 ((a00 * b11 - a02 * b08 + a03 * b07) * d)
            o.[6] <- float32 ((a32 * b02 - a30 * b05 - a33 * b01) * d)
            o.[7] <- float32 ((a20 * b05 - a22 * b02 + a23 * b01) * d)
            o.[8] <- float32 ((a10 * b10 - a11 * b08 + a13 * b06) * d)
            o.[9] <- float32 ((a01 * b08 - a00 * b10 - a03 * b06) * d)
            o.[10] <- float32 ((a30 * b04 - a31 * b02 + a33 * b00) * d)
            o.[11] <- float32 ((a21 * b02 - a20 * b04 - a23 * b00) * d)
            o.[12] <- float32 ((a11 * b07 - a10 * b09 - a12 * b06) * d)
            o.[13] <- float32 ((a00 * b09 - a01 * b07 + a02 * b06) * d)
            o.[14] <- float32 ((a31 * b01 - a30 * b03 - a32 * b00) * d)
            o.[15] <- float32 ((a20 * b03 - a21 * b01 + a22 * b00) * d)
            true

    /// Transformiert einen Punkt (w = 1) inklusive perspektivischer Division.
    let transformPoint (m: Mat4) (p: Vec3) =
        let a = m.Data
        let x = float a.[0] * p.X + float a.[4] * p.Y + float a.[8] * p.Z + float a.[12]
        let y = float a.[1] * p.X + float a.[5] * p.Y + float a.[9] * p.Z + float a.[13]
        let z = float a.[2] * p.X + float a.[6] * p.Y + float a.[10] * p.Z + float a.[14]
        let w = float a.[3] * p.X + float a.[7] * p.Y + float a.[11] * p.Z + float a.[15]

        if abs w > 1e-12 && w <> 1.0 then
            Vec3(x / w, y / w, z / w)
        else
            Vec3(x, y, z)

    /// Transformiert eine Richtung (w = 0): nur Rotation/Skalierung, keine Translation.
    let transformDirection (m: Mat4) (d: Vec3) =
        let a = m.Data

        Vec3(
            float a.[0] * d.X + float a.[4] * d.Y + float a.[8] * d.Z,
            float a.[1] * d.X + float a.[5] * d.Y + float a.[9] * d.Z,
            float a.[2] * d.X + float a.[6] * d.Y + float a.[10] * d.Z
        )

    /// Die clip-space w-Komponente eines Punkts (> 0 = vor der Kamera).
    /// Wird vom HUD fuer Off-Screen-Indikatoren gebraucht.
    let clipW (m: Mat4) (p: Vec3) =
        let a = m.Data
        float a.[3] * p.X + float a.[7] * p.Y + float a.[11] * p.Z + float a.[15]

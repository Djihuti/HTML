namespace SpaceEngine

// =====================================================================================
//  ProceduralMeshes.fs  -  prozedurale Geometrie (alles Setup-Zeit, nichts pro Frame)
//
//  Alle Meshes sind flach schattiert (Low-Poly-Look): jedes Dreieck besitzt eigene
//  Vertices mit der Flaechennormale. Die Generatoren arbeiten mit einem kleinen
//  MeshBuilder, der die Windungsrichtung selbst korrigiert ("...Outward"): man
//  beschreibt nur die Form, nicht die Reihenfolge der Eckpunkte.
//
//  Schiffskonvention: Nase zeigt nach -Z, +Y ist oben, +X ist rechts.
// =====================================================================================

open System
open System.Collections.Generic
open Fable.Core

/// Kleiner deterministischer Zufallsgenerator (xorshift32). Gleicher Seed =
/// gleiche Welt - wichtig fuer reproduzierbare Meshes und Tests.
type Rng(seed: int) =
    let mutable state: uint32 = if seed = 0 then 0x9E3779B9u else uint32 seed

    /// Gleichverteilt in [0, 1).
    member _.NextFloat() : float =
        let mutable x = state
        x <- x ^^^ (x <<< 13)
        x <- x ^^^ (x >>> 17)
        x <- x ^^^ (x <<< 5)
        state <- x
        float x / 4294967296.0

    /// Gleichverteilt in [min, max).
    member this.Range(min: float, max: float) : float = min + (max - min) * this.NextFloat()

    /// Gleichverteilter Punkt auf der Einheitskugel.
    member this.UnitVector() : Vec3 =
        let z = this.Range(-1.0, 1.0)
        let angle = this.Range(0.0, 2.0 * Math.PI)
        let r = sqrt (1.0 - z * z)
        Vec3(r * cos angle, r * sin angle, z)

/// Vertexfarbe: RGB (linear) plus Emissive (0 = beleuchtet, 1 = selbstleuchtend).
type MeshColor =
    { R: float
      G: float
      B: float
      Emissive: float }

[<RequireQualifiedAccess>]
module MeshColor =
    let rgb (r: float) (g: float) (b: float) : MeshColor =
        { R = r; G = g; B = b; Emissive = 0.0 }

    let glow (r: float) (g: float) (b: float) (emissive: float) : MeshColor =
        { R = r
          G = g
          B = b
          Emissive = emissive }

    let white = rgb 1.0 1.0 1.0

/// Sammelt flach schattierte Dreiecke und erzeugt daraus ein MeshData.
type MeshBuilder() =
    let data = ResizeArray<float>()
    let mutable vertexCount = 0

    member _.VertexCount = vertexCount

    member _.Vertex(p: Vec3, n: Vec3, c: MeshColor) =
        data.Add p.X
        data.Add p.Y
        data.Add p.Z
        data.Add n.X
        data.Add n.Y
        data.Add n.Z
        data.Add c.R
        data.Add c.G
        data.Add c.B
        data.Add c.Emissive
        vertexCount <- vertexCount + 1

    /// Dreieck (gegen den Uhrzeigersinn = Vorderseite) mit eigener Farbe pro Ecke.
    /// Entartete Dreiecke (Flaeche ~ 0) werden uebersprungen.
    member this.TriangleColors(a: Vec3, b: Vec3, c: Vec3, ca: MeshColor, cb: MeshColor, cc: MeshColor) =
        let n = Vec3.cross (b - a) (c - a)

        if Vec3.lengthSq n > 1e-14 then
            let n = Vec3.normalize n
            this.Vertex(a, n, ca)
            this.Vertex(b, n, cb)
            this.Vertex(c, n, cc)

    member this.Triangle(a: Vec3, b: Vec3, c: Vec3, color: MeshColor) =
        this.TriangleColors(a, b, c, color, color, color)

    /// Dreieck, dessen Vorderseite von `inside` weg zeigt - die Reihenfolge der
    /// Eckpunkte ist egal.
    member this.TriangleOutward(a: Vec3, b: Vec3, c: Vec3, inside: Vec3, color: MeshColor) =
        let n = Vec3.cross (b - a) (c - a)
        let center = (a + b + c) / 3.0

        if Vec3.dot n (center - inside) >= 0.0 then
            this.Triangle(a, b, c, color)
        else
            this.Triangle(a, c, b, color)

    /// Konvexes Polygon als Faecher, Vorderseite von `inside` weg.
    member this.PolygonOutward(points: Vec3[], inside: Vec3, color: MeshColor) =
        for i in 1 .. points.Length - 2 do
            this.TriangleOutward(points.[0], points.[i], points.[i + 1], inside, color)

    /// Hexaeder aus 8 Punkten: [0..3] = unterer Ring, [4..7] = oberer Ring
    /// (gleiche Umlaufrichtung).
    member this.Hexahedron(p: Vec3[], color: MeshColor) =
        let mutable sum = Vec3.zero

        for q in p do
            sum <- sum + q

        let inside = sum / 8.0
        this.PolygonOutward([| p.[0]; p.[1]; p.[2]; p.[3] |], inside, color)
        this.PolygonOutward([| p.[4]; p.[5]; p.[6]; p.[7] |], inside, color)
        this.PolygonOutward([| p.[0]; p.[1]; p.[5]; p.[4] |], inside, color)
        this.PolygonOutward([| p.[1]; p.[2]; p.[6]; p.[5] |], inside, color)
        this.PolygonOutward([| p.[2]; p.[3]; p.[7]; p.[6] |], inside, color)
        this.PolygonOutward([| p.[3]; p.[0]; p.[4]; p.[7] |], inside, color)

    /// Achsenparalleler Quader.
    member this.Box(center: Vec3, half: Vec3, color: MeshColor) =
        let corner (sx: float) (sy: float) (sz: float) =
            Vec3(center.X + half.X * sx, center.Y + half.Y * sy, center.Z + half.Z * sz)

        this.Hexahedron(
            [| corner -1.0 -1.0 -1.0
               corner 1.0 -1.0 -1.0
               corner 1.0 -1.0 1.0
               corner -1.0 -1.0 1.0
               corner -1.0 1.0 -1.0
               corner 1.0 1.0 -1.0
               corner 1.0 1.0 1.0
               corner -1.0 1.0 1.0 |],
            color
        )

    /// Platte: ein Viereck-Umriss, an jeder Ecke um +/- halfThickness entlang
    /// `normal` aufgedickt (Tragflaechen, Leitwerke).
    member this.Slab(outline: Vec3[], halfThickness: float[], normal: Vec3, color: MeshColor) =
        let bottom = Array.init 4 (fun i -> outline.[i] - normal * halfThickness.[i])
        let top = Array.init 4 (fun i -> outline.[i] + normal * halfThickness.[i])
        this.Hexahedron(Array.append bottom top, color)

    /// Verbindet aufeinanderfolgende Ringe (gleiche Punktzahl) zu einer Roehre.
    /// Ein auf einen Punkt geschrumpfter Ring ergibt eine Spitze.
    member this.Loft(rings: Vec3[][], capStart: bool, capEnd: bool, color: MeshColor) =
        let centroid (ring: Vec3[]) =
            let mutable sum = Vec3.zero

            for q in ring do
                sum <- sum + q

            sum / float ring.Length

        for r in 0 .. rings.Length - 2 do
            let a = rings.[r]
            let b = rings.[r + 1]
            let n = a.Length
            let inside = (centroid a + centroid b) * 0.5

            for i in 0 .. n - 1 do
                let j = (i + 1) % n
                this.TriangleOutward(a.[i], a.[j], b.[j], inside, color)
                this.TriangleOutward(a.[i], b.[j], b.[i], inside, color)

        if capStart then
            this.PolygonOutward(rings.[0], centroid rings.[1], color)

        if capEnd then
            let last = rings.Length - 1
            this.PolygonOutward(rings.[last], centroid rings.[last - 1], color)

    member _.Build() : MeshData =
        if vertexCount > 65535 then
            failwith "Mesh hat mehr als 65535 Vertices (16-Bit-Indizes)."

        let vertices = JS.Constructors.Float32Array.Create data.Count

        for i in 0 .. data.Count - 1 do
            vertices.[i] <- float32 data.[i]

        let indices = JS.Constructors.Uint16Array.Create vertexCount

        for i in 0 .. vertexCount - 1 do
            indices.[i] <- uint16 i

        { Vertices = vertices
          Indices = indices }

/// Befestigungspunkte und Abmessungen der Schiffe in lokalen Koordinaten
/// (fuer Triebwerkslichter, Abgasflammen, Laser-Muendungen und Kollision).
module ShipHardpoints =
    let playerEngineLeft = Vec3(-0.62, 0.02, 2.0)
    let playerEngineRight = Vec3(0.62, 0.02, 2.0)
    let playerGunLeft = Vec3(-1.25, -0.1, -0.95)
    let playerGunRight = Vec3(1.25, -0.1, -0.95)
    /// Kameraposition der Cockpit-Perspektive (vor der Kanzel, Blick ueber die Nase).
    let playerCockpit = Vec3(0.0, 0.40, -1.3)
    /// Kollisionsbox (OBB) im Schiffssystem und Radius ihrer Huellkugel.
    let playerObbCenter = Vec3(0.0, 0.2, -0.3)
    let playerObbHalf = Vec3(2.1, 0.5, 2.2)
    let playerRadius = 3.2

    let interceptorEngine = Vec3(0.0, 0.0, 1.1)
    let interceptorGunLeft = Vec3(-1.2, 0.0, -2.0)
    let interceptorGunRight = Vec3(1.2, 0.0, -2.0)
    let interceptorObbCenter = Vec3(0.0, 0.0, -0.45)
    let interceptorObbHalf = Vec3(1.32, 0.42, 1.55)
    let interceptorRadius = 2.2

[<RequireQualifiedAccess>]
module ProceduralMeshes =

    /// n-Eck in der XY-Ebene bei Tiefe z, Mittelpunkt (cx, cy), Halbachsen rx/ry.
    let private ring (n: int, cx: float, cy: float, z: float, rx: float, ry: float) : Vec3[] =
        Array.init n (fun i ->
            let angle = 2.0 * Math.PI * float i / float n
            Vec3(cx + cos angle * rx, cy + sin angle * ry, z))

    /// Spiegelt Punkte an der YZ-Ebene (linke/rechte Schiffshaelfte).
    let private mirrorX (points: Vec3[]) : Vec3[] =
        points |> Array.map (fun p -> Vec3(-p.X, p.Y, p.Z))

    // ---------------------------------------------------------------------------------
    //  Spielerschiff: Delta-Wing-Fighter (ca. 4.6 lang, 4.7 Spannweite)
    // ---------------------------------------------------------------------------------

    let playerShip () : MeshData =
        let b = MeshBuilder()
        let hull = MeshColor.rgb 0.62 0.66 0.72
        let hullDark = MeshColor.rgb 0.17 0.19 0.23
        let accent = MeshColor.rgb 0.85 0.20 0.08
        let canopy = MeshColor.glow 0.05 0.20 0.36 0.3
        let engineGlow = MeshColor.glow 0.45 0.80 1.0 1.0

        // Rumpf: sechseckiger Querschnitt, flacher Bauch; die Nasenspitze als Akzent
        let nose = ring (6, 0.0, -0.02, -2.6, 0.01, 0.01)
        let f1 = ring (6, 0.0, 0.0, -1.7, 0.20, 0.15)
        let f2 = ring (6, 0.0, 0.02, -0.7, 0.40, 0.28)
        let f3 = ring (6, 0.0, 0.02, 0.8, 0.48, 0.30)
        let f4 = ring (6, 0.0, 0.04, 1.8, 0.36, 0.22)
        b.Loft([| nose; f1 |], false, false, accent)
        b.Loft([| f1; f2; f3; f4 |], false, true, hull)

        // Kanzel
        b.Loft(
            [| ring (6, 0.0, 0.12, -1.45, 0.01, 0.01)
               ring (6, 0.0, 0.20, -1.0, 0.17, 0.17)
               ring (6, 0.0, 0.27, -0.35, 0.20, 0.20)
               ring (6, 0.0, 0.26, 0.45, 0.01, 0.01) |],
            false,
            false,
            canopy
        )

        // Delta-Fluegel: an der Wurzel dick, zur Spitze duenn, leichte negative V-Stellung
        let wing =
            [| Vec3(0.38, 0.0, -0.8)
               Vec3(2.2, -0.06, 1.15)
               Vec3(2.2, -0.06, 1.7)
               Vec3(0.38, 0.0, 1.55) |]

        let wingThickness = [| 0.085; 0.025; 0.025; 0.085 |]
        b.Slab(wing, wingThickness, Vec3.unitY, hull)
        b.Slab(mirrorX wing, wingThickness, Vec3.unitY, hull)

        // Fluegelspitzen als Farbakzent
        let wingTip =
            [| Vec3(2.2, -0.06, 0.95)
               Vec3(2.34, -0.06, 1.1)
               Vec3(2.34, -0.06, 1.85)
               Vec3(2.2, -0.06, 1.85) |]

        let tipThickness = [| 0.06; 0.06; 0.06; 0.06 |]
        b.Slab(wingTip, tipThickness, Vec3.unitY, accent)
        b.Slab(mirrorX wingTip, tipThickness, Vec3.unitY, accent)

        // Doppelleitwerk, nach aussen geneigt
        let fin =
            [| Vec3(0.5, 0.1, 0.6)
               Vec3(0.8, 0.9, 1.4)
               Vec3(0.84, 0.9, 1.8)
               Vec3(0.5, 0.1, 1.7) |]

        let finThickness = [| 0.04; 0.015; 0.015; 0.04 |]
        let finNormal = Vec3.normalize (Vec3(0.8, -0.3, 0.0))
        b.Slab(fin, finThickness, finNormal, hull)
        b.Slab(mirrorX fin, finThickness, Vec3(-finNormal.X, finNormal.Y, 0.0), hull)

        for side in [| -1.0; 1.0 |] do
            // Triebwerksgondel mit leuchtender Duese
            let x = 0.62 * side
            let nozzle = ring (8, x, 0.02, 2.0, 0.19, 0.19)

            b.Loft(
                [| ring (8, x, 0.02, 0.1, 0.01, 0.01)
                   ring (8, x, 0.02, 0.55, 0.20, 0.20)
                   ring (8, x, 0.02, 1.7, 0.23, 0.23)
                   nozzle |],
                false,
                false,
                hullDark
            )

            b.PolygonOutward(nozzle, Vec3(x, 0.02, 1.0), engineGlow)

            // Laserkanone unter dem Fluegel
            b.Box(Vec3(1.25 * side, -0.1, -0.2), Vec3(0.045, 0.045, 0.75), hullDark)

        b.Build()

    // ---------------------------------------------------------------------------------
    //  Gegner: Interceptor mit zwei nach vorn ragenden Klingen
    // ---------------------------------------------------------------------------------

    let interceptor () : MeshData =
        let b = MeshBuilder()
        let hull = MeshColor.rgb 0.30 0.30 0.35
        let hullDark = MeshColor.rgb 0.13 0.13 0.16
        let accent = MeshColor.rgb 0.85 0.10 0.08
        let visor = MeshColor.glow 1.0 0.16 0.10 0.85
        let engineGlow = MeshColor.glow 1.0 0.42 0.12 1.0

        // Zentrale Kapsel mit gluehendem Hecktriebwerk
        let podEnd = ring (6, 0.0, 0.0, 1.1, 0.28, 0.20)

        b.Loft(
            [| ring (6, 0.0, 0.0, -1.5, 0.01, 0.01)
               ring (6, 0.0, 0.0, -0.8, 0.30, 0.24)
               ring (6, 0.0, 0.0, 0.3, 0.40, 0.30)
               podEnd |],
            false,
            false,
            hull
        )

        b.PolygonOutward(podEnd, Vec3(0.0, 0.0, 0.3), engineGlow)

        // Leuchtendes Visier
        b.Box(Vec3(0.0, 0.22, -0.7), Vec3(0.13, 0.05, 0.26), visor)

        // Streben zu den Klingen
        let strut =
            [| Vec3(0.3, 0.0, -0.3)
               Vec3(1.15, 0.0, -0.1)
               Vec3(1.15, 0.0, 0.55)
               Vec3(0.3, 0.0, 0.75) |]

        let strutThickness = [| 0.07; 0.045; 0.045; 0.07 |]
        b.Slab(strut, strutThickness, Vec3.unitY, hullDark)
        b.Slab(mirrorX strut, strutThickness, Vec3.unitY, hullDark)

        // Klingen: rautenfoermiger Querschnitt, rote Spitzen
        for side in [| -1.0; 1.0 |] do
            let x = 1.2 * side
            let b1 = ring (4, x, 0.0, -0.7, 0.09, 0.34)
            b.Loft([| ring (4, x, 0.0, -2.0, 0.005, 0.005); b1 |], false, false, accent)

            b.Loft(
                [| b1
                   ring (4, x, 0.0, 0.6, 0.11, 0.40)
                   ring (4, x, 0.0, 1.0, 0.05, 0.16) |],
                false,
                true,
                hull
            )

        b.Build()

    // ---------------------------------------------------------------------------------
    //  Asteroid: verformter Ikosaeder (Radius der Huellkugel = 1)
    // ---------------------------------------------------------------------------------

    /// Ikosphaere mit gemeinsamen Eckpunkten (noetig, damit die Verformung
    /// keine Risse zwischen Nachbardreiecken erzeugt).
    let private icosphere (subdivisions: int) : Vec3[] * int[] =
        let t = (1.0 + sqrt 5.0) / 2.0

        let raw =
            [| -1.0; t; 0.0
               1.0; t; 0.0
               -1.0; -t; 0.0
               1.0; -t; 0.0
               0.0; -1.0; t
               0.0; 1.0; t
               0.0; -1.0; -t
               0.0; 1.0; -t
               t; 0.0; -1.0
               t; 0.0; 1.0
               -t; 0.0; -1.0
               -t; 0.0; 1.0 |]

        let positions = ResizeArray<Vec3>()

        for i in 0..11 do
            positions.Add(Vec3.normalize (Vec3(raw.[i * 3], raw.[i * 3 + 1], raw.[i * 3 + 2])))

        let mutable triangles =
            [| 0; 11; 5
               0; 5; 1
               0; 1; 7
               0; 7; 10
               0; 10; 11
               1; 5; 9
               5; 11; 4
               11; 10; 2
               10; 7; 6
               7; 1; 8
               3; 9; 4
               3; 4; 2
               3; 2; 6
               3; 6; 8
               3; 8; 9
               4; 9; 5
               2; 4; 11
               6; 2; 10
               8; 6; 7
               9; 8; 1 |]

        let midpoints = Dictionary<int, int>()

        let midpoint (a: int) (b: int) : int =
            let key = if a < b then a * 65536 + b else b * 65536 + a

            match midpoints.TryGetValue key with
            | true, index -> index
            | _ ->
                positions.Add(Vec3.normalize ((positions.[a] + positions.[b]) * 0.5))
                let index = positions.Count - 1
                midpoints.[key] <- index
                index

        for _ in 1..subdivisions do
            let next = ResizeArray<int>()

            for f in 0 .. triangles.Length / 3 - 1 do
                let a = triangles.[f * 3]
                let b = triangles.[f * 3 + 1]
                let c = triangles.[f * 3 + 2]
                let ab = midpoint a b
                let bc = midpoint b c
                let ca = midpoint c a
                next.AddRange [| a; ab; ca; b; bc; ab; c; ca; bc; ab; bc; ca |]

            triangles <- next.ToArray()

        positions.ToArray(), triangles

    /// Kantiger Low-Poly-Asteroid. subdivisions: 1 = 80, 2 = 320 Dreiecke.
    let asteroid (seed: int) (subdivisions: int) : MeshData =
        let rng = Rng(seed)
        let positions, triangles = icosphere subdivisions

        // Grobe Form: ueberlagerte Sinus-"Beulen" entlang zufaelliger Richtungen
        let lobeCount = 5
        let lobeDirection = Array.init lobeCount (fun _ -> rng.UnitVector())
        let lobeFrequency = Array.init lobeCount (fun _ -> rng.Range(1.5, 4.0))
        let lobeAmplitude = Array.init lobeCount (fun _ -> rng.Range(0.05, 0.16))
        let lobePhase = Array.init lobeCount (fun _ -> rng.Range(0.0, 2.0 * Math.PI))
        let squashY = rng.Range(0.65, 1.0)
        let squashZ = rng.Range(0.75, 1.2)

        // Kanten: Schnittebenen kappen Teile der Oberflaeche zu ebenen Facetten
        let cutCount = 7
        let cutNormal = Array.init cutCount (fun _ -> rng.UnitVector())
        let cutDistance = Array.init cutCount (fun _ -> rng.Range(0.55, 0.85))

        let displaced =
            positions
            |> Array.map (fun direction ->
                let mutable radius = 1.0 + rng.Range(-0.035, 0.035)

                for k in 0 .. lobeCount - 1 do
                    radius <-
                        radius
                        + lobeAmplitude.[k]
                          * sin (lobeFrequency.[k] * Vec3.dot direction lobeDirection.[k] + lobePhase.[k])

                let mutable p =
                    Vec3(direction.X * radius, direction.Y * radius * squashY, direction.Z * radius * squashZ)

                for k in 0 .. cutCount - 1 do
                    let over = Vec3.dot p cutNormal.[k] - cutDistance.[k]

                    if over > 0.0 then
                        p <- p - cutNormal.[k] * over

                p)

        // Auf Huellkugelradius 1 normieren: Instanz-Skalierung = Kollisionsradius
        let maxRadius = displaced |> Array.fold (fun m p -> Math.Max(m, Vec3.length p)) 0.0
        let scale = 1.0 / maxRadius
        let b = MeshBuilder()

        for f in 0 .. triangles.Length / 3 - 1 do
            // Leichte Helligkeitsstreuung pro Facette macht den Fels lebendiger
            let shade = rng.Range(0.70, 1.0)
            let color = MeshColor.rgb shade (shade * 0.96) (shade * 0.90)

            b.Triangle(
                displaced.[triangles.[f * 3]] * scale,
                displaced.[triangles.[f * 3 + 1]] * scale,
                displaced.[triangles.[f * 3 + 2]] * scale,
                color
            )

        b.Build()

    // ---------------------------------------------------------------------------------
    //  Laser-Bolt: gestrecktes Oktaeder im Einheitswuerfel (Laenge entlang Z)
    // ---------------------------------------------------------------------------------

    /// Instanz-Skalierung (Dicke, Dicke, Laenge) ergibt die endgueltige Form.
    let laserBolt () : MeshData =
        let b = MeshBuilder()
        let color = MeshColor.glow 1.0 1.0 1.0 1.0
        let front = Vec3(0.0, 0.0, -0.5)
        let back = Vec3(0.0, 0.0, 0.5)

        let middle =
            [| Vec3(0.5, 0.0, 0.0)
               Vec3(0.0, 0.5, 0.0)
               Vec3(-0.5, 0.0, 0.0)
               Vec3(0.0, -0.5, 0.0) |]

        for i in 0..3 do
            let j = (i + 1) % 4
            b.TriangleOutward(front, middle.[i], middle.[j], Vec3.zero, color)
            b.TriangleOutward(back, middle.[i], middle.[j], Vec3.zero, color)

        b.Build()

    // ---------------------------------------------------------------------------------
    //  Glow-Sprite: Scheibe mit radialem Helligkeitsverlauf (Sterne, Sonne, Partikel)
    // ---------------------------------------------------------------------------------

    /// Scheibe in der XY-Ebene mit Radius 1, Vorderseite +Z. Die Helligkeit faellt
    /// von der Mitte (weiss) zum Rand (schwarz) ab - additiv gezeichnet entsteht ein
    /// weicher Lichtpunkt. `rings` konzentrische Ringe formen den Verlauf: 3 genuegen
    /// fuer wenige Pixel grosse Sterne, 6 ergeben ein glattes Gluehen (Sonne, Explosionen).
    /// Instanzfarben > 1 lassen den Kern zu einer festen Scheibe ausbrennen.
    let glowSprite (segments: int) (rings: int) : MeshData =
        let b = MeshBuilder()

        // Der Shader gamma-korrigiert die Ausgabe (hoch 1/2.2). Damit das Gluehen
        // auf dem Bildschirm quadratisch ausklingt, muss es linear mit (1-r)^4.4 fallen.
        let intensity (r: float) = System.Math.Pow(1.0 - r, 4.4)

        // Ringe zur Mitte hin dichter, weil die Kurve dort am steilsten ist
        let radius (k: int) =
            System.Math.Pow(float k / float rings, 1.5)

        let color (r: float) =
            let i = intensity r
            MeshColor.glow i i i 1.0

        let point (k: int) (i: int) =
            let angle = 2.0 * Math.PI * float (i % segments) / float segments
            let r = radius k
            Vec3(cos angle * r, sin angle * r, 0.0)

        for i in 0 .. segments - 1 do
            // Innerer Faecher
            b.TriangleColors(Vec3.zero, point 1 i, point 1 (i + 1), color 0.0, color (radius 1), color (radius 1))

            // Ringsegmente
            for k in 1 .. rings - 1 do
                let inner = color (radius k)
                let outer = color (radius (k + 1))
                b.TriangleColors(point k i, point (k + 1) i, point (k + 1) (i + 1), inner, outer, outer)
                b.TriangleColors(point k i, point (k + 1) (i + 1), point k (i + 1), inner, outer, inner)

        b.Build()

    // ---------------------------------------------------------------------------------
    //  Sternenhimmel: fuellt einen (kamerarelativen) Batch aus Glow-Sprites
    // ---------------------------------------------------------------------------------

    /// Sichtbare Sonne: ausgebrannter Kern plus zwei weiche Halos (3 Instanzen).
    /// Der Batch muss kamerarelativ sein und ein glattes Glow-Sprite verwenden.
    let fillSun (batch: InstanceBatch) (radius: float) (sunDirection: Vec3) =
        InstanceBatch.clear batch
        let sun = Vec3.normalize sunDirection
        let rotation = Quat.lookRotation sun Vec3.unitY
        let position = sun * radius

        let layer (size: float) (r: float) (g: float) (bl: float) =
            let s = radius * size
            InstanceBatch.push batch position rotation (Vec3(s, s, s)) r g bl 1.0

        layer 0.07 9.0 8.4 7.0 // Kern: Farbe weit ueber 1 -> feste Scheibe mit weichem Rand
        layer 0.22 1.5 1.25 0.9
        layer 0.60 0.42 0.34 0.26

    /// Verteilt Sterne auf einer Kugel mit `radius` um den Ursprung.
    /// Die Sprites zeigen zum Mittelpunkt - bei einem kamerarelativen Batch sind
    /// sie damit automatisch Billboards. Nutzt die gesamte Kapazitaet des Batches.
    let fillStarfield (batch: InstanceBatch) (seed: int) (radius: float) =
        let rng = Rng(seed)
        InstanceBatch.clear batch

        // Die Haelfte der Sterne liegt in einem geneigten Band ("Milchstrasse")
        let bandRotation = Quat.fromAxisAngle (Vec3(1.0, 0.3, 0.2)) 1.1

        while batch.Count < batch.Capacity do
            let inBand = rng.NextFloat() < 0.5

            let direction =
                if inBand then
                    let angle = rng.Range(0.0, 2.0 * Math.PI)
                    // Summe dreier Zufallszahlen ~ Glockenkurve um die Bandmitte
                    let latitude = (rng.NextFloat() + rng.NextFloat() + rng.NextFloat() - 1.5) * 0.3

                    Quat.rotateVec3
                        bandRotation
                        (Vec3(cos angle * cos latitude, sin latitude, sin angle * cos latitude))
                else
                    rng.UnitVector()

            // Wenige helle, viele schwache Sterne
            let m = rng.NextFloat()
            let brightness = m * m * m

            let size =
                if inBand then
                    radius * (0.0020 + 0.0022 * brightness)
                else
                    radius * (0.0024 + 0.0060 * brightness)

            // Werte > 1 lassen den Kern heller Sterne ausbrennen
            let intensity = (if inBand then 0.9 else 1.3) + 2.6 * brightness

            // Farbtemperatur: blau-weiss, weiss, gelblich, orange
            let temperature = rng.NextFloat()
            let mutable r = 1.0
            let mutable g = 1.0
            let mutable bl = 1.0

            if temperature < 0.2 then
                r <- 0.65
                g <- 0.78
            elif temperature < 0.65 then
                ()
            elif temperature < 0.88 then
                g <- 0.9
                bl <- 0.68
            else
                g <- 0.68
                bl <- 0.45

            InstanceBatch.push
                batch
                (direction * radius)
                (Quat.lookRotation direction Vec3.unitY)
                (Vec3(size, size, size))
                (r * intensity)
                (g * intensity)
                (bl * intensity)
                1.0

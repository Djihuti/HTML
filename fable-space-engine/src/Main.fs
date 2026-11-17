module SpaceEngine.Main

// =====================================================================================
//  Main.fs  -  Stand Meilenstein 2: Flugtest
//
//  Das Spielerschiff fliegt mit Traegheit, Boost und Pitch/Yaw/Roll durch ein
//  Asteroidenfeld vor einem 3D-Sternenhimmel. Die Schleife trennt bereits
//    * Physik: fester Zeitschritt (120 Hz) ueber einen Akkumulator,
//    * Darstellung: variable Bildrate, interpoliert zwischen zwei Physikzustaenden.
//
//  Vorlaeufig in dieser Datei (wandert in spaeteren Meilensteinen aus):
//    * Ship + Flight  -> PhysicsAndPool.fs (Meilenstein 3, dann auch fuer Gegner)
//    * Asteroidenfeld -> PhysicsAndPool.fs (Meilenstein 3, zerstoerbar, OBB/Sphere)
//    * drawHud        -> HUD.fs (Meilenstein 4)
// =====================================================================================

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser.Dom

// -------------------------------------------------------------------------------------
//  Flugmodell
// -------------------------------------------------------------------------------------

type Ship =
    { mutable Position: Vec3
      mutable Rotation: Quat
      /// Zustand vor dem letzten Physikschritt (fuer die Render-Interpolation).
      mutable PrevPosition: Vec3
      mutable PrevRotation: Quat
      mutable Velocity: Vec3
      /// Winkelgeschwindigkeit im lokalen System (rad/s).
      mutable PitchRate: float
      mutable YawRate: float
      mutable RollRate: float
      /// Schubhebel 0..1.
      mutable Throttle: float
      /// Geglaetteter Boost 0..1.
      mutable Boost: float
      mutable Braking: bool }

module Flight =

    /// Endgeschwindigkeit bei vollem Schub (Einheiten/s).
    [<Literal>]
    let CruiseSpeed = 75.0

    /// Zusaetzliche Endgeschwindigkeit bei vollem Boost.
    [<Literal>]
    let BoostSpeed = 120.0

    /// Daempfung (1/s): bestimmt, wie lange das Schiff nach einer Kursaenderung
    /// noch in die alte Richtung driftet (~1/Drag Sekunden).
    [<Literal>]
    let Drag = 1.1

    [<Literal>]
    let BrakeDrag = 3.0

    [<Literal>]
    let MaxPitchRate = 1.5

    [<Literal>]
    let MaxYawRate = 0.85

    [<Literal>]
    let MaxRollRate = 2.6

    /// Wie schnell die Drehraten dem Steuerbefehl folgen (1/s) - die Drehtraegheit.
    [<Literal>]
    let AngularResponse = 6.0

    [<Literal>]
    let BoostResponse = 4.0

    /// Aenderung des Schubhebels pro Sekunde bei gehaltener Taste.
    [<Literal>]
    let ThrottleRate = 0.7

    let create () : Ship =
        { Position = Vec3.zero
          Rotation = Quat.identity
          PrevPosition = Vec3.zero
          PrevRotation = Quat.identity
          Velocity = Vec3(0.0, 0.0, -30.0)
          PitchRate = 0.0
          YawRate = 0.0
          RollRate = 0.0
          Throttle = 0.6
          Boost = 0.0
          Braking = false }

    /// Ein fester Physikschritt.
    let step (ship: Ship) (input: InputState) (dt: float) =
        ship.PrevPosition <- ship.Position
        ship.PrevRotation <- ship.Rotation

        // Schub, Boost, Bremse
        let throttle = ship.Throttle + Input.throttleChange input * ThrottleRate * dt
        ship.Throttle <- System.Math.Min(1.0, System.Math.Max(0.0, throttle))
        let boostTarget = if Input.isDown input GameAction.Boost then 1.0 else 0.0
        ship.Boost <- ship.Boost + (boostTarget - ship.Boost) * (1.0 - exp (-BoostResponse * dt))
        ship.Braking <- Input.isDown input GameAction.Brake

        // Drehung: Die Drehraten laufen dem Steuerbefehl traege nach und werden
        // dann im LOKALEN System per Quaternion integriert (kein Gimbal Lock).
        let blend = 1.0 - exp (-AngularResponse * dt)
        ship.PitchRate <- ship.PitchRate + (Input.pitch input * MaxPitchRate - ship.PitchRate) * blend
        ship.YawRate <- ship.YawRate + (Input.yaw input * MaxYawRate - ship.YawRate) * blend
        ship.RollRate <- ship.RollRate + (Input.roll input * MaxRollRate - ship.RollRate) * blend
        ship.Rotation <- Quat.integrateLocal ship.Rotation ship.PitchRate ship.YawRate ship.RollRate dt

        // Translation: Schub entlang der Nase, isotrope Daempfung. Der Schub ist so
        // gewaehlt, dass die Endgeschwindigkeit genau targetSpeed betraegt; weil die
        // Daempfung den alten Geschwindigkeitsvektor nur allmaehlich abbaut, driftet
        // das Schiff in Kurven sichtbar nach.
        let targetSpeed =
            if ship.Braking then
                0.0
            else
                ship.Throttle * CruiseSpeed + ship.Boost * BoostSpeed

        let drag = if ship.Braking then BrakeDrag else Drag
        let forward = Quat.forward ship.Rotation
        let accelerated = Vec3.mulAdd ship.Velocity forward (targetSpeed * drag * dt)
        ship.Velocity <- accelerated * exp (-drag * dt)
        ship.Position <- Vec3.mulAdd ship.Position ship.Velocity dt

// -------------------------------------------------------------------------------------
//  Szene
// -------------------------------------------------------------------------------------

[<Literal>]
let private PhysicsStep = 0.008333333333333333 // 1/120 s

/// Obergrenze an Physikschritten pro Frame (verhindert die "Todesspirale",
/// wenn ein Frame laenger dauert als die Simulation aufholen kann).
[<Literal>]
let private MaxStepsPerFrame = 8

[<Literal>]
let private AsteroidCount = 420

/// Pro Asteroid: [x, y, z, Radius, Achse x, Achse y, Achse z, Drehrate, Phase, r, g, b]
[<Literal>]
let private AsteroidStride = 12

[<Literal>]
let private AsteroidVariants = 4

[<Literal>]
let private DustCount = 420

/// Kantenlaenge des Wuerfels um die Kamera, in dem der Raumstaub lebt.
[<Literal>]
let private DustCell = 160.0

/// Pro Staubkorn: [x, y, z, Groesse]
[<Literal>]
let private DustStride = 4

[<Literal>]
let private StarCount = 2600

/// Kollisionsradius des Schiffs (bewusst etwas kleiner als das Modell).
[<Literal>]
let private ShipCollisionRadius = 1.6

type private Scene =
    { Renderer: Renderer
      Hud: CanvasRenderingContext2D
      HudCanvas: HTMLCanvasElement
      Input: InputState
      Camera: Camera
      Ship: Ship
      ShipBatch: InstanceBatch
      AsteroidBatches: InstanceBatch[]
      SkyBatch: InstanceBatch
      SunBatch: InstanceBatch
      DustBatch: InstanceBatch
      ExhaustBatch: InstanceBatch
      Asteroids: JS.Float32Array
      Dust: JS.Float32Array
      ShipScale: Vec3
      mutable Time: float
      mutable LastTimestamp: float
      mutable Accumulator: float
      // --- HUD: Texte werden nur 8x pro Sekunde neu erzeugt, gezeichnet wird jeder Frame
      mutable FrameCounter: int
      mutable StatTimer: float
      mutable AimX: float
      mutable AimY: float
      mutable AimVisible: bool
      mutable SpeedText: string
      mutable StatsText: string
      mutable DetailText: string }

let private createAsteroids () : JS.Float32Array =
    let rng = Rng(20261117)
    let data = JS.Constructors.Float32Array.Create(AsteroidCount * AsteroidStride)

    for i in 0 .. AsteroidCount - 1 do
        let o = i * AsteroidStride

        // Die ersten 90 bilden ein dichtes, gut durchfliegbares Feld vor dem
        // Startpunkt, der Rest einen flachen Guertel um den Ursprung.
        let nearField = i < 90

        // Groesse: viele kleine Brocken, wenige Riesen (Riesen nur im Guertel)
        let radius =
            if nearField then
                2.0 + 13.0 * System.Math.Pow(rng.NextFloat(), 2.0)
            elif rng.NextFloat() < 0.04 then
                rng.Range(50.0, 90.0)
            else
                2.0 + 30.0 * System.Math.Pow(rng.NextFloat(), 2.5)

        let mutable position =
            if nearField then
                Vec3(0.0, 0.0, -240.0) + rng.UnitVector() * rng.Range(50.0, 300.0)
            else
                let angle = rng.Range(0.0, 2.0 * System.Math.PI)
                let distance = 60.0 + 890.0 * sqrt (rng.NextFloat())
                let height = (rng.NextFloat() + rng.NextFloat() - 1.0) * 130.0
                Vec3(cos angle * distance, height, sin angle * distance)

        // Startbereich des Spielers freihalten
        let clearance = 45.0 + radius

        if Vec3.length position < clearance then
            position <- Vec3.normalize (position + Vec3(0.01, 0.0, 0.0)) * (clearance + 10.0)

        let axis = rng.UnitVector()
        let shade = rng.Range(0.36, 0.62)
        let bluish = rng.NextFloat() < 0.3
        data.[o] <- float32 position.X
        data.[o + 1] <- float32 position.Y
        data.[o + 2] <- float32 position.Z
        data.[o + 3] <- float32 radius
        data.[o + 4] <- float32 axis.X
        data.[o + 5] <- float32 axis.Y
        data.[o + 6] <- float32 axis.Z
        // Grosse Brocken drehen sich langsamer
        data.[o + 7] <- float32 (rng.Range(0.05, 0.5) * 6.0 / (4.0 + radius))
        data.[o + 8] <- float32 (rng.Range(0.0, 2.0 * System.Math.PI))
        data.[o + 9] <- float32 (if bluish then shade * 0.88 else shade)
        data.[o + 10] <- float32 (if bluish then shade * 0.94 else shade * 0.93)
        data.[o + 11] <- float32 (if bluish then shade else shade * 0.84)

    data

let private createDust () : JS.Float32Array =
    let rng = Rng(4711)
    let data = JS.Constructors.Float32Array.Create(DustCount * DustStride)

    for i in 0 .. DustCount - 1 do
        let o = i * DustStride
        data.[o] <- float32 (rng.Range(0.0, DustCell))
        data.[o + 1] <- float32 (rng.Range(0.0, DustCell))
        data.[o + 2] <- float32 (rng.Range(0.0, DustCell))
        data.[o + 3] <- float32 (rng.Range(0.05, 0.13))

    data

let private createScene () : Scene =
    let glCanvas = unbox<HTMLCanvasElement> (document.getElementById "gl-canvas")
    let hudCanvas = unbox<HTMLCanvasElement> (document.getElementById "hud-canvas")
    let renderer = Renderer.create glCanvas
    let camera = Camera.create ()
    camera.CockpitOffset <- ShipHardpoints.playerCockpit

    // Sonne: Richtung ZUR Sonne; dieselbe Richtung bekommt die sichtbare Sonnenscheibe
    let sunDirection = Vec3.normalize (Vec3(0.78, 0.42, 0.28))
    Lights.setSun renderer.Lights sunDirection.X sunDirection.Y sunDirection.Z 1.0 0.95 0.86
    Lights.setAmbient renderer.Lights 0.06 0.07 0.10

    // Meshes
    let shipMesh = Renderer.createMesh renderer (ProceduralMeshes.playerShip ())
    let boltMesh = Renderer.createMesh renderer (ProceduralMeshes.laserBolt ())
    let starMesh = Renderer.createMesh renderer (ProceduralMeshes.glowSprite 8 3)
    let glowMesh = Renderer.createMesh renderer (ProceduralMeshes.glowSprite 24 6)

    let asteroidMeshes =
        [| Renderer.createMesh renderer (ProceduralMeshes.asteroid 11 2)
           Renderer.createMesh renderer (ProceduralMeshes.asteroid 23 2)
           Renderer.createMesh renderer (ProceduralMeshes.asteroid 37 1)
           Renderer.createMesh renderer (ProceduralMeshes.asteroid 51 1) |]

    // Batches (ein Draw-Call pro Batch)
    let shipBatch = Renderer.createBatch renderer shipMesh 1 Opaque
    shipBatch.Shininess <- 70.0
    shipBatch.Specular <- 0.6

    let asteroidBatches =
        asteroidMeshes
        |> Array.map (fun mesh ->
            let batch = Renderer.createBatch renderer mesh AsteroidCount Opaque
            batch.Shininess <- 12.0
            batch.Specular <- 0.06
            batch)

    // Himmel und Sonne werden einmal gefuellt und danach nie wieder hochgeladen.
    let skyBatch = Renderer.createBatch renderer starMesh StarCount Additive
    skyBatch.CameraRelative <- true
    ProceduralMeshes.fillStarfield skyBatch 1977 (camera.Far * 0.85)
    let sunBatch = Renderer.createBatch renderer glowMesh 3 Additive
    sunBatch.CameraRelative <- true
    ProceduralMeshes.fillSun sunBatch (camera.Far * 0.85) sunDirection

    let dustBatch = Renderer.createBatch renderer boltMesh DustCount Additive
    let exhaustBatch = Renderer.createBatch renderer boltMesh 4 Additive

    let ship = Flight.create ()
    Camera.snap camera ship.Position ship.Rotation

    { Renderer = renderer
      Hud = hudCanvas.getContext_2d ()
      HudCanvas = hudCanvas
      Input = Input.attach glCanvas
      Camera = camera
      Ship = ship
      ShipBatch = shipBatch
      AsteroidBatches = asteroidBatches
      SkyBatch = skyBatch
      SunBatch = sunBatch
      DustBatch = dustBatch
      ExhaustBatch = exhaustBatch
      Asteroids = createAsteroids ()
      Dust = createDust ()
      ShipScale = Vec3.one
      Time = 0.0
      LastTimestamp = -1.0
      Accumulator = 0.0
      FrameCounter = 0
      StatTimer = 0.0
      AimX = 0.0
      AimY = 0.0
      AimVisible = false
      SpeedText = "0"
      StatsText = ""
      DetailText = "" }

// -------------------------------------------------------------------------------------
//  Physik
// -------------------------------------------------------------------------------------

/// Vorlaeufige Kollision Schiff <-> Asteroid: Kugel gegen Kugel, das Schiff wird
/// herausgeschoben und prallt gedaempft ab. Rein skalar; nur bei einem Treffer
/// entstehen zwei neue Vektoren.
let private collideWithAsteroids (scene: Scene) =
    let ship = scene.Ship
    let a = scene.Asteroids
    let mutable px = ship.Position.X
    let mutable py = ship.Position.Y
    let mutable pz = ship.Position.Z
    let mutable vx = ship.Velocity.X
    let mutable vy = ship.Velocity.Y
    let mutable vz = ship.Velocity.Z
    let mutable hit = false

    for i in 0 .. AsteroidCount - 1 do
        let o = i * AsteroidStride
        let dx = px - float a.[o]
        let dy = py - float a.[o + 1]
        let dz = pz - float a.[o + 2]
        let minDistance = float a.[o + 3] * 0.9 + ShipCollisionRadius
        let distanceSq = dx * dx + dy * dy + dz * dz

        if distanceSq < minDistance * minDistance && distanceSq > 1e-9 then
            let distance = sqrt distanceSq
            let nx = dx / distance
            let ny = dy / distance
            let nz = dz / distance
            // Aus dem Asteroiden herausschieben
            px <- float a.[o] + nx * minDistance
            py <- float a.[o + 1] + ny * minDistance
            pz <- float a.[o + 2] + nz * minDistance
            // Anteil der Geschwindigkeit in den Asteroiden hinein umkehren (gedaempft)
            let vn = vx * nx + vy * ny + vz * nz

            if vn < 0.0 then
                vx <- (vx - nx * vn * 1.4) * 0.85
                vy <- (vy - ny * vn * 1.4) * 0.85
                vz <- (vz - nz * vn * 1.4) * 0.85

            hit <- true

    if hit then
        ship.Position <- Vec3(px, py, pz)
        ship.Velocity <- Vec3(vx, vy, vz)

// -------------------------------------------------------------------------------------
//  Darstellung
// -------------------------------------------------------------------------------------

/// Schreibt alle Asteroiden in ihre Batches (Rotation aus Achse + Zeit).
let private pushAsteroids (scene: Scene) =
    let a = scene.Asteroids
    let time = scene.Time

    for v in 0 .. AsteroidVariants - 1 do
        InstanceBatch.clear scene.AsteroidBatches.[v]

    for i in 0 .. AsteroidCount - 1 do
        let o = i * AsteroidStride
        let radius = float a.[o + 3]
        let half = (float a.[o + 8] + time * float a.[o + 7]) * 0.5
        let s = sin half

        InstanceBatch.pushTRS
            scene.AsteroidBatches.[i % AsteroidVariants]
            (float a.[o])
            (float a.[o + 1])
            (float a.[o + 2])
            (float a.[o + 4] * s)
            (float a.[o + 5] * s)
            (float a.[o + 6] * s)
            (cos half)
            radius
            radius
            radius
            (float a.[o + 9])
            (float a.[o + 10])
            (float a.[o + 11])
            0.0

/// Raumstaub: Die Koerner liegen in einem Wuerfel, der sich periodisch wiederholt
/// und immer um die Kamera zentriert ist. Bei Fahrt werden sie entlang der
/// Flugrichtung zu Strichen gestreckt - der wichtigste Geschwindigkeitseindruck.
let private pushDust (scene: Scene) =
    let d = scene.Dust
    let camera = scene.Camera.Position
    let velocity = scene.Ship.Velocity
    let speed = Vec3.length velocity
    let half = DustCell * 0.5
    let mutable qx = 0.0
    let mutable qy = 0.0
    let mutable qz = 0.0
    let mutable qw = 1.0

    if speed > 1.0 then
        let q = Quat.lookRotation velocity Vec3.unitY
        qx <- q.X
        qy <- q.Y
        qz <- q.Z
        qw <- q.W

    let stretch = speed * 0.035
    InstanceBatch.clear scene.DustBatch

    for i in 0 .. DustCount - 1 do
        let o = i * DustStride
        // Position relativ zur Kamera, periodisch in [-half, half)
        let mutable dx = float d.[o] - camera.X
        let mutable dy = float d.[o + 1] - camera.Y
        let mutable dz = float d.[o + 2] - camera.Z
        dx <- dx - floor (dx / DustCell) * DustCell - half
        dy <- dy - floor (dy / DustCell) * DustCell - half
        dz <- dz - floor (dz / DustCell) * DustCell - half
        // Zum Rand hin ausblenden, damit das Umspringen unsichtbar bleibt
        let fade = 1.0 - (dx * dx + dy * dy + dz * dz) / (half * half)

        if fade > 0.0 then
            let size = float d.[o + 3]
            let brightness = 0.55 * fade

            InstanceBatch.pushTRS
                scene.DustBatch
                (camera.X + dx)
                (camera.Y + dy)
                (camera.Z + dz)
                qx
                qy
                qz
                qw
                size
                size
                (size + stretch)
                (0.62 * brightness)
                (0.74 * brightness)
                brightness
                1.0

/// Abgasflammen: pro Triebwerk ein breiter blauer und ein schmaler heisser Kern.
let private pushExhaust (scene: Scene) (position: Vec3) (rotation: Quat) =
    let ship = scene.Ship
    let thrust = if ship.Braking then 0.0 else ship.Throttle
    // Leichtes Flackern
    let flicker = 0.92 + 0.08 * sin (scene.Time * 71.0)
    let length = (0.35 + 1.5 * thrust + 3.4 * ship.Boost) * flicker
    let glow = 0.35 + 0.65 * thrust + 0.9 * ship.Boost
    InstanceBatch.clear scene.ExhaustBatch

    for side in 0..1 do
        let nozzle =
            if side = 0 then
                ShipHardpoints.playerEngineLeft
            else
                ShipHardpoints.playerEngineRight

        // Die Flamme beginnt an der Duese und reicht `length` nach hinten.
        let outer = position + Quat.rotateVec3 rotation (Vec3(nozzle.X, nozzle.Y, nozzle.Z + length * 0.5))

        InstanceBatch.pushTRS
            scene.ExhaustBatch
            outer.X
            outer.Y
            outer.Z
            rotation.X
            rotation.Y
            rotation.Z
            rotation.W
            0.34
            0.34
            length
            (0.10 * glow)
            (0.36 * glow)
            (0.95 * glow)
            1.0

        let coreLength = length * 0.6
        let core = position + Quat.rotateVec3 rotation (Vec3(nozzle.X, nozzle.Y, nozzle.Z + coreLength * 0.5))

        InstanceBatch.pushTRS
            scene.ExhaustBatch
            core.X
            core.Y
            core.Z
            rotation.X
            rotation.Y
            rotation.Z
            rotation.W
            0.17
            0.17
            coreLength
            (0.75 * glow)
            (0.9 * glow)
            glow
            1.0

/// Vorlaeufiges HUD (wird in Meilenstein 4 durch HUD.fs ersetzt).
let private drawHud (scene: Scene) =
    let renderer = scene.Renderer
    let ctx = scene.Hud
    let ship = scene.Ship
    let input = scene.Input
    let ratio = renderer.PixelRatio
    let w = float renderer.Width / ratio
    let h = float renderer.Height / ratio
    ctx.setTransform (ratio, 0.0, 0.0, ratio, 0.0, 0.0)
    ctx.clearRect (0.0, 0.0, w, h)

    // Fadenkreuz am Zielpunkt des Schiffs
    if scene.AimVisible then
        let x = scene.AimX
        let y = scene.AimY
        ctx.strokeStyle <- U3.Case1 "rgba(120, 230, 255, 0.9)"
        ctx.lineWidth <- 1.5
        ctx.beginPath ()
        ctx.arc (x, y, 13.0, 0.0, 2.0 * System.Math.PI)
        ctx.moveTo (x - 24.0, y)
        ctx.lineTo (x - 8.0, y)
        ctx.moveTo (x + 8.0, y)
        ctx.lineTo (x + 24.0, y)
        ctx.moveTo (x, y - 24.0)
        ctx.lineTo (x, y - 8.0)
        ctx.moveTo (x, y + 8.0)
        ctx.lineTo (x, y + 24.0)
        ctx.stroke ()

    // Virtueller Joystick der Maus
    if input.PointerLocked then
        let cx = w * 0.5
        let cy = h * 0.5
        ctx.strokeStyle <- U3.Case1 "rgba(120, 230, 255, 0.22)"
        ctx.lineWidth <- 1.0
        ctx.beginPath ()
        ctx.arc (cx, cy, 70.0, 0.0, 2.0 * System.Math.PI)
        ctx.stroke ()
        ctx.fillStyle <- U3.Case1 "rgba(255, 190, 90, 0.9)"
        ctx.beginPath ()
        ctx.arc (cx + input.StickX * 70.0, cy + input.StickY * 70.0, 3.5, 0.0, 2.0 * System.Math.PI)
        ctx.fill ()

    // Statuszeilen oben links
    ctx.textBaseline <- "top"
    ctx.textAlign <- "left"
    ctx.font <- "600 13px ui-monospace, Consolas, monospace"
    ctx.fillStyle <- U3.Case1 "rgba(120, 220, 255, 0.95)"
    ctx.fillText ("MEILENSTEIN 2  ::  FLUGTEST", 20.0, 18.0)
    ctx.font <- "12px ui-monospace, Consolas, monospace"
    ctx.fillStyle <- U3.Case1 "rgba(200, 215, 235, 0.8)"
    ctx.fillText (scene.StatsText, 20.0, 42.0)
    ctx.fillText (scene.DetailText, 20.0, 60.0)

    // Geschwindigkeit, Schub, Boost unten links
    let baseY = h - 96.0
    ctx.fillStyle <- U3.Case1 "rgba(200, 215, 235, 0.8)"
    ctx.fillText ("TEMPO", 20.0, baseY)
    ctx.fillText ("SCHUB", 20.0, baseY + 34.0)
    ctx.fillText ("BOOST", 20.0, baseY + 56.0)
    ctx.font <- "600 22px ui-monospace, Consolas, monospace"
    ctx.fillStyle <- U3.Case1 "rgba(120, 230, 255, 0.95)"
    ctx.fillText (scene.SpeedText, 76.0, baseY - 7.0)
    ctx.fillStyle <- U3.Case1 "rgba(120, 230, 255, 0.16)"
    ctx.fillRect (76.0, baseY + 35.0, 170.0, 9.0)
    ctx.fillRect (76.0, baseY + 57.0, 170.0, 9.0)
    ctx.fillStyle <- U3.Case1 (if ship.Braking then "rgba(255, 110, 90, 0.9)" else "rgba(120, 230, 255, 0.9)")
    ctx.fillRect (76.0, baseY + 35.0, 170.0 * ship.Throttle, 9.0)
    ctx.fillStyle <- U3.Case1 "rgba(255, 190, 90, 0.95)"
    ctx.fillRect (76.0, baseY + 57.0, 170.0 * ship.Boost, 9.0)

    // Steuerungshilfe unten mittig
    ctx.font <- "12px ui-monospace, Consolas, monospace"
    ctx.textAlign <- "center"
    ctx.fillStyle <- U3.Case1 "rgba(200, 215, 235, 0.6)"
    ctx.fillText ("W/S Pitch   A/D Roll   Q/E Yaw   Shift Boost", w * 0.5, h - 64.0)
    ctx.fillText ("Pfeile hoch/runter Schub   X Bremse   C Kamera", w * 0.5, h - 44.0)

    ctx.fillText (
        (if input.PointerLocked then
             "Maus steuert Pitch/Yaw   -   Esc gibt die Maus frei"
         else
             "Klick ins Bild: Maussteuerung"),
        w * 0.5,
        h - 24.0
    )

let private render (scene: Scene) (position: Vec3) (rotation: Quat) =
    let renderer = scene.Renderer
    let camera = scene.Camera
    let ship = scene.Ship

    if Renderer.resize renderer then
        // HUD-Canvas deckungsgleich mit dem WebGL-Canvas halten
        scene.HudCanvas.width <- float renderer.Width
        scene.HudCanvas.height <- float renderer.Height

    Camera.updateMatrices camera (Renderer.aspect renderer)

    // Triebwerkslicht: beleuchtet Heck und nahe Asteroiden
    let thrust = if ship.Braking then 0.0 else ship.Throttle
    let engine = position + Quat.rotateVec3 rotation (Vec3(0.0, 0.1, 2.6))
    Lights.clearPoints renderer.Lights
    Lights.addPoint renderer.Lights engine.X engine.Y engine.Z 16.0 0.35 0.65 1.0 (0.3 + 0.9 * thrust + 2.4 * ship.Boost)

    // Instanzen schreiben
    InstanceBatch.clear scene.ShipBatch
    InstanceBatch.push scene.ShipBatch position rotation scene.ShipScale 1.0 1.0 1.0 0.0
    pushAsteroids scene
    pushDust scene
    pushExhaust scene position rotation

    // Zeichnen: erst deckend, dann additiv
    Renderer.beginFrame renderer camera.View camera.Projection camera.Position

    for v in 0 .. AsteroidVariants - 1 do
        Renderer.draw renderer scene.AsteroidBatches.[v]

    Renderer.draw renderer scene.ShipBatch
    Renderer.draw renderer scene.SkyBatch
    Renderer.draw renderer scene.SunBatch
    Renderer.draw renderer scene.DustBatch
    Renderer.draw renderer scene.ExhaustBatch

    // Zielpunkt des Schiffs auf den Bildschirm projizieren (3D -> 2D)
    let aim = Vec3.mulAdd position (Quat.forward rotation) 800.0

    if Mat4.clipW renderer.ViewProj aim > 0.0 then
        let ndc = Mat4.transformPoint renderer.ViewProj aim
        scene.AimX <- (ndc.X * 0.5 + 0.5) * float renderer.Width / renderer.PixelRatio
        scene.AimY <- (0.5 - ndc.Y * 0.5) * float renderer.Height / renderer.PixelRatio
        scene.AimVisible <- true
    else
        scene.AimVisible <- false

    drawHud scene

// -------------------------------------------------------------------------------------
//  Schleife
// -------------------------------------------------------------------------------------

let private scene: Scene option =
    try
        Some(createScene ())
    with ex ->
        // Klartext statt schwarzem Bildschirm, falls WebGL2 fehlt oder ein Shader bricht.
        let box = document.getElementById "error"
        box.textContent <- ex.Message
        box.removeAttribute "hidden"
        console.error ex
        None

#if DEBUG
// Nur im Watch-Modus (npm run dev): Szene fuer Tests in der Browser-Konsole.
window?spaceEngine <- scene
#endif

/// Haelt den requestAnimationFrame-Callback als einmal erzeugtes Funktionsobjekt.
/// Wuerde `frame` direkt uebergeben, erzeugte Fable in jedem Frame eine neue Closure.
type private Loop =
    { mutable Callback: float -> unit }

let private loop: Loop = { Callback = ignore }

let private updateHudTexts (scene: Scene) (dt: float) =
    scene.FrameCounter <- scene.FrameCounter + 1
    scene.StatTimer <- scene.StatTimer + dt

    if scene.StatTimer >= 0.125 then
        let renderer = scene.Renderer
        let fps = int (float scene.FrameCounter / scene.StatTimer + 0.5)
        scene.FrameCounter <- 0
        scene.StatTimer <- 0.0
        scene.SpeedText <- string (int (Vec3.length scene.Ship.Velocity + 0.5))

        scene.StatsText <-
            "FPS "
            + string fps
            + "   Draw-Calls "
            + string renderer.DrawCalls
            + "   Instanzen "
            + string renderer.InstancesDrawn

        scene.DetailText <-
            "Kamera "
            + (match scene.Camera.Mode with
               | Chase -> "Verfolger"
               | Cockpit -> "Cockpit")
            + (if scene.Input.InvertPitch then "   Pitch invertiert" else "")

let private frame (timestamp: float) =
    match scene with
    | Some scene ->
        let input = scene.Input
        let ship = scene.Ship

        // dt deckeln, damit ein Tab-Wechsel keinen Zeitsprung erzeugt
        let dt =
            if scene.LastTimestamp < 0.0 then
                0.0
            else
                System.Math.Min(0.1, (timestamp - scene.LastTimestamp) / 1000.0)

        scene.LastTimestamp <- timestamp
        scene.Time <- scene.Time + dt

        // Einmal-Aktionen (Flanken) gehoeren zum Frame, nicht zum Physikschritt
        if Input.wasPressed input GameAction.ToggleCamera then
            Camera.toggleMode scene.Camera

        if Input.wasPressed input GameAction.InvertPitch then
            input.InvertPitch <- not input.InvertPitch

        // Physik mit festem Zeitschritt
        scene.Accumulator <- scene.Accumulator + dt
        let mutable steps = 0

        while scene.Accumulator >= PhysicsStep && steps < MaxStepsPerFrame do
            Flight.step ship input PhysicsStep
            collideWithAsteroids scene
            scene.Accumulator <- scene.Accumulator - PhysicsStep
            steps <- steps + 1

        if steps = MaxStepsPerFrame then
            scene.Accumulator <- 0.0

        // Darstellung zwischen letztem und aktuellem Physikzustand interpolieren
        let alpha = scene.Accumulator / PhysicsStep
        let position = Vec3.lerp ship.PrevPosition ship.Position alpha
        let rotation = Quat.slerp ship.PrevRotation ship.Rotation alpha

        Camera.update scene.Camera position rotation ship.Boost dt
        updateHudTexts scene dt
        render scene position rotation
        Input.endFrame input
        window.requestAnimationFrame loop.Callback |> ignore
    | None -> ()

loop.Callback <- frame
window.requestAnimationFrame loop.Callback |> ignore

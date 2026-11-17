namespace SpaceEngine

// =====================================================================================
//  Camera.fs  -  Verfolgerkamera (Third Person) und Cockpit-Perspektive
//
//  Chase-Modus: Die Kamera folgt der Orientierung des Schiffs mit Verzoegerung
//  (Quat.slerp). Weil das Schiff sofort, die Kamera aber weich dreht, neigt und
//  rollt sich das Schiff sichtbar VOR der Kamera. Der Abstandsvektor wird
//  zusaetzlich per Vec3.lerp geglaettet; beim Boost faellt die Kamera zurueck
//  und der Oeffnungswinkel weitet sich.
//
//  Cockpit-Modus: starr mit dem Schiff verbunden.
// =====================================================================================

type CameraMode =
    | Chase
    | Cockpit

type Camera =
    { mutable Mode: CameraMode
      mutable Position: Vec3
      mutable Rotation: Quat
      /// Geglaetteter Abstand Kamera - Ziel in Weltkoordinaten.
      mutable Offset: Vec3
      /// Aktueller vertikaler Oeffnungswinkel (Radiant).
      mutable Fov: float
      View: Mat4
      Projection: Mat4
      // --- Abstimmung ---
      /// Kameraposition relativ zum Schiff im Chase-Modus (lokal: hinten oben).
      mutable ChaseOffset: Vec3
      /// Kameraposition relativ zum Schiff im Cockpit-Modus.
      mutable CockpitOffset: Vec3
      /// Wie schnell die Kamera der Schiffsdrehung folgt (1/s; kleiner = traeger).
      mutable RotationStiffness: float
      /// Wie schnell der Abstandsvektor nachgefuehrt wird (1/s).
      mutable PositionStiffness: float
      mutable BaseFov: float
      /// Zusaetzlicher Oeffnungswinkel bei vollem Boost.
      mutable BoostFov: float
      /// Relative Abstandsvergroesserung bei vollem Boost.
      mutable BoostPullback: float
      mutable Near: float
      mutable Far: float }

[<RequireQualifiedAccess>]
module Camera =

    let create () : Camera =
        let degrees = System.Math.PI / 180.0

        { Mode = Chase
          Position = Vec3.zero
          Rotation = Quat.identity
          Offset = Vec3.zero
          Fov = 60.0 * degrees
          View = Mat4.create ()
          Projection = Mat4.create ()
          ChaseOffset = Vec3(0.0, 2.1, 9.5)
          CockpitOffset = Vec3(0.0, 0.52, -0.55)
          RotationStiffness = 7.0
          PositionStiffness = 10.0
          BaseFov = 60.0 * degrees
          BoostFov = 14.0 * degrees
          BoostPullback = 0.35
          Near = 0.3
          Far = 6000.0 }

    /// Wechselt zwischen Verfolger- und Cockpit-Perspektive. Der Uebergang ist
    /// weich, weil Offset und Rotation von ihrem aktuellen Wert aus weiterlaufen.
    let toggleMode (camera: Camera) =
        camera.Mode <-
            match camera.Mode with
            | Chase -> Cockpit
            | Cockpit -> Chase

    /// Setzt die Kamera ohne Glaettung hinter das Ziel (Start, Respawn).
    let snap (camera: Camera) (targetPosition: Vec3) (targetRotation: Quat) =
        let local =
            match camera.Mode with
            | Chase -> camera.ChaseOffset
            | Cockpit -> camera.CockpitOffset

        camera.Rotation <- targetRotation
        camera.Offset <- Quat.rotateVec3 targetRotation local
        camera.Position <- targetPosition + camera.Offset
        camera.Fov <- camera.BaseFov

    /// Fuehrt die Kamera dem Ziel nach. `boost` in 0..1 steuert Rueckfall und FOV.
    /// Die Glaettungsfaktoren 1 - exp(-k * dt) sind bildratenunabhaengig.
    let update (camera: Camera) (targetPosition: Vec3) (targetRotation: Quat) (boost: float) (dt: float) =
        let fovTarget = camera.BaseFov + camera.BoostFov * boost
        camera.Fov <- camera.Fov + (fovTarget - camera.Fov) * (1.0 - exp (-5.0 * dt))

        match camera.Mode with
        | Chase ->
            camera.Rotation <-
                Quat.slerp camera.Rotation targetRotation (1.0 - exp (-camera.RotationStiffness * dt))

            let pullback = 1.0 + camera.BoostPullback * boost
            let desired = Quat.rotateVec3 camera.Rotation (camera.ChaseOffset * pullback)
            camera.Offset <- Vec3.lerp camera.Offset desired (1.0 - exp (-camera.PositionStiffness * dt))
            camera.Position <- targetPosition + camera.Offset
        | Cockpit ->
            // Rotation starr, der Offset gleitet beim Umschalten weich ins Cockpit.
            camera.Rotation <- targetRotation
            let desired = Quat.rotateVec3 targetRotation camera.CockpitOffset
            let distanceSq = Vec3.distanceSq camera.Offset desired

            camera.Offset <-
                if distanceSq < 1e-4 then
                    desired
                else
                    Vec3.lerp camera.Offset desired (1.0 - exp (-18.0 * dt))

            camera.Position <- targetPosition + camera.Offset

    /// Schreibt View- und Projektionsmatrix (einmal pro gerendertem Frame).
    let updateMatrices (camera: Camera) (aspect: float) =
        Mat4.viewFromRotationTranslation camera.View camera.Rotation camera.Position
        Mat4.perspective camera.Projection camera.Fov aspect camera.Near camera.Far

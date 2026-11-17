namespace SpaceEngine

// =====================================================================================
//  Input.fs  -  nicht-blockierender Eingabezustand
//
//  Die Browser-Events schreiben nur in vorab angelegte Typed Arrays und Felder;
//  die Spielschleife fragt den Zustand ab, wann immer sie will (Polling). Es gibt
//  keine Event-Queue und damit keine Allokation pro Tastendruck oder Frame.
//
//  Tastatur:  W/S Pitch, A/D Roll, Q/E Yaw, Shift Boost, Leertaste Feuer,
//             Pfeil hoch/runter (oder R/F) Schub, X Bremse, C Kamera,
//             I Pitch invertieren, Enter Bestaetigen, P Pause
//  Maus:      Klick ins Bild aktiviert PointerLock. Die Maus bewegt dann einen
//             virtuellen Joystick (Yaw/Pitch), linke Taste feuert, Esc gibt frei.
// =====================================================================================

open Fable.Core
open Browser.Types
open Browser.Dom

/// Logische Aktionen. Als F#-Enum sind das im JavaScript blanke Zahlen und
/// damit direkt als Array-Index nutzbar.
type GameAction =
    | PitchDown = 0
    | PitchUp = 1
    | RollLeft = 2
    | RollRight = 3
    | YawLeft = 4
    | YawRight = 5
    | Boost = 6
    | Fire = 7
    | ThrottleUp = 8
    | ThrottleDown = 9
    | Brake = 10
    | ToggleCamera = 11
    | InvertPitch = 12
    | Confirm = 13
    | Pause = 14

type InputState =
    { /// Pro Aktion: 1 = Taste wird gerade gehalten.
      Down: JS.Uint8Array
      /// Pro Aktion: 1 = Taste wurde seit dem letzten endFrame neu gedrueckt.
      Pressed: JS.Uint8Array
      /// Virtueller Joystick der Maus, jeweils -1..1 (X rechts, Y unten).
      mutable StickX: float
      mutable StickY: float
      /// Linke Maustaste gehalten (nur bei aktivem PointerLock).
      mutable MouseFire: bool
      mutable PointerLocked: bool
      /// Stick-Ausschlag pro Pixel Mausbewegung.
      mutable MouseSensitivity: float
      /// false: W = Nase runter, S = Nase hoch (wie ein Steuerknueppel).
      mutable InvertPitch: bool }

[<RequireQualifiedAccess>]
module Input =

    [<Literal>]
    let private ActionCount = 15

    /// Ausschlaege unterhalb dieser Schwelle gelten als Ruhestellung.
    [<Literal>]
    let private StickDeadzone = 0.06

    /// Ordnet physische Tasten (KeyboardEvent.code, layoutunabhaengig) Aktionen zu.
    /// Liefert -1 fuer nicht belegte Tasten.
    let private actionOfCode (code: string) : int =
        match code with
        | "KeyW" -> int GameAction.PitchDown
        | "KeyS" -> int GameAction.PitchUp
        | "KeyA" -> int GameAction.RollLeft
        | "KeyD" -> int GameAction.RollRight
        | "KeyQ" -> int GameAction.YawLeft
        | "KeyE" -> int GameAction.YawRight
        | "ShiftLeft"
        | "ShiftRight" -> int GameAction.Boost
        | "Space" -> int GameAction.Fire
        | "ArrowUp"
        | "KeyR" -> int GameAction.ThrottleUp
        | "ArrowDown"
        | "KeyF" -> int GameAction.ThrottleDown
        | "KeyX" -> int GameAction.Brake
        | "KeyC" -> int GameAction.ToggleCamera
        | "KeyI" -> int GameAction.InvertPitch
        | "Enter" -> int GameAction.Confirm
        | "KeyP" -> int GameAction.Pause
        | _ -> -1

    let private releaseAll (state: InputState) =
        for i in 0 .. ActionCount - 1 do
            state.Down.[i] <- 0uy

        state.MouseFire <- false
        state.StickX <- 0.0
        state.StickY <- 0.0

    /// Legt den Zustand an und registriert alle Listener (einmalig beim Start).
    let attach (canvas: HTMLCanvasElement) : InputState =
        let state =
            { Down = JS.Constructors.Uint8Array.Create ActionCount
              Pressed = JS.Constructors.Uint8Array.Create ActionCount
              StickX = 0.0
              StickY = 0.0
              MouseFire = false
              PointerLocked = false
              MouseSensitivity = 0.0035
              InvertPitch = false }

        window.addEventListener (
            "keydown",
            fun (e: Event) ->
                let key = unbox<KeyboardEvent> e
                let action = actionOfCode key.code

                // Browser-Kuerzel (Strg+R, Alt+Tab ...) nicht abfangen
                if action >= 0 && not key.ctrlKey && not key.altKey && not key.metaKey then
                    // Auto-Repeat des Betriebssystems erzeugt kein neues "Pressed"
                    if state.Down.[action] = 0uy then
                        state.Pressed.[action] <- 1uy

                    state.Down.[action] <- 1uy
                    e.preventDefault ()
        )

        window.addEventListener (
            "keyup",
            fun (e: Event) ->
                let action = actionOfCode (unbox<KeyboardEvent> e).code

                if action >= 0 then
                    state.Down.[action] <- 0uy
        )

        // Verliert das Fenster den Fokus, kaeme das keyup nie an - alles loslassen.
        window.addEventListener ("blur", (fun _ -> releaseAll state))

        canvas.addEventListener (
            "mousedown",
            fun (e: Event) ->
                let mouse = unbox<MouseEvent> e

                if not state.PointerLocked then
                    canvas.requestPointerLock ()
                elif mouse.button = 0.0 then
                    state.MouseFire <- true
        )

        window.addEventListener (
            "mouseup",
            fun (e: Event) ->
                if (unbox<MouseEvent> e).button = 0.0 then
                    state.MouseFire <- false
        )

        document.addEventListener (
            "pointerlockchange",
            fun _ ->
                state.PointerLocked <- obj.ReferenceEquals(document.pointerLockElement, canvas)

                if not state.PointerLocked then
                    state.MouseFire <- false
                    state.StickX <- 0.0
                    state.StickY <- 0.0
        )

        document.addEventListener (
            "mousemove",
            fun (e: Event) ->
                if state.PointerLocked then
                    let mouse = unbox<MouseEvent> e
                    let mutable x = state.StickX + mouse.movementX * state.MouseSensitivity
                    let mutable y = state.StickY + mouse.movementY * state.MouseSensitivity
                    // Auf den Einheitskreis begrenzen
                    let lengthSq = x * x + y * y

                    if lengthSq > 1.0 then
                        let inv = 1.0 / sqrt lengthSq
                        x <- x * inv
                        y <- y * inv

                    state.StickX <- x
                    state.StickY <- y
        )

        state

    /// Wird die Aktion gerade gehalten?
    let inline isDown (state: InputState) (action: GameAction) : bool = state.Down.[int action] <> 0uy

    /// Wurde die Aktion in diesem Frame neu ausgeloest (Flanke)?
    let inline wasPressed (state: InputState) (action: GameAction) : bool = state.Pressed.[int action] <> 0uy

    /// Am Ende jedes Frames aufrufen: setzt die Flanken zurueck.
    let endFrame (state: InputState) =
        for i in 0 .. ActionCount - 1 do
            state.Pressed.[i] <- 0uy

    let inline private held (state: InputState) (action: GameAction) : float = float state.Down.[int action]

    let private clampUnit (v: float) : float =
        if v > 1.0 then 1.0
        elif v < -1.0 then -1.0
        else v

    let private applyDeadzone (v: float) : float =
        if abs v < StickDeadzone then 0.0 else v

    /// Pitch-Befehl in -1..1, positiv = Nase hoch.
    let pitch (state: InputState) : float =
        let keys = held state GameAction.PitchUp - held state GameAction.PitchDown
        let keys = if state.InvertPitch then -keys else keys
        // Maus nach vorn (StickY negativ) hebt die Nase
        clampUnit (keys - applyDeadzone state.StickY)

    /// Yaw-Befehl in -1..1, positiv = Nase nach links.
    let yaw (state: InputState) : float =
        clampUnit (
            held state GameAction.YawLeft
            - held state GameAction.YawRight
            - applyDeadzone state.StickX
        )

    /// Roll-Befehl in -1..1, positiv = Rolle nach links.
    let roll (state: InputState) : float =
        held state GameAction.RollLeft - held state GameAction.RollRight

    /// Schubaenderung in -1..1 (Pfeiltasten bzw. R/F).
    let throttleChange (state: InputState) : float =
        held state GameAction.ThrottleUp - held state GameAction.ThrottleDown

    /// Feuerbefehl aus Tastatur oder Maus.
    let fire (state: InputState) : bool =
        state.Down.[int GameAction.Fire] <> 0uy || state.MouseFire

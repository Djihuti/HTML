namespace SpaceEngine

// =====================================================================================
//  WebGLRenderer.fs  -  nativer WebGL2-Renderer ohne externe 3D-Bibliothek
//
//  Pipeline in einem Satz: ALLES wird instanziert gezeichnet. Ein InstanceBatch
//  verbindet ein Mesh mit einem Instanz-Buffer (Modellmatrix + Farbe pro Instanz)
//  und kostet genau einen Draw-Call - egal ob 1 Spielerschiff oder 2000 Partikel.
//
//  Vertex-Layout (pro Vertex, 10 Floats):  location 0 = Position, 1 = Normale,
//                                           location 2 = vec4 (RGB, Emissive 0..1)
//  Instanz-Layout (pro Instanz, 20 Floats): location 3..6 = mat4 Modellmatrix,
//                                           location 7 = vec4 (RGB, Emissive 0..1)
//
//  Endfarbe = Vertexfarbe * Instanzfarbe; Emissive = Maximum beider Werte. So kann
//  ein Mesh eigene Farbzonen tragen (Cockpit, Triebwerksgluehen) und trotzdem pro
//  Instanz eingefaerbt werden.
//
//  Beleuchtung: Blinn-Phong mit einem Directional Light (Sonne), Ambient und bis
//  zu 8 Point Lights (Triebwerke, Explosionen). Emissive = 1 schaltet die
//  Beleuchtung fuer die Instanz ab (Laser, Sterne, Partikel).
//
//  Der Frame-Pfad (beginFrame / InstanceBatch.push* / draw) allokiert nichts:
//  alle Puffer und Uniform-Arrays werden beim Setup angelegt.
// =====================================================================================

open Fable.Core
open Browser.Types
open Browser.Dom

// -------------------------------------------------------------------------------------
//  WebGL2-Bindings. Fable.Browser.WebGL (1.3.0) deckt nur WebGL1 ab; die WebGL2-
//  Erweiterungen, die die Engine braucht (VAOs, Instancing), werden hier ergaenzt.
// -------------------------------------------------------------------------------------

/// Opaques Handle eines Vertex Array Objects.
type WebGLVertexArrayObject =
    interface
    end

type WebGL2RenderingContext =
    inherit WebGLRenderingContext
    abstract createVertexArray: unit -> WebGLVertexArrayObject
    abstract bindVertexArray: vertexArray: WebGLVertexArrayObject -> unit
    abstract deleteVertexArray: vertexArray: WebGLVertexArrayObject -> unit
    abstract vertexAttribDivisor: index: float * divisor: float -> unit
    abstract drawArraysInstanced: mode: float * first: float * count: float * instanceCount: float -> unit

    abstract drawElementsInstanced:
        mode: float * count: float * ``type``: float * offset: float * instanceCount: float -> unit

    /// WebGL2-Ueberladung von bufferSubData: laedt nur `length` Elemente ab `srcOffset`
    /// hoch, ohne dass dafuer eine subarray()-View allokiert werden muss.
    [<Emit("$0.bufferSubData($1, $2, $3, $4, $5)")>]
    abstract bufferSubDataRange:
        target: float * dstByteOffset: float * srcData: JS.Float32Array * srcOffset: float * length: float -> unit

// -------------------------------------------------------------------------------------
//  Datentypen
// -------------------------------------------------------------------------------------

/// Feste Groessen der Vertex- und Instanzformate.
module Layout =
    /// Floats pro Vertex: Position (3) + Normale (3) + Farbe/Emissive (4).
    [<Literal>]
    let VertexFloats = 10

    /// Offset der Farbe innerhalb eines Vertex.
    [<Literal>]
    let VertexColorOffset = 6

    /// Floats pro Instanz: Modellmatrix (16) + Farbe/Emissive (4).
    [<Literal>]
    let InstanceFloats = 20

    /// Offset der Farbe innerhalb einer Instanz.
    [<Literal>]
    let InstanceColorOffset = 16

    /// Obergrenze der gleichzeitig aktiven Point Lights (muss zum Shader passen).
    [<Literal>]
    let MaxPointLights = 8

/// Geometrie auf der CPU-Seite: verschachtelte Vertices [px py pz nx ny nz r g b e]
/// und 16-Bit-Indizes (max. 65 536 Vertices pro Mesh - reicht fuer Low-Poly).
type MeshData =
    { Vertices: JS.Float32Array
      Indices: JS.Uint16Array }

/// Geometrie auf der GPU.
type Mesh =
    { VertexBuffer: WebGLBuffer
      IndexBuffer: WebGLBuffer
      IndexCount: int }

/// Wie ein Batch mit dem Framebuffer verrechnet wird.
type BlendMode =
    /// Deckend, schreibt in den Tiefenpuffer.
    | Opaque
    /// Additiv (Leuchten: Laser, Partikel, Sterne), schreibt nicht in den Tiefenpuffer.
    | Additive

/// Ein Mesh plus Instanz-Buffer = ein Draw-Call.
/// Pro Frame: clear -> push* -> Renderer.draw.
type InstanceBatch =
    { Mesh: Mesh
      Vao: WebGLVertexArrayObject
      InstanceBuffer: WebGLBuffer
      /// CPU-Spiegel des Instanz-Buffers (Capacity * Layout.InstanceFloats).
      Data: JS.Float32Array
      Capacity: int
      Blend: BlendMode
      /// Anzahl der in diesem Frame geschriebenen Instanzen.
      mutable Count: int
      /// true, sobald sich Instanzdaten geaendert haben. Statische Batches (Sterne)
      /// werden so nur ein einziges Mal zur GPU hochgeladen.
      mutable NeedsUpload: bool
      /// true = Positionen sind relativ zur Kamera (Himmel): der Batch wird ohne
      /// Kamera-Translation gezeichnet und wirkt dadurch unendlich weit entfernt.
      mutable CameraRelative: bool
      /// Blinn-Phong-Exponent (groesser = kleineres, schaerferes Glanzlicht).
      mutable Shininess: float
      /// Staerke des Glanzlichts (0 = matt).
      mutable Specular: float }

/// Lichtzustand der Szene; wird in beginFrame als Uniforms hochgeladen.
type Lights =
    { /// Normalisierte Richtung ZUR Sonne (Weltkoordinaten).
      SunDirection: JS.Float32Array
      SunColor: JS.Float32Array
      Ambient: JS.Float32Array
      /// Pro Licht: xyz = Position, w = Reichweite.
      PointPosRange: JS.Float32Array
      /// Pro Licht: rgb = Farbe * Intensitaet, w ungenutzt.
      PointColor: JS.Float32Array
      mutable PointCount: int }

type Renderer =
    { Canvas: HTMLCanvasElement
      Gl: WebGL2RenderingContext
      Program: WebGLProgram
      UViewProj: WebGLUniformLocation
      UCameraPos: WebGLUniformLocation
      USunDir: WebGLUniformLocation
      USunColor: WebGLUniformLocation
      UAmbient: WebGLUniformLocation
      UPointCount: WebGLUniformLocation
      UPointPosRange: WebGLUniformLocation
      UPointColor: WebGLUniformLocation
      UShininess: WebGLUniformLocation
      USpecular: WebGLUniformLocation
      Lights: Lights
      /// Scratch: Projektion * View des aktuellen Frames (auch vom HUD nutzbar).
      ViewProj: Mat4
      /// Scratch: View-Matrix ohne Translation und die zugehoerige Projektion * View
      /// fuer kamerarelative Batches.
      SkyView: Mat4
      SkyViewProj: Mat4
      /// Scratch: Kameraposition als Uniform-Quelle.
      CameraPos: JS.Float32Array
      /// Hintergrundfarbe (RGB).
      ClearColor: JS.Float32Array
      /// Groesse des Drawing Buffers in physischen Pixeln.
      mutable Width: int
      mutable Height: int
      /// Verhaeltnis physische Pixel / CSS-Pixel des aktuellen Frames.
      mutable PixelRatio: float
      /// Aktiver Blend-Zustand: 0 = Opaque, 1 = Additive (vermeidet redundante GL-Calls).
      mutable BlendState: int
      /// Aktive uViewProj-Matrix: 0 = Welt, 1 = kamerarelativ.
      mutable ViewProjState: int
      /// Statistik des aktuellen Frames.
      mutable DrawCalls: int
      mutable InstancesDrawn: int }

// -------------------------------------------------------------------------------------
//  Shader (GLSL ES 3.00)
// -------------------------------------------------------------------------------------

module Shaders =

    let vertex =
        """#version 300 es
precision highp float;

layout(location = 0) in vec3 aPosition;
layout(location = 1) in vec3 aNormal;
layout(location = 2) in vec4 aColor;   // rgb = Vertexfarbe, a = Emissive
layout(location = 3) in mat4 iModel;   // belegt location 3..6
layout(location = 7) in vec4 iColor;   // rgb = Instanzfarbe, a = Emissive

uniform mat4 uViewProj;

out vec3 vWorldPos;
out vec3 vNormal;
out vec4 vColor;

void main() {
    vec4 world = iModel * vec4(aPosition, 1.0);
    vWorldPos = world.xyz;

    // Normalenmatrix fuer M = R * S (auch nicht-uniforme Skalierung):
    // (M^-1)^T = R * S^-1 = M * S^-2. Die quadrierten Skalierungen sind die
    // quadrierten Spaltenlaengen von M - das spart ein inverse() pro Vertex.
    mat3 m = mat3(iModel);
    vec3 scaleSq = vec3(dot(m[0], m[0]), dot(m[1], m[1]), dot(m[2], m[2]));
    vNormal = m * (aNormal / scaleSq);

    vColor = vec4(aColor.rgb * iColor.rgb, max(aColor.a, iColor.a));
    gl_Position = uViewProj * world;
}
"""

    let fragment =
        """#version 300 es
precision highp float;

#define MAX_POINT_LIGHTS 8

in vec3 vWorldPos;
in vec3 vNormal;
in vec4 vColor;

uniform vec3 uCameraPos;
uniform vec3 uSunDir;      // normalisiert, zeigt ZUR Sonne
uniform vec3 uSunColor;
uniform vec3 uAmbient;
uniform int uPointCount;
uniform vec4 uPointPosRange[MAX_POINT_LIGHTS]; // xyz = Position, w = Reichweite
uniform vec4 uPointColor[MAX_POINT_LIGHTS];    // rgb = Farbe * Intensitaet
uniform float uShininess;
uniform float uSpecular;

out vec4 outColor;

void main() {
    vec3 albedo = vColor.rgb;
    vec3 N = normalize(vNormal);
    vec3 V = normalize(uCameraPos - vWorldPos);

    vec3 diffuse = uAmbient;
    vec3 specular = vec3(0.0);

    // Directional Light (Sonne), Blinn-Phong
    float sunNdotL = max(dot(N, uSunDir), 0.0);
    if (sunNdotL > 0.0) {
        vec3 H = normalize(uSunDir + V);
        diffuse += uSunColor * sunNdotL;
        specular += uSunColor * pow(max(dot(N, H), 0.0), uShininess);
    }

    // Point Lights mit weichem, endlichem Abfall
    for (int i = 0; i < MAX_POINT_LIGHTS; i++) {
        if (i >= uPointCount) break;
        vec3 toLight = uPointPosRange[i].xyz - vWorldPos;
        float dist = length(toLight);
        float range = uPointPosRange[i].w;
        if (dist < range) {
            vec3 L = toLight / max(dist, 1e-4);
            float falloff = 1.0 - dist / range;
            falloff *= falloff;
            float ndotl = max(dot(N, L), 0.0);
            if (ndotl > 0.0) {
                vec3 H = normalize(L + V);
                vec3 radiance = uPointColor[i].rgb * falloff;
                diffuse += radiance * ndotl;
                specular += radiance * pow(max(dot(N, H), 0.0), uShininess);
            }
        }
    }

    vec3 lit = albedo * diffuse + specular * uSpecular;

    // Emissive blendet von "beleuchtet" zu "selbstleuchtend".
    vec3 color = mix(lit, albedo, vColor.a);

    // Lineare Beleuchtung -> sRGB-naehernde Gammakorrektur
    outColor = vec4(pow(max(color, 0.0), vec3(1.0 / 2.2)), 1.0);
}
"""

// -------------------------------------------------------------------------------------
//  Lichter
// -------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module Lights =

    let create () : Lights =
        let f32 (n: int) = JS.Constructors.Float32Array.Create n
        let sunDir = f32 3
        sunDir.[1] <- 1.0f
        let sunColor = f32 3
        sunColor.[0] <- 1.0f
        sunColor.[1] <- 1.0f
        sunColor.[2] <- 1.0f

        { SunDirection = sunDir
          SunColor = sunColor
          Ambient = f32 3
          PointPosRange = f32 (Layout.MaxPointLights * 4)
          PointColor = f32 (Layout.MaxPointLights * 4)
          PointCount = 0 }

    /// Richtung ZUR Sonne (wird normalisiert) und ihre Farbe/Intensitaet.
    let setSun (l: Lights) (dx: float) (dy: float) (dz: float) (r: float) (g: float) (b: float) =
        let len = sqrt (dx * dx + dy * dy + dz * dz)
        let inv = if len > 1e-12 then 1.0 / len else 0.0
        l.SunDirection.[0] <- float32 (dx * inv)
        l.SunDirection.[1] <- float32 (dy * inv)
        l.SunDirection.[2] <- float32 (dz * inv)
        l.SunColor.[0] <- float32 r
        l.SunColor.[1] <- float32 g
        l.SunColor.[2] <- float32 b

    let setAmbient (l: Lights) (r: float) (g: float) (b: float) =
        l.Ambient.[0] <- float32 r
        l.Ambient.[1] <- float32 g
        l.Ambient.[2] <- float32 b

    /// Entfernt alle Point Lights (einmal pro Frame vor addPoint aufrufen).
    let inline clearPoints (l: Lights) = l.PointCount <- 0

    /// Fuegt ein Point Light hinzu. Ist das Limit erreicht, wird es ignoriert.
    let addPoint
        (l: Lights)
        (x: float)
        (y: float)
        (z: float)
        (range: float)
        (r: float)
        (g: float)
        (b: float)
        (intensity: float)
        =
        if l.PointCount < Layout.MaxPointLights then
            let o = l.PointCount * 4
            l.PointPosRange.[o] <- float32 x
            l.PointPosRange.[o + 1] <- float32 y
            l.PointPosRange.[o + 2] <- float32 z
            l.PointPosRange.[o + 3] <- float32 range
            l.PointColor.[o] <- float32 (r * intensity)
            l.PointColor.[o + 1] <- float32 (g * intensity)
            l.PointColor.[o + 2] <- float32 (b * intensity)
            l.PointColor.[o + 3] <- 0.0f
            l.PointCount <- l.PointCount + 1

// -------------------------------------------------------------------------------------
//  Instanz-Batches
// -------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module InstanceBatch =

    /// Verwirft alle Instanzen des letzten Frames (Speicher bleibt erhalten).
    let inline clear (batch: InstanceBatch) =
        batch.Count <- 0
        batch.NeedsUpload <- true

    /// Schreibt eine Instanz aus reinen Skalaren - der allokationsfreie Pfad fuer
    /// Pools und Schleifen. Ist der Batch voll, wird die Instanz verworfen.
    let pushTRS
        (batch: InstanceBatch)
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
        (r: float)
        (g: float)
        (b: float)
        (emissive: float)
        =
        if batch.Count < batch.Capacity then
            let o = batch.Count * Layout.InstanceFloats
            let d = batch.Data
            Mat4.composeTRS d o px py pz qx qy qz qw sx sy sz
            d.[o + 16] <- float32 r
            d.[o + 17] <- float32 g
            d.[o + 18] <- float32 b
            d.[o + 19] <- float32 emissive
            batch.Count <- batch.Count + 1
            batch.NeedsUpload <- true

    /// Schreibt eine Instanz aus Vec3/Quat (bequem fuer Einzelobjekte wie Schiffe).
    let push
        (batch: InstanceBatch)
        (position: Vec3)
        (rotation: Quat)
        (scale: Vec3)
        (r: float)
        (g: float)
        (b: float)
        (emissive: float)
        =
        pushTRS
            batch
            position.X
            position.Y
            position.Z
            rotation.X
            rotation.Y
            rotation.Z
            rotation.W
            scale.X
            scale.Y
            scale.Z
            r
            g
            b
            emissive

    /// Schreibt eine Instanz mit fertiger Modellmatrix.
    let pushMatrix (batch: InstanceBatch) (model: Mat4) (r: float) (g: float) (b: float) (emissive: float) =
        if batch.Count < batch.Capacity then
            let o = batch.Count * Layout.InstanceFloats
            let d = batch.Data
            let m = model.Data

            for i in 0..15 do
                d.[o + i] <- m.[i]

            d.[o + 16] <- float32 r
            d.[o + 17] <- float32 g
            d.[o + 18] <- float32 b
            d.[o + 19] <- float32 emissive
            batch.Count <- batch.Count + 1
            batch.NeedsUpload <- true

// -------------------------------------------------------------------------------------
//  Renderer
// -------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module Renderer =

    let private compileShader (gl: WebGL2RenderingContext) (kind: float) (label: string) (source: string) : WebGLShader =
        let shader = gl.createShader kind
        gl.shaderSource (shader, source)
        gl.compileShader shader

        if not (unbox<bool> (gl.getShaderParameter (shader, gl.COMPILE_STATUS))) then
            let log = gl.getShaderInfoLog shader
            gl.deleteShader shader
            failwith ("Shader-Kompilierung fehlgeschlagen (" + label + "):\n" + log)

        shader

    let private linkProgram (gl: WebGL2RenderingContext) (vertexSource: string) (fragmentSource: string) : WebGLProgram =
        let vs = compileShader gl gl.VERTEX_SHADER "Vertex" vertexSource
        let fs = compileShader gl gl.FRAGMENT_SHADER "Fragment" fragmentSource
        let program = gl.createProgram ()
        gl.attachShader (program, vs)
        gl.attachShader (program, fs)
        gl.linkProgram program
        // Nach dem Linken werden die Shader-Objekte nicht mehr gebraucht.
        gl.deleteShader vs
        gl.deleteShader fs

        if not (unbox<bool> (gl.getProgramParameter (program, gl.LINK_STATUS))) then
            let log = gl.getProgramInfoLog program
            gl.deleteProgram program
            failwith ("Shader-Programm konnte nicht gelinkt werden:\n" + log)

        program

    /// Initialisiert WebGL2 auf dem Canvas, kompiliert die Shader und setzt den
    /// festen GL-Zustand. Wirft eine Exception mit Klartext, wenn WebGL2 fehlt.
    let create (canvas: HTMLCanvasElement) : Renderer =
        let attributes =
            JsInterop.createObj
                [ "antialias", box true
                  "alpha", box false
                  "depth", box true
                  "stencil", box false
                  "powerPreference", box "high-performance" ]

        let context = canvas.getContext ("webgl2", attributes)

        if isNull context then
            failwith "WebGL2 wird von diesem Browser bzw. dieser Grafikkarte nicht unterstuetzt."

        let gl = unbox<WebGL2RenderingContext> context
        let program = linkProgram gl Shaders.vertex Shaders.fragment
        let uniform (name: string) = gl.getUniformLocation (program, name)

        gl.enable gl.DEPTH_TEST
        gl.depthFunc gl.LEQUAL
        gl.enable gl.CULL_FACE
        gl.cullFace gl.BACK
        gl.frontFace gl.CCW
        gl.disable gl.BLEND

        let clearColor = JS.Constructors.Float32Array.Create 3
        clearColor.[0] <- 0.004f
        clearColor.[1] <- 0.006f
        clearColor.[2] <- 0.016f

        { Canvas = canvas
          Gl = gl
          Program = program
          UViewProj = uniform "uViewProj"
          UCameraPos = uniform "uCameraPos"
          USunDir = uniform "uSunDir"
          USunColor = uniform "uSunColor"
          UAmbient = uniform "uAmbient"
          UPointCount = uniform "uPointCount"
          UPointPosRange = uniform "uPointPosRange[0]"
          UPointColor = uniform "uPointColor[0]"
          UShininess = uniform "uShininess"
          USpecular = uniform "uSpecular"
          Lights = Lights.create ()
          ViewProj = Mat4.create ()
          SkyView = Mat4.create ()
          SkyViewProj = Mat4.create ()
          CameraPos = JS.Constructors.Float32Array.Create 3
          ClearColor = clearColor
          Width = 0
          Height = 0
          PixelRatio = 1.0
          BlendState = 0
          ViewProjState = 0
          DrawCalls = 0
          InstancesDrawn = 0 }

    /// Laedt Geometrie in statische GPU-Buffer. Nur beim Setup aufrufen.
    let createMesh (renderer: Renderer) (data: MeshData) : Mesh =
        let gl = renderer.Gl
        // Kein VAO gebunden lassen, damit der ELEMENT_ARRAY_BUFFER-Bind unten
        // nicht versehentlich den Zustand eines fremden VAOs veraendert.
        gl.bindVertexArray (unbox null)

        let vbo = gl.createBuffer ()
        gl.bindBuffer (gl.ARRAY_BUFFER, vbo)
        gl.bufferData (gl.ARRAY_BUFFER, U3.Case2(unbox<JS.ArrayBufferView> data.Vertices), gl.STATIC_DRAW)

        let ibo = gl.createBuffer ()
        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, ibo)
        gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, U3.Case2(unbox<JS.ArrayBufferView> data.Indices), gl.STATIC_DRAW)

        { VertexBuffer = vbo
          IndexBuffer = ibo
          IndexCount = data.Indices.length }

    /// Legt einen Batch fuer bis zu `capacity` Instanzen eines Meshes an
    /// (VAO + dynamischer Instanz-Buffer). Nur beim Setup aufrufen.
    let createBatch (renderer: Renderer) (mesh: Mesh) (capacity: int) (blend: BlendMode) : InstanceBatch =
        let gl = renderer.Gl
        let floatBytes = 4
        let vertexStride = float (Layout.VertexFloats * floatBytes)
        let instanceStride = float (Layout.InstanceFloats * floatBytes)

        let vao = gl.createVertexArray ()
        gl.bindVertexArray vao

        // Pro-Vertex-Attribute aus dem Mesh
        gl.bindBuffer (gl.ARRAY_BUFFER, mesh.VertexBuffer)
        gl.enableVertexAttribArray 0.0
        gl.vertexAttribPointer (0.0, 3.0, gl.FLOAT, false, vertexStride, 0.0)
        gl.enableVertexAttribArray 1.0
        gl.vertexAttribPointer (1.0, 3.0, gl.FLOAT, false, vertexStride, float (3 * floatBytes))
        gl.enableVertexAttribArray 2.0

        gl.vertexAttribPointer (
            2.0,
            4.0,
            gl.FLOAT,
            false,
            vertexStride,
            float (Layout.VertexColorOffset * floatBytes)
        )
        // Der Index-Buffer gehoert zum VAO-Zustand.
        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, mesh.IndexBuffer)

        // Pro-Instanz-Attribute: eine mat4 belegt vier aufeinanderfolgende Locations.
        let instanceBuffer = gl.createBuffer ()
        gl.bindBuffer (gl.ARRAY_BUFFER, instanceBuffer)
        gl.bufferData (gl.ARRAY_BUFFER, U3.Case1(float capacity * instanceStride), gl.DYNAMIC_DRAW)

        for column in 0..3 do
            let location = float (3 + column)
            gl.enableVertexAttribArray location
            gl.vertexAttribPointer (location, 4.0, gl.FLOAT, false, instanceStride, float (column * 4 * floatBytes))
            gl.vertexAttribDivisor (location, 1.0)

        gl.enableVertexAttribArray 7.0

        gl.vertexAttribPointer (
            7.0,
            4.0,
            gl.FLOAT,
            false,
            instanceStride,
            float (Layout.InstanceColorOffset * floatBytes)
        )

        gl.vertexAttribDivisor (7.0, 1.0)

        gl.bindVertexArray (unbox null)
        gl.bindBuffer (gl.ARRAY_BUFFER, unbox null)

        { Mesh = mesh
          Vao = vao
          InstanceBuffer = instanceBuffer
          Data = JS.Constructors.Float32Array.Create(capacity * Layout.InstanceFloats)
          Capacity = capacity
          Blend = blend
          Count = 0
          NeedsUpload = true
          CameraRelative = false
          Shininess = 48.0
          Specular = 0.5 }

    /// Gleicht den Drawing Buffer an die CSS-Groesse des Canvas an (inkl.
    /// devicePixelRatio, gedeckelt auf 2 gegen unnoetige Fuellrate auf 4K-Displays).
    /// Liefert true, wenn sich die Groesse geaendert hat. Pro Frame aufrufbar.
    let resize (renderer: Renderer) : bool =
        let ratio = System.Math.Min(System.Math.Max(window.devicePixelRatio, 1.0), 2.0)
        let width = int (System.Math.Max(1.0, floor (renderer.Canvas.clientWidth * ratio)))
        let height = int (System.Math.Max(1.0, floor (renderer.Canvas.clientHeight * ratio)))
        renderer.PixelRatio <- ratio

        if width <> renderer.Width || height <> renderer.Height then
            renderer.Canvas.width <- float width
            renderer.Canvas.height <- float height
            renderer.Width <- width
            renderer.Height <- height
            true
        else
            false

    /// Seitenverhaeltnis des Drawing Buffers.
    let inline aspect (renderer: Renderer) =
        float renderer.Width / float renderer.Height

    /// Beginnt einen Frame: Viewport, Clear, Kamera- und Licht-Uniforms.
    let beginFrame (renderer: Renderer) (view: Mat4) (projection: Mat4) (cameraPosition: Vec3) =
        let gl = renderer.Gl
        let lights = renderer.Lights
        renderer.DrawCalls <- 0
        renderer.InstancesDrawn <- 0

        // Deckenden Zustand herstellen - depthMask muss vor dem Clear aktiv sein,
        // sonst wird der Tiefenpuffer nicht geloescht.
        gl.disable gl.BLEND
        gl.depthMask true
        renderer.BlendState <- 0

        gl.viewport (0.0, 0.0, float renderer.Width, float renderer.Height)

        gl.clearColor (
            float renderer.ClearColor.[0],
            float renderer.ClearColor.[1],
            float renderer.ClearColor.[2],
            1.0
        )

        gl.clear (gl.COLOR_BUFFER_BIT + gl.DEPTH_BUFFER_BIT)

        Mat4.multiply renderer.ViewProj projection view
        Vec3.writeTo renderer.CameraPos 0 cameraPosition

        // Kamerarelative Variante: dieselbe Blickrichtung, aber ohne Translation.
        Mat4.copy renderer.SkyView view
        let skyView = renderer.SkyView.Data
        skyView.[12] <- 0.0f
        skyView.[13] <- 0.0f
        skyView.[14] <- 0.0f
        Mat4.multiply renderer.SkyViewProj projection renderer.SkyView

        gl.useProgram renderer.Program
        gl.uniformMatrix4fv (renderer.UViewProj, false, renderer.ViewProj.Data)
        renderer.ViewProjState <- 0
        gl.uniform3fv (renderer.UCameraPos, renderer.CameraPos)
        gl.uniform3fv (renderer.USunDir, lights.SunDirection)
        gl.uniform3fv (renderer.USunColor, lights.SunColor)
        gl.uniform3fv (renderer.UAmbient, lights.Ambient)
        gl.uniform1i (renderer.UPointCount, float lights.PointCount)
        gl.uniform4fv (renderer.UPointPosRange, lights.PointPosRange)
        gl.uniform4fv (renderer.UPointColor, lights.PointColor)

    /// Zeichnet alle Instanzen eines Batches in EINEM Draw-Call.
    /// Reihenfolge im Frame: erst alle Opaque-Batches, dann die Additive-Batches.
    let draw (renderer: Renderer) (batch: InstanceBatch) =
        if batch.Count > 0 then
            let gl = renderer.Gl

            match batch.Blend with
            | Opaque ->
                if renderer.BlendState <> 0 then
                    gl.disable gl.BLEND
                    gl.depthMask true
                    renderer.BlendState <- 0
            | Additive ->
                if renderer.BlendState <> 1 then
                    gl.enable gl.BLEND
                    gl.blendFunc (gl.ONE, gl.ONE)
                    gl.depthMask false
                    renderer.BlendState <- 1

            if batch.CameraRelative then
                if renderer.ViewProjState <> 1 then
                    gl.uniformMatrix4fv (renderer.UViewProj, false, renderer.SkyViewProj.Data)
                    renderer.ViewProjState <- 1
            elif renderer.ViewProjState <> 0 then
                gl.uniformMatrix4fv (renderer.UViewProj, false, renderer.ViewProj.Data)
                renderer.ViewProjState <- 0

            gl.uniform1f (renderer.UShininess, batch.Shininess)
            gl.uniform1f (renderer.USpecular, batch.Specular)

            gl.bindVertexArray batch.Vao

            if batch.NeedsUpload then
                gl.bindBuffer (gl.ARRAY_BUFFER, batch.InstanceBuffer)

                // Nur den tatsaechlich benutzten Teil des Instanz-Buffers hochladen.
                gl.bufferSubDataRange (
                    gl.ARRAY_BUFFER,
                    0.0,
                    batch.Data,
                    0.0,
                    float (batch.Count * Layout.InstanceFloats)
                )

                batch.NeedsUpload <- false

            gl.drawElementsInstanced (
                gl.TRIANGLES,
                float batch.Mesh.IndexCount,
                gl.UNSIGNED_SHORT,
                0.0,
                float batch.Count
            )

            renderer.DrawCalls <- renderer.DrawCalls + 1
            renderer.InstancesDrawn <- renderer.InstancesDrawn + batch.Count

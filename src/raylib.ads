with Ada.Streams;  use Ada.Streams;
with Ada.Calendar; use Ada.Calendar;
with Ada.Numerics;

with Interfaces;
with Interfaces.C;
with System;

package RayLib is
   Version : constant String := "4.0";

   Pi : constant := Ada.Numerics.Pi;

   DEG2RAD : constant := Pi / 180.0;
   RAD2DEG : constant := 180.0 / Pi;

   subtype Unsigned_8 is Interfaces.Unsigned_8;
   subtype Unsigned_16 is Interfaces.Unsigned_16;

   type Unsigned_16_Array is array (Natural range <>) of Unsigned_16;
   type Unsigned_16_Array_Access is access all Unsigned_16_Array;
   type Float_Array is array (Natural range <>) of Float;
   type Float_Array_Access is access all Float_Array;
   type Natural_Array is array (Natural range <>) of Natural;
   type Natural_Array_Access is access all Natural_Array;
   type Integer_Array is array (Natural range <>) of Integer;
   type Integer_Array_Access is access all Integer_Array;

   --  Vector2, 2 components
   type Vector2 is record
      X : Float;
      --  Vector x component
      Y : Float;
      --  Vector y component
   end record;

   type Vector2_Array is array (Natural range <>) of Vector2;
   type Vector2_Array_Access is access all Vector2_Array;

   --  Vector3, 3 components
   type Vector3 is record
      X : Float;
      --  Vector x component
      Y : Float;
      --  Vector y component
      Z : Float;
      --  Vector z component
   end record;

   type Vector3_Array is array (Natural range <>) of Vector3;
   type Vector3_Array_Access is access all Vector3_Array;

   --  Vector4, 4 components
   type Vector4 is record
      X : Float;
      --  Vector x component
      Y : Float;
      --  Vector y component
      Z : Float;
      --  Vector z component
      W : Float;
      --  Vector w component
   end record;

   --  Quaternion, 4 components (Vector4 alias)
   subtype Quaternion is RayLib.Vector4;

   type Vector4_Array is array (Natural range <>) of Vector4;
   type Vector4_Array_Access is access all Vector4_Array;

   --  Matrix, 4x4 components, column major, OpenGL style, right handed
   type Matrix is record
      M0 : Float;
      --  Matrix first row (4 components)
      M4 : Float;
      --  Matrix first row (4 components)
      M8 : Float;
      --  Matrix first row (4 components)
      M12 : Float;
      --  Matrix first row (4 components)
      M1 : Float;
      --  Matrix second row (4 components)
      M5 : Float;
      --  Matrix second row (4 components)
      M9 : Float;
      --  Matrix second row (4 components)
      M13 : Float;
      --  Matrix second row (4 components)
      M2 : Float;
      --  Matrix third row (4 components)
      M6 : Float;
      --  Matrix third row (4 components)
      M10 : Float;
      --  Matrix third row (4 components)
      M14 : Float;
      --  Matrix third row (4 components)
      M3 : Float;
      --  Matrix fourth row (4 components)
      M7 : Float;
      --  Matrix fourth row (4 components)
      M11 : Float;
      --  Matrix fourth row (4 components)
      M15 : Float;
      --  Matrix fourth row (4 components)
   end record;

   type Matrix_Array is array (Natural range <>) of Matrix;
   type Matrix_Array_Access is access all Matrix_Array;

   --  Color, 4 components, R8G8B8A8 (32bit)
   type Color is record
      R : Unsigned_8;
      --  Color red value
      G : Unsigned_8;
      --  Color green value
      B : Unsigned_8;
      --  Color blue value
      A : Unsigned_8;
      --  Color alpha value
   end record;

   type Color_Array is array (Natural range <>) of Color;
   type Color_Array_Access is access all Color_Array;

   --  Rectangle, 4 components
   type Rectangle is record
      X : Float;
      --  Rectangle top-left corner position x
      Y : Float;
      --  Rectangle top-left corner position y
      Width : Float;
      --  Rectangle width
      Height : Float;
      --  Rectangle height
   end record;

   type Rectangle_Array is array (Natural range <>) of Rectangle;
   type Rectangle_Array_Access is access all Rectangle_Array;

   --  Image, pixel data stored in CPU memory (RAM)
   type Image is record
      Data : System.Address;
      --  Image raw data
      Width : Integer;
      --  Image base width
      Height : Integer;
      --  Image base height
      Mipmaps : Integer;
      --  Mipmap levels, 1 by default
      Format : Integer;
      --  Data format (PixelFormat type)
   end record;

   type Image_Array is array (Natural range <>) of Image;
   type Image_Array_Access is access all Image_Array;

   --  Texture, tex data stored in GPU memory (VRAM)
   type Texture is record
      Id : Natural;
      --  OpenGL texture id
      Width : Integer;
      --  Texture base width
      Height : Integer;
      --  Texture base height
      Mipmaps : Integer;
      --  Mipmap levels, 1 by default
      Format : Integer;
      --  Data format (PixelFormat type)
   end record;

   --  Texture2D, same as Texture
   subtype Texture2D is RayLib.Texture;

   --  TextureCubemap, same as Texture
   subtype Texture_Cubemap is RayLib.Texture;

   type Texture_Array is array (Natural range <>) of Texture;
   type Texture_Array_Access is access all Texture_Array;

   --  RenderTexture, fbo for texture rendering
   type Render_Texture is record
      Id : Natural;
      --  OpenGL framebuffer object id
      Texture : RayLib.Texture;
      --  Color buffer attachment texture
      Depth : RayLib.Texture;
      --  Depth buffer attachment texture
   end record;

   --  RenderTexture2D, same as RenderTexture
   subtype Render_Texture2D is RayLib.Render_Texture;

   type Render_Texture_Array is array (Natural range <>) of Render_Texture;
   type Render_Texture_Array_Access is access all Render_Texture_Array;

   --  NPatchInfo, n-patch layout info
   type N_Patch_Info is record
      Source : RayLib.Rectangle;
      --  Texture source rectangle
      Left : Integer;
      --  Left border offset
      Top : Integer;
      --  Top border offset
      Right : Integer;
      --  Right border offset
      Bottom : Integer;
      --  Bottom border offset
      Layout : Integer;
      --  Layout of the n-patch: 3x3, 1x3 or 3x1
   end record;

   type N_Patch_Info_Array is array (Natural range <>) of N_Patch_Info;
   type N_Patch_Info_Array_Access is access all N_Patch_Info_Array;

   --  GlyphInfo, font characters glyphs info
   type Glyph_Info is record
      Value : Integer;
      --  Character value (Unicode)
      Offset_X : Integer;
      --  Character offset X when drawing
      Offset_Y : Integer;
      --  Character offset Y when drawing
      Advance_X : Integer;
      --  Character advance position X
      Image : RayLib.Image;
      --  Character image data
   end record;

   type Glyph_Info_Array is array (Natural range <>) of Glyph_Info;
   type Glyph_Info_Array_Access is access all Glyph_Info_Array;

   --  Font, font texture and GlyphInfo array data
   type Font is record
      Base_Size : Integer;
      --  Base size (default chars height)
      Glyph_Count : Integer;
      --  Number of glyph characters
      Glyph_Padding : Integer;
      --  Padding around the glyph characters
      Texture : RayLib.Texture2D;
      --  Texture atlas containing the glyphs
      Recs : Rectangle_Array_Access;
      --  Rectangles in texture for the glyphs
      Glyphs : Glyph_Info_Array_Access;
      --  Glyphs info data
   end record;

   type Font_Array is array (Natural range <>) of Font;
   type Font_Array_Access is access all Font_Array;

   --  Camera, defines position/orientation in 3d space
   type Camera3D is record
      Position : RayLib.Vector3;
      --  Camera position
      Target : RayLib.Vector3;
      --  Camera target it looks-at
      Up : RayLib.Vector3;
      --  Camera up vector (rotation over its axis)
      Fovy : Float;
      --  Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
      Projection : Integer;
      --  Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
   end record;

   --  Camera type fallback, defaults to Camera3D
   subtype Camera is RayLib.Camera3D;

   type Camera3D_Array is array (Natural range <>) of Camera3D;
   type Camera3D_Array_Access is access all Camera3D_Array;

   --  Camera2D, defines position/orientation in 2d space
   type Camera2D is record
      Offset : RayLib.Vector2;
      --  Camera offset (displacement from target)
      Target : RayLib.Vector2;
      --  Camera target (rotation and zoom origin)
      Rotation : Float;
      --  Camera rotation in degrees
      Zoom : Float;
      --  Camera zoom (scaling), should be 1.0f by default
   end record;

   type Camera2D_Array is array (Natural range <>) of Camera2D;
   type Camera2D_Array_Access is access all Camera2D_Array;

   --  Mesh, vertex data and vao/vbo
   type Mesh is record
      Vertex_Count : Integer;
      --  Number of vertices stored in arrays
      Triangle_Count : Integer;
      --  Number of triangles stored (indexed or not)
      Vertices : Float_Array_Access;
      --  Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
      Texcoords : Float_Array_Access;
      --  Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
      Texcoords2 : Float_Array_Access;
      --  Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
      Normals : Float_Array_Access;
      --  Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
      Tangents : Float_Array_Access;
   --  Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
      Colors : access Stream_Element_Array;
      --  Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
      Indices : Unsigned_16_Array_Access;
      --  Vertex indices (in case vertex data comes indexed)
      Anim_Vertices : Float_Array_Access;
      --  Animated vertex positions (after bones transformations)
      Anim_Normals : Float_Array_Access;
      --  Animated normals (after bones transformations)
      Bone_Ids : access Stream_Element_Array;
      --  Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex (skinning)
      Bone_Weights : Float_Array_Access;
      --  Vertex bone weight, up to 4 bones influence by vertex (skinning)
      Vao_Id : Natural;
      --  OpenGL Vertex Array Object id
      Vbo_Id : Natural_Array_Access;
      --  OpenGL Vertex Buffer Objects id (default vertex data)
   end record;

   type Mesh_Array is array (Natural range <>) of Mesh;
   type Mesh_Array_Access is access all Mesh_Array;

   --  Shader
   type Shader is record
      Id : Natural;
      --  Shader program id
      Locs : Integer_Array_Access;
      --  Shader locations array (RL_MAX_SHADER_LOCATIONS)
   end record;

   type Shader_Array is array (Natural range <>) of Shader;
   type Shader_Array_Access is access all Shader_Array;

   --  MaterialMap
   type Material_Map is record
      Texture : RayLib.Texture2D;
      --  Material map texture
      Color : RayLib.Color;
      --  Material map color
      Value : Float;
      --  Material map value
   end record;

   type Material_Map_Array is array (Natural range <>) of Material_Map;
   type Material_Map_Array_Access is access all Material_Map_Array;

   --  Material, includes shader and maps
   type Material is record
      Shader : RayLib.Shader;
      --  Material shader
      Maps : Material_Map_Array_Access;
      --  Material maps array (MAX_MATERIAL_MAPS)
      Params : Float_Array (1 .. 4);
      --  Material generic parameters (if required)
   end record;

   type Material_Array is array (Natural range <>) of Material;
   type Material_Array_Access is access all Material_Array;

   --  Transform, vectex transformation data
   type Transform is record
      Translation : RayLib.Vector3;
      --  Translation
      Rotation : RayLib.Quaternion;
      --  Rotation
      Scale : RayLib.Vector3;
      --  Scale
   end record;

   type Transform_Array is array (Natural range <>) of Transform;
   type Transform_Array_Access is access all Transform_Array;
   type Transform_Array_Array is
     array (Natural range <>, Natural range <>) of Transform;
   type Transform_Array_Array_Access is access all Transform_Array_Array;

   --  Bone, skeletal animation bone
   type Bone_Info is record
      Name : String (1 .. 32);
      --  Bone name
      Parent : Integer;
      --  Bone parent
   end record;

   type Bone_Info_Array is array (Natural range <>) of Bone_Info;
   type Bone_Info_Array_Access is access all Bone_Info_Array;

   --  Model, meshes, materials and animation data
   type Model is record
      Transform : RayLib.Matrix;
      --  Local transform matrix
      Mesh_Count : Integer;
      --  Number of meshes
      Material_Count : Integer;
      --  Number of materials
      Meshes : Mesh_Array_Access;
      --  Meshes array
      Materials : Material_Array_Access;
      --  Materials array
      Mesh_Material : Integer_Array_Access;
      --  Mesh material number
      Bone_Count : Integer;
      --  Number of bones
      Bones : Bone_Info_Array_Access;
      --  Bones information (skeleton)
      Bind_Pose : Transform_Array_Access;
      --  Bones base transformation (pose)
   end record;

   type Model_Array is array (Natural range <>) of Model;
   type Model_Array_Access is access all Model_Array;

   --  ModelAnimation
   type Model_Animation is record
      Bone_Count : Integer;
      --  Number of bones
      Frame_Count : Integer;
      --  Number of animation frames
      Bones : Bone_Info_Array_Access;
      --  Bones information (skeleton)
      Frame_Poses : Transform_Array_Array_Access;
      --  Poses array by frame
   end record;

   type Model_Animation_Array is array (Natural range <>) of Model_Animation;
   type Model_Animation_Array_Access is access all Model_Animation_Array;

   --  Ray, ray for raycasting
   type Ray is record
      Position : RayLib.Vector3;
      --  Ray position (origin)
      Direction : RayLib.Vector3;
      --  Ray direction
   end record;

   type Ray_Array is array (Natural range <>) of Ray;
   type Ray_Array_Access is access all Ray_Array;

   --  RayCollision, ray hit information
   type Ray_Collision is record
      Hit : Boolean;
      --  Did the ray hit something?
      Distance : Float;
      --  Distance to nearest hit
      Point : RayLib.Vector3;
      --  Point of nearest hit
      Normal : RayLib.Vector3;
      --  Surface normal of hit
   end record;

   type Ray_Collision_Array is array (Natural range <>) of Ray_Collision;
   type Ray_Collision_Array_Access is access all Ray_Collision_Array;

   --  BoundingBox
   type Bounding_Box is record
      Min : RayLib.Vector3;
      --  Minimum vertex box-corner
      Max : RayLib.Vector3;
      --  Maximum vertex box-corner
   end record;

   type Bounding_Box_Array is array (Natural range <>) of Bounding_Box;
   type Bounding_Box_Array_Access is access all Bounding_Box_Array;

   --  Wave, audio wave data
   type Wave is record
      Frame_Count : Natural;
      --  Total number of frames (considering channels)
      Sample_Rate : Natural;
      --  Frequency (samples per second)
      Sample_Size : Natural;
      --  Bit depth (bits per sample): 8, 16, 32 (24 not supported)
      Channels : Natural;
      --  Number of channels (1-mono, 2-stereo, ...)
      Data : System.Address;
      --  Buffer data pointer
   end record;

   type Wave_Array is array (Natural range <>) of Wave;
   type Wave_Array_Access is access all Wave_Array;

   --  AudioStream, custom audio stream
   type Audio_Stream is record
      Buffer : System.Address;
      --  Pointer to internal data used by the audio system
      Processor : System.Address;
      --  Pointer to internal data processor, useful for audio effects
      Sample_Rate : Natural;
      --  Frequency (samples per second)
      Sample_Size : Natural;
      --  Bit depth (bits per sample): 8, 16, 32 (24 not supported)
      Channels : Natural;
      --  Number of channels (1-mono, 2-stereo, ...)
   end record;

   type Audio_Stream_Array is array (Natural range <>) of Audio_Stream;
   type Audio_Stream_Array_Access is access all Audio_Stream_Array;

   --  Sound
   type Sound is record
      Stream : RayLib.Audio_Stream;
      --  Audio stream
      Frame_Count : Natural;
      --  Total number of frames (considering channels)
   end record;

   type Sound_Array is array (Natural range <>) of Sound;
   type Sound_Array_Access is access all Sound_Array;

   --  Music, audio stream, anything longer than ~10 seconds should be streamed
   type Music is record
      Stream : RayLib.Audio_Stream;
      --  Audio stream
      Frame_Count : Natural;
      --  Total number of frames (considering channels)
      Looping : Boolean;
      --  Music looping enable
      Ctx_Type : Integer;
      --  Type of music context (audio filetype)
      Ctx_Data : System.Address;
      --  Audio context data, depends on type
   end record;

   type Music_Array is array (Natural range <>) of Music;
   type Music_Array_Access is access all Music_Array;

   --  VrDeviceInfo, Head-Mounted-Display device parameters
   type VR_Device_Info is record
      H_Resolution : Integer;
      --  Horizontal resolution in pixels
      V_Resolution : Integer;
      --  Vertical resolution in pixels
      H_Screen_Size : Float;
      --  Horizontal size in meters
      V_Screen_Size : Float;
      --  Vertical size in meters
      V_Screen_Center : Float;
      --  Screen center in meters
      Eye_To_Screen_Distance : Float;
      --  Distance between eye and display in meters
      Lens_Separation_Distance : Float;
      --  Lens separation distance in meters
      Interpupillary_Distance : Float;
      --  IPD (distance between pupils) in meters
      Lens_Distortion_Values : Float_Array (1 .. 4);
      --  Lens distortion constant parameters
      Chroma_Ab_Correction : Float_Array (1 .. 4);
      --  Chromatic aberration correction parameters
   end record;

   type VR_Device_Info_Array is array (Natural range <>) of VR_Device_Info;
   type VR_Device_Info_Array_Access is access all VR_Device_Info_Array;

   --  VrStereoConfig, VR stereo rendering configuration for simulator
   type VR_Stereo_Config is record
      Projection : Matrix_Array (1 .. 2);
      --  VR projection matrices (per eye)
      View_Offset : Matrix_Array (1 .. 2);
      --  VR view offset matrices (per eye)
      Left_Lens_Center : Float_Array (1 .. 2);
      --  VR left lens center
      Right_Lens_Center : Float_Array (1 .. 2);
      --  VR right lens center
      Left_Screen_Center : Float_Array (1 .. 2);
      --  VR left screen center
      Right_Screen_Center : Float_Array (1 .. 2);
      --  VR right screen center
      Scale : Float_Array (1 .. 2);
      --  VR distortion scale
      Scale_In : Float_Array (1 .. 2);
      --  VR distortion scale in
   end record;

   type VR_Stereo_Config_Array is array (Natural range <>) of VR_Stereo_Config;
   type VR_Stereo_Config_Array_Access is access all VR_Stereo_Config_Array;

   Light_Gray  : constant Color := (200, 200, 200, 255);
   Gray        : constant Color := (130, 130, 130, 255);
   Dark_Gray   : constant Color := (80, 80, 80, 255);
   Yellow      : constant Color := (253, 249, 0, 255);
   Gold        : constant Color := (255, 203, 0, 255);
   Orange      : constant Color := (255, 161, 0, 255);
   Pink        : constant Color := (255, 109, 194, 255);
   Red         : constant Color := (230, 41, 55, 255);
   Maroon      : constant Color := (190, 33, 55, 255);
   Green       : constant Color := (0, 228, 48, 255);
   Lime        : constant Color := (0, 158, 47, 255);
   Dark_Green  : constant Color := (0, 117, 44, 255);
   Sky_Blue    : constant Color := (102, 191, 255, 255);
   Blue        : constant Color := (0, 121, 241, 255);
   Dark_Blue   : constant Color := (0, 82, 172, 255);
   Purple      : constant Color := (200, 122, 255, 255);
   Violet      : constant Color := (135, 60, 190, 255);
   Dark_Purple : constant Color := (112, 31, 126, 255);
   Beige       : constant Color := (211, 176, 131, 255);
   Brown       : constant Color := (127, 106, 79, 255);
   Dark_Brown  : constant Color := (127, 106, 79, 255);

   White     : constant Color := (255, 255, 255, 255);
   Black     : constant Color := (0, 0, 0, 255);
   Blank     : constant Color := (0, 0, 0, 0);
   Magenta   : constant Color := (255, 0, 255, 255);
   Ray_White : constant Color := (245, 245, 245, 255);

   --  System/Window config flags
   type Config_Flag is new Interfaces.C.unsigned;

   --  Set to try enabling V-Sync on GPU
   Flag_Vsync_Hint : constant Config_Flag := 16#0000_0040#;
   --  Set to run program in fullscreen
   Flag_Fullscreen_Mode : constant Config_Flag := 16#0000_0002#;
   --  Set to allow resizable window
   Flag_Window_Resizable : constant Config_Flag := 16#0000_0004#;
   --  Set to disable window decoration (frame and buttons)
   Flag_Window_Undecorated : constant Config_Flag := 16#0000_0008#;
   --  Set to hide window
   Flag_Window_Hidden : constant Config_Flag := 16#0000_0080#;
   --  Set to minimize window (iconify)
   Flag_Window_Minimized : constant Config_Flag := 16#0000_0200#;
   --  Set to maximize window (expanded to monitor)
   Flag_Window_Maximized : constant Config_Flag := 16#0000_0400#;
   --  Set to window non focused
   Flag_Window_Unfocused : constant Config_Flag := 16#0000_0800#;
   --  Set to window always on top
   Flag_Window_Topmost : constant Config_Flag := 16#0000_1000#;
   --  Set to allow windows running while minimized
   Flag_Window_Always_Run : constant Config_Flag := 16#0000_0100#;
   --  Set to allow transparent framebuffer
   Flag_Window_Transparent : constant Config_Flag := 16#0000_0010#;
   --  Set to support HighDPI
   Flag_Window_HighDPI : constant Config_Flag := 16#0000_2000#;
   --  Set to try enabling MSAA 4X
   Flag_MSAA_4x_Hint : constant Config_Flag := 16#0000_0020#;
   --  Set to try enabling interlaced video format (for V3D)
   Flag_Interlaced_Hint : constant Config_Flag := 16#0001_0000#;

   --  Trace log level
   type Trace_Log_Level is
     (Log_All,
   --  Display all logs
   Log_Trace,
   --  Trace logging, intended for internal use only
   Log_Debug,
   --  Debug logging, used for internal debugging, it should be disabled on release builds
   Log_Info,
   --  Info logging, used for program execution info
   Log_Warning,
   --  Warning logging, used on recoverable failures
   Log_Error,
   --  Error logging, used on unrecoverable failures

      Log_Fatal,
   --  Fatal logging, used to abort program: exit(EXIT_FAILURE)
   Log_None
      --  Disable logging
      );

   --  Keyboard keys (US keyboard layout)
   type Keyboard_Key is new Interfaces.C.unsigned;

   --  Key: NULL, used for no key pressed
   Key_Null : constant Keyboard_Key := 0;
   --  Key: '
   Key_Apostrophe : constant Keyboard_Key := 39;
   --  Key: ,
   Key_Comma : constant Keyboard_Key := 44;
   --  Key: -
   Key_Minus : constant Keyboard_Key := 45;
   --  Key: .
   Key_Period : constant Keyboard_Key := 46;
   --  Key: /
   Key_Slash : constant Keyboard_Key := 47;
   --  Key: 0
   Key_Zero : constant Keyboard_Key := 48;
   --  Key: 1
   Key_One : constant Keyboard_Key := 49;
   --  Key: 2
   Key_Two : constant Keyboard_Key := 50;
   --  Key: 3
   Key_Three : constant Keyboard_Key := 51;
   --  Key: 4
   Key_Four : constant Keyboard_Key := 52;
   --  Key: 5
   Key_Five : constant Keyboard_Key := 53;
   --  Key: 6
   Key_Six : constant Keyboard_Key := 54;
   --  Key: 7
   Key_Seven : constant Keyboard_Key := 55;
   --  Key: 8
   Key_Eight : constant Keyboard_Key := 56;
   --  Key: 9
   Key_Nine : constant Keyboard_Key := 57;
   --  Key: ;
   Key_Semicolon : constant Keyboard_Key := 59;
   --  Key: =
   Key_Equal : constant Keyboard_Key := 61;
   --  Key: A | a
   Key_A : constant Keyboard_Key := 65;
   --  Key: B | b
   Key_B : constant Keyboard_Key := 66;
   --  Key: C | c
   Key_C : constant Keyboard_Key := 67;
   --  Key: D | d
   Key_D : constant Keyboard_Key := 68;
   --  Key: E | e
   Key_E : constant Keyboard_Key := 69;
   --  Key: F | f
   Key_F : constant Keyboard_Key := 70;
   --  Key: G | g
   Key_G : constant Keyboard_Key := 71;
   --  Key: H | h
   Key_H : constant Keyboard_Key := 72;
   --  Key: I | i
   Key_I : constant Keyboard_Key := 73;
   --  Key: J | j
   Key_J : constant Keyboard_Key := 74;
   --  Key: K | k
   Key_K : constant Keyboard_Key := 75;
   --  Key: L | l
   Key_L : constant Keyboard_Key := 76;
   --  Key: M | m
   Key_M : constant Keyboard_Key := 77;
   --  Key: N | n
   Key_N : constant Keyboard_Key := 78;
   --  Key: O | o
   Key_O : constant Keyboard_Key := 79;
   --  Key: P | p
   Key_P : constant Keyboard_Key := 80;
   --  Key: Q | q
   Key_Q : constant Keyboard_Key := 81;
   --  Key: R | r
   Key_R : constant Keyboard_Key := 82;
   --  Key: S | s
   Key_S : constant Keyboard_Key := 83;
   --  Key: T | t
   Key_T : constant Keyboard_Key := 84;
   --  Key: U | u
   Key_U : constant Keyboard_Key := 85;
   --  Key: V | v
   Key_V : constant Keyboard_Key := 86;
   --  Key: W | w
   Key_W : constant Keyboard_Key := 87;
   --  Key: X | x
   Key_X : constant Keyboard_Key := 88;
   --  Key: Y | y
   Key_Y : constant Keyboard_Key := 89;
   --  Key: Z | z
   Key_Z : constant Keyboard_Key := 90;
   --  Key: [
   Key_Left_Bracket : constant Keyboard_Key := 91;
   --  Key: '\'
   Key_Backslash : constant Keyboard_Key := 92;
   --  Key: ]
   Key_Right_Bracket : constant Keyboard_Key := 93;
   --  Key: `
   Key_Grave : constant Keyboard_Key := 96;
   --  Key: Space
   Key_Space : constant Keyboard_Key := 32;
   --  Key: Esc
   Key_Escape : constant Keyboard_Key := 256;
   --  Key: Enter
   Key_Enter : constant Keyboard_Key := 257;
   --  Key: Tab
   Key_Tab : constant Keyboard_Key := 258;
   --  Key: Backspace
   Key_Backspace : constant Keyboard_Key := 259;
   --  Key: Ins
   Key_Insert : constant Keyboard_Key := 260;
   --  Key: Del
   Key_Delete : constant Keyboard_Key := 261;
   --  Key: Cursor right
   Key_Right : constant Keyboard_Key := 262;
   --  Key: Cursor left
   Key_Left : constant Keyboard_Key := 263;
   --  Key: Cursor down
   Key_Down : constant Keyboard_Key := 264;
   --  Key: Cursor up
   Key_Up : constant Keyboard_Key := 265;
   --  Key: Page up
   Key_Page_Up : constant Keyboard_Key := 266;
   --  Key: Page down
   Key_Page_Down : constant Keyboard_Key := 267;
   --  Key: Home
   Key_Home : constant Keyboard_Key := 268;
   --  Key: End
   Key_End : constant Keyboard_Key := 269;
   --  Key: Caps lock
   Key_Caps_Lock : constant Keyboard_Key := 280;
   --  Key: Scroll down
   Key_Scroll_Lock : constant Keyboard_Key := 281;
   --  Key: Num lock
   Key_Num_Lock : constant Keyboard_Key := 282;
   --  Key: Print screen
   Key_Print_Screen : constant Keyboard_Key := 283;
   --  Key: Pause
   Key_Pause : constant Keyboard_Key := 284;
   --  Key: F1
   Key_F1 : constant Keyboard_Key := 290;
   --  Key: F2
   Key_F2 : constant Keyboard_Key := 291;
   --  Key: F3
   Key_F3 : constant Keyboard_Key := 292;
   --  Key: F4
   Key_F4 : constant Keyboard_Key := 293;
   --  Key: F5
   Key_F5 : constant Keyboard_Key := 294;
   --  Key: F6
   Key_F6 : constant Keyboard_Key := 295;
   --  Key: F7
   Key_F7 : constant Keyboard_Key := 296;
   --  Key: F8
   Key_F8 : constant Keyboard_Key := 297;
   --  Key: F9
   Key_F9 : constant Keyboard_Key := 298;
   --  Key: F10
   Key_F10 : constant Keyboard_Key := 299;
   --  Key: F11
   Key_F11 : constant Keyboard_Key := 300;
   --  Key: F12
   Key_F12 : constant Keyboard_Key := 301;
   --  Key: Shift left
   Key_Left_Shift : constant Keyboard_Key := 340;
   --  Key: Control left
   Key_Left_Control : constant Keyboard_Key := 341;
   --  Key: Alt left
   Key_Left_Alt : constant Keyboard_Key := 342;
   --  Key: Super left
   Key_Left_Super : constant Keyboard_Key := 343;
   --  Key: Shift right
   Key_Right_Shift : constant Keyboard_Key := 344;
   --  Key: Control right
   Key_Right_Control : constant Keyboard_Key := 345;
   --  Key: Alt right
   Key_Right_Alt : constant Keyboard_Key := 346;
   --  Key: Super right
   Key_Right_Super : constant Keyboard_Key := 347;
   --  Key: KB menu
   Key_Kb_Menu : constant Keyboard_Key := 348;
   --  Key: Keypad 0
   Key_Kp_0 : constant Keyboard_Key := 320;
   --  Key: Keypad 1
   Key_Kp_1 : constant Keyboard_Key := 321;
   --  Key: Keypad 2
   Key_Kp_2 : constant Keyboard_Key := 322;
   --  Key: Keypad 3
   Key_Kp_3 : constant Keyboard_Key := 323;
   --  Key: Keypad 4
   Key_Kp_4 : constant Keyboard_Key := 324;
   --  Key: Keypad 5
   Key_Kp_5 : constant Keyboard_Key := 325;
   --  Key: Keypad 6
   Key_Kp_6 : constant Keyboard_Key := 326;
   --  Key: Keypad 7
   Key_Kp_7 : constant Keyboard_Key := 327;
   --  Key: Keypad 8
   Key_Kp_8 : constant Keyboard_Key := 328;
   --  Key: Keypad 9
   Key_Kp_9 : constant Keyboard_Key := 329;
   --  Key: Keypad .
   Key_Kp_Decimal : constant Keyboard_Key := 330;
   --  Key: Keypad /
   Key_Kp_Divide : constant Keyboard_Key := 331;
   --  Key: Keypad *
   Key_Kp_Multiply : constant Keyboard_Key := 332;
   --  Key: Keypad -
   Key_Kp_Subtract : constant Keyboard_Key := 333;
   --  Key: Keypad +
   Key_Kp_Add : constant Keyboard_Key := 334;
   --  Key: Keypad Enter
   Key_Kp_Enter : constant Keyboard_Key := 335;
   --  Key: Keypad =
   Key_Kp_Equal : constant Keyboard_Key := 336;
   --  Key: Android back button
   Key_Back : constant Keyboard_Key := 4;
   --  Key: Android menu button
   Key_Menu : constant Keyboard_Key := 82;
   --  Key: Android volume up button
   Key_Volume_Up : constant Keyboard_Key := 24;
   --  Key: Android volume down button
   Key_Volume_Down : constant Keyboard_Key := 25;

   --  Mouse buttons
   type Mouse_Button is new Interfaces.C.unsigned;

   --  Mouse button left
   Mouse_Button_Left : constant Mouse_Button := 0;
   --  Mouse button right
   Mouse_Button_Right : constant Mouse_Button := 1;
   --  Mouse button middle (pressed wheel)
   Mouse_Button_Middle : constant Mouse_Button := 2;
   --  Mouse button side (advanced mouse device)
   Mouse_Button_Side : constant Mouse_Button := 3;
   --  Mouse button extra (advanced mouse device)
   Mouse_Button_Extra : constant Mouse_Button := 4;
   --  Mouse button fordward (advanced mouse device)
   Mouse_Button_Forward : constant Mouse_Button := 5;
   --  Mouse button back (advanced mouse device)
   Mouse_Button_Back : constant Mouse_Button := 6;

   --  Mouse cursor
   type Mouse_Cursor is new Interfaces.C.unsigned;

   --  Default pointer shape
   Mouse_Cursor_Default : constant Mouse_Cursor := 0;
   --  Arrow shape
   Mouse_Cursor_Arrow : constant Mouse_Cursor := 1;
   --  Text writing cursor shape
   Mouse_Cursor_Ibeam : constant Mouse_Cursor := 2;
   --  Cross shape
   Mouse_Cursor_Crosshair : constant Mouse_Cursor := 3;
   --  Pointing hand cursor
   Mouse_Cursor_Pointing_Hand : constant Mouse_Cursor := 4;
   --  Horizontal resize/move arrow shape
   Mouse_Cursor_Resize_EW : constant Mouse_Cursor := 5;
   --  Vertical resize/move arrow shape
   Mouse_Cursor_Resize_NS : constant Mouse_Cursor := 6;
   --  Top-left to bottom-right diagonal resize/move arrow shape
   Mouse_Cursor_Resize_NWSE : constant Mouse_Cursor := 7;
   --  The top-right to bottom-left diagonal resize/move arrow shape
   Mouse_Cursor_Resize_NESW : constant Mouse_Cursor := 8;
   --  The omni-directional resize/move cursor shape
   Mouse_Cursor_Resize_All : constant Mouse_Cursor := 9;
   --  The operation-not-allowed shape
   Mouse_Cursor_Not_Allowed : constant Mouse_Cursor := 10;

   --  Gamepad buttons
   type Gamepad_Button is new Interfaces.C.unsigned;

   --  Unknown button, just for error checking
   Gamepad_Button_Unknown : constant Gamepad_Button := 0;
   --  Gamepad left DPAD up button
   Gamepad_Button_Left_Face_Up : constant Gamepad_Button := 1;
   --  Gamepad left DPAD right button
   Gamepad_Button_Left_Face_Right : constant Gamepad_Button := 2;
   --  Gamepad left DPAD down button
   Gamepad_Button_Left_Face_Down : constant Gamepad_Button := 3;
   --  Gamepad left DPAD left button
   Gamepad_Button_Left_Face_Left : constant Gamepad_Button := 4;
   --  Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
   Gamepad_Button_Right_Face_Up : constant Gamepad_Button := 5;
   --  Gamepad right button right (i.e. PS3: Square, Xbox: X)
   Gamepad_Button_Right_Face_Right : constant Gamepad_Button := 6;
   --  Gamepad right button down (i.e. PS3: Cross, Xbox: A)
   Gamepad_Button_Right_Face_Down : constant Gamepad_Button := 7;
   --  Gamepad right button left (i.e. PS3: Circle, Xbox: B)
   Gamepad_Button_Right_Face_Left : constant Gamepad_Button := 8;
   --  Gamepad top/back trigger left (first), it could be a trailing button
   Gamepad_Button_Left_Trigger_1 : constant Gamepad_Button := 9;
   --  Gamepad top/back trigger left (second), it could be a trailing button
   Gamepad_Button_Left_Trigger_2 : constant Gamepad_Button := 10;
   --  Gamepad top/back trigger right (one), it could be a trailing button
   Gamepad_Button_Right_Trigger_1 : constant Gamepad_Button := 11;
   --  Gamepad top/back trigger right (second), it could be a trailing button
   Gamepad_Button_Right_Trigger_2 : constant Gamepad_Button := 12;
   --  Gamepad center buttons, left one (i.e. PS3: Select)
   Gamepad_Button_Middle_Left : constant Gamepad_Button := 13;
   --  Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
   Gamepad_Button_Middle : constant Gamepad_Button := 14;
   --  Gamepad center buttons, right one (i.e. PS3: Start)
   Gamepad_Button_Middle_Right : constant Gamepad_Button := 15;
   --  Gamepad joystick pressed button left
   Gamepad_Button_Left_Thumb : constant Gamepad_Button := 16;
   --  Gamepad joystick pressed button right
   Gamepad_Button_Right_Thumb : constant Gamepad_Button := 17;

   --  Gamepad axis
   type Gamepad_Axis is new Interfaces.C.unsigned;

   --  Gamepad left stick X axis
   Gamepad_Axis_Left_X : constant Gamepad_Axis := 0;
   --  Gamepad left stick Y axis
   Gamepad_Axis_Left_Y : constant Gamepad_Axis := 1;
   --  Gamepad right stick X axis
   Gamepad_Axis_Right_X : constant Gamepad_Axis := 2;
   --  Gamepad right stick Y axis
   Gamepad_Axis_Right_Y : constant Gamepad_Axis := 3;
   --  Gamepad back trigger left, pressure level: [1..-1]
   Gamepad_Axis_Left_Trigger : constant Gamepad_Axis := 4;
   --  Gamepad back trigger right, pressure level: [1..-1]
   Gamepad_Axis_Right_Trigger : constant Gamepad_Axis := 5;

   --  Material map index
   type Material_Map_Index is new Interfaces.C.unsigned;

   --  Albedo material (same as: MATERIAL_MAP_DIFFUSE)
   Material_Map_Albedo : constant Material_Map_Index := 0;
   --  Metalness material (same as: MATERIAL_MAP_SPECULAR)
   Material_Map_Metalness : constant Material_Map_Index := 1;
   --  Normal material
   Material_Map_Normal : constant Material_Map_Index := 2;
   --  Roughness material
   Material_Map_Roughness : constant Material_Map_Index := 3;
   --  Ambient occlusion material
   Material_Map_Occlusion : constant Material_Map_Index := 4;
   --  Emission material
   Material_Map_Emission : constant Material_Map_Index := 5;
   --  Heightmap material
   Material_Map_Height : constant Material_Map_Index := 6;
   --  Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
   Material_Map_Cubemap : constant Material_Map_Index := 7;
   --  Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
   Material_Map_Irradiance : constant Material_Map_Index := 8;
   --  Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
   Material_Map_Prefilter : constant Material_Map_Index := 9;
   --  Brdf material
   Material_Map_Brdf : constant Material_Map_Index := 10;

   Material_Map_Diffuse : constant Material_Map_Index := Material_Map_Albedo;

   Material_Map_Specular : constant Material_Map_Index :=
     Material_Map_Metalness;

   --  Shader location index
   type Shader_Location_Index is new Interfaces.C.unsigned;

   --  Shader location: vertex attribute: position
   Shader_Loc_Vertex_Position : constant Shader_Location_Index := 0;
   --  Shader location: vertex attribute: texcoord01
   Shader_Loc_Vertex_Texcoord01 : constant Shader_Location_Index := 1;
   --  Shader location: vertex attribute: texcoord02
   Shader_Loc_Vertex_Texcoord02 : constant Shader_Location_Index := 2;
   --  Shader location: vertex attribute: normal
   Shader_Loc_Vertex_Normal : constant Shader_Location_Index := 3;
   --  Shader location: vertex attribute: tangent
   Shader_Loc_Vertex_Tangent : constant Shader_Location_Index := 4;
   --  Shader location: vertex attribute: color
   Shader_Loc_Vertex_Color : constant Shader_Location_Index := 5;
   --  Shader location: matrix uniform: model-view-projection
   Shader_Loc_Matrix_Mvp : constant Shader_Location_Index := 6;
   --  Shader location: matrix uniform: view (camera transform)
   Shader_Loc_Matrix_View : constant Shader_Location_Index := 7;
   --  Shader location: matrix uniform: projection
   Shader_Loc_Matrix_Projection : constant Shader_Location_Index := 8;
   --  Shader location: matrix uniform: model (transform)
   Shader_Loc_Matrix_Model : constant Shader_Location_Index := 9;
   --  Shader location: matrix uniform: normal
   Shader_Loc_Matrix_Normal : constant Shader_Location_Index := 10;
   --  Shader location: vector uniform: view
   Shader_Loc_Vector_View : constant Shader_Location_Index := 11;
   --  Shader location: vector uniform: diffuse color
   Shader_Loc_Color_Diffuse : constant Shader_Location_Index := 12;
   --  Shader location: vector uniform: specular color
   Shader_Loc_Color_Specular : constant Shader_Location_Index := 13;
   --  Shader location: vector uniform: ambient color
   Shader_Loc_Color_Ambient : constant Shader_Location_Index := 14;
   --  Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
   Shader_Loc_Map_Albedo : constant Shader_Location_Index := 15;
   --  Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
   Shader_Loc_Map_Metalness : constant Shader_Location_Index := 16;
   --  Shader location: sampler2d texture: normal
   Shader_Loc_Map_Normal : constant Shader_Location_Index := 17;
   --  Shader location: sampler2d texture: roughness
   Shader_Loc_Map_Roughness : constant Shader_Location_Index := 18;
   --  Shader location: sampler2d texture: occlusion
   Shader_Loc_Map_Occlusion : constant Shader_Location_Index := 19;
   --  Shader location: sampler2d texture: emission
   Shader_Loc_Map_Emission : constant Shader_Location_Index := 20;
   --  Shader location: sampler2d texture: height
   Shader_Loc_Map_Height : constant Shader_Location_Index := 21;
   --  Shader location: samplerCube texture: cubemap
   Shader_Loc_Map_Cubemap : constant Shader_Location_Index := 22;
   --  Shader location: samplerCube texture: irradiance
   Shader_Loc_Map_Irradiance : constant Shader_Location_Index := 23;
   --  Shader location: samplerCube texture: prefilter
   Shader_Loc_Map_Prefilter : constant Shader_Location_Index := 24;
   --  Shader location: sampler2d texture: brdf
   Shader_Loc_Map_Brdf : constant Shader_Location_Index := 25;

   Shader_Loc_Map_Diffuse : constant Shader_Location_Index :=
     Shader_Loc_Map_Albedo;

   Shader_Loc_Map_Specular : constant Shader_Location_Index :=
     Shader_Loc_Map_Metalness;

   --  Shader uniform data type
   type Shader_Uniform_Data_Type is new Interfaces.C.unsigned;

   --  Shader uniform type: float
   Shader_Uniform_Float : constant Shader_Uniform_Data_Type := 0;
   --  Shader uniform type: vec2 (2 float)
   Shader_Uniform_Vec2 : constant Shader_Uniform_Data_Type := 1;
   --  Shader uniform type: vec3 (3 float)
   Shader_Uniform_Vec3 : constant Shader_Uniform_Data_Type := 2;
   --  Shader uniform type: vec4 (4 float)
   Shader_Uniform_Vec4 : constant Shader_Uniform_Data_Type := 3;
   --  Shader uniform type: int
   Shader_Uniform_Int : constant Shader_Uniform_Data_Type := 4;
   --  Shader uniform type: ivec2 (2 int)
   Shader_Uniform_Ivec2 : constant Shader_Uniform_Data_Type := 5;
   --  Shader uniform type: ivec3 (3 int)
   Shader_Uniform_Ivec3 : constant Shader_Uniform_Data_Type := 6;
   --  Shader uniform type: ivec4 (4 int)
   Shader_Uniform_Ivec4 : constant Shader_Uniform_Data_Type := 7;
   --  Shader uniform type: sampler2d
   Shader_Uniform_Sampler2D : constant Shader_Uniform_Data_Type := 8;

   --  Shader attribute data types
   type Shader_Attribute_Data_Type is new Interfaces.C.unsigned;

   --  Shader attribute type: float
   Shader_Attrib_Float : constant Shader_Attribute_Data_Type := 0;
   --  Shader attribute type: vec2 (2 float)
   Shader_Attrib_Vec2 : constant Shader_Attribute_Data_Type := 1;
   --  Shader attribute type: vec3 (3 float)
   Shader_Attrib_Vec3 : constant Shader_Attribute_Data_Type := 2;
   --  Shader attribute type: vec4 (4 float)
   Shader_Attrib_Vec4 : constant Shader_Attribute_Data_Type := 3;

   --  Pixel formats
   type Pixel_Format is new Interfaces.C.unsigned;

   --  8 bit per pixel (no alpha)
   Pixelformat_Uncompressed_Grayscale : constant Pixel_Format := 1;
   --  8*2 bpp (2 channels)
   Pixelformat_Uncompressed_Gray_Alpha : constant Pixel_Format := 2;
   --  16 bpp
   Pixelformat_Uncompressed_R5G6B5 : constant Pixel_Format := 3;
   --  24 bpp
   Pixelformat_Uncompressed_R8G8B8 : constant Pixel_Format := 4;
   --  16 bpp (1 bit alpha)
   Pixelformat_Uncompressed_R5G5B5A1 : constant Pixel_Format := 5;
   --  16 bpp (4 bit alpha)
   Pixelformat_Uncompressed_R4G4B4A4 : constant Pixel_Format := 6;
   --  32 bpp
   Pixelformat_Uncompressed_R8G8B8A8 : constant Pixel_Format := 7;
   --  32 bpp (1 channel - float)
   Pixelformat_Uncompressed_R32 : constant Pixel_Format := 8;
   --  32*3 bpp (3 channels - float)
   Pixelformat_Uncompressed_R32G32B32 : constant Pixel_Format := 9;
   --  32*4 bpp (4 channels - float)
   Pixelformat_Uncompressed_R32G32B32A32 : constant Pixel_Format := 10;
   --  4 bpp (no alpha)
   Pixelformat_Compressed_DXT1_RGB : constant Pixel_Format := 11;
   --  4 bpp (1 bit alpha)
   Pixelformat_Compressed_DXT1_RGBA : constant Pixel_Format := 12;
   --  8 bpp
   Pixelformat_Compressed_DXT3_RGBA : constant Pixel_Format := 13;
   --  8 bpp
   Pixelformat_Compressed_DXT5_RGBA : constant Pixel_Format := 14;
   --  4 bpp
   Pixelformat_Compressed_ETC1_RGB : constant Pixel_Format := 15;
   --  4 bpp
   Pixelformat_Compressed_ETC2_RGB : constant Pixel_Format := 16;
   --  8 bpp
   Pixelformat_Compressed_ETC2_EAC_RGBA : constant Pixel_Format := 17;
   --  4 bpp
   Pixelformat_Compressed_PVRT_RGB : constant Pixel_Format := 18;
   --  4 bpp
   Pixelformat_Compressed_PVRT_RGBA : constant Pixel_Format := 19;
   --  8 bpp
   Pixelformat_Compressed_ASTC_4X4_RGBA : constant Pixel_Format := 20;
   --  2 bpp
   Pixelformat_Compressed_ASTC_8X8_RGBA : constant Pixel_Format := 21;

   --  Texture parameters: filter mode
   type Texture_Filter is new Interfaces.C.unsigned;

   --  No filter, just pixel approximation
   Texture_Filter_Point : constant Texture_Filter := 0;
   --  Linear filtering
   Texture_Filter_Bilinear : constant Texture_Filter := 1;
   --  Trilinear filtering (linear with mipmaps)
   Texture_Filter_Trilinear : constant Texture_Filter := 2;
   --  Anisotropic filtering 4x
   Texture_Filter_Anisotropic_4x : constant Texture_Filter := 3;
   --  Anisotropic filtering 8x
   Texture_Filter_Anisotropic_8x : constant Texture_Filter := 4;
   --  Anisotropic filtering 16x
   Texture_Filter_Anisotropic_16x : constant Texture_Filter := 5;

   --  Texture parameters: wrap mode
   type Texture_Wrap is new Interfaces.C.unsigned;

   --  Repeats texture in tiled mode
   Texture_Wrap_Repeat : constant Texture_Wrap := 0;
   --  Clamps texture to edge pixel in tiled mode
   Texture_Wrap_Clamp : constant Texture_Wrap := 1;
   --  Mirrors and repeats the texture in tiled mode
   Texture_Wrap_Mirror_Repeat : constant Texture_Wrap := 2;
   --  Mirrors and clamps to border the texture in tiled mode
   Texture_Wrap_Mirror_Clamp : constant Texture_Wrap := 3;

   --  Cubemap layouts
   type Cubemap_Layout is new Interfaces.C.unsigned;

   --  Automatically detect layout type
   Cubemap_Layout_Auto_Detect : constant Cubemap_Layout := 0;
   --  Layout is defined by a vertical line with faces
   Cubemap_Layout_Line_Vertical : constant Cubemap_Layout := 1;
   --  Layout is defined by an horizontal line with faces
   Cubemap_Layout_Line_Horizontal : constant Cubemap_Layout := 2;
   --  Layout is defined by a 3x4 cross with cubemap faces
   Cubemap_Layout_Cross_Three_By_Four : constant Cubemap_Layout := 3;
   --  Layout is defined by a 4x3 cross with cubemap faces
   Cubemap_Layout_Cross_Four_By_Three : constant Cubemap_Layout := 4;
   --  Layout is defined by a panorama image (equirectangular map)
   Cubemap_Layout_Panorama : constant Cubemap_Layout := 5;

   --  Font type, defines generation method
   type Font_Type is new Interfaces.C.unsigned;

   --  Default font generation, anti-aliased
   Font_Default : constant Font_Type := 0;
   --  Bitmap font generation, no anti-aliasing
   Font_Bitmap : constant Font_Type := 1;
   --  SDF font generation, requires external shader
   Font_SDF : constant Font_Type := 2;

   --  Color blending modes (pre-defined)
   type Blend_Mode is new Interfaces.C.unsigned;

   --  Blend textures considering alpha (default)
   Blend_Alpha : constant Blend_Mode := 0;
   --  Blend textures adding colors
   Blend_Additive : constant Blend_Mode := 1;
   --  Blend textures multiplying colors
   Blend_Multiplied : constant Blend_Mode := 2;
   --  Blend textures adding colors (alternative)
   Blend_Add_Colors : constant Blend_Mode := 3;
   --  Blend textures subtracting colors (alternative)
   Blend_Subtract_Colors : constant Blend_Mode := 4;
   --  Blend premultiplied textures considering alpha
   Blend_Alpha_Premul : constant Blend_Mode := 5;
   --  Blend textures using custom src/dst factors (use rlSetBlendMode())
   Blend_Custom : constant Blend_Mode := 6;

   --  Gesture
   type Gesture is new Interfaces.C.unsigned;

   --  No gesture
   Gesture_None : constant Gesture := 0;
   --  Tap gesture
   Gesture_Tap : constant Gesture := 1;
   --  Double tap gesture
   Gesture_Doubletap : constant Gesture := 2;
   --  Hold gesture
   Gesture_Hold : constant Gesture := 4;
   --  Drag gesture
   Gesture_Drag : constant Gesture := 8;
   --  Swipe right gesture
   Gesture_Swipe_Right : constant Gesture := 16;
   --  Swipe left gesture
   Gesture_Swipe_Left : constant Gesture := 32;
   --  Swipe up gesture
   Gesture_Swipe_Up : constant Gesture := 64;
   --  Swipe down gesture
   Gesture_Swipe_Down : constant Gesture := 128;
   --  Pinch in gesture
   Gesture_Pinch_In : constant Gesture := 256;
   --  Pinch out gesture
   Gesture_Pinch_Out : constant Gesture := 512;

   --  Camera system modes
   type Camera_Mode is new Interfaces.C.unsigned;

   --  Custom camera
   Camera_Custom : constant Camera_Mode := 0;
   --  Free camera
   Camera_Free : constant Camera_Mode := 1;
   --  Orbital camera
   Camera_Orbital : constant Camera_Mode := 2;
   --  First person camera
   Camera_First_Person : constant Camera_Mode := 3;
   --  Third person camera
   Camera_Third_Person : constant Camera_Mode := 4;

   --  Camera projection
   type Camera_Projection is new Interfaces.C.unsigned;

   --  Perspective projection
   Camera_Perspective : constant Camera_Projection := 0;
   --  Orthographic projection
   Camera_Orthographic : constant Camera_Projection := 1;

   --  N-patch layout
   type N_Patch_Layout is new Interfaces.C.unsigned;

   --  Npatch layout: 3x3 tiles
   Npatch_Nine_Patch : constant N_Patch_Layout := 0;
   --  Npatch layout: 1x3 tiles
   Npatch_Three_Patch_Vertical : constant N_Patch_Layout := 1;
   --  Npatch layout: 3x1 tiles
   Npatch_Three_Patch_Horizontal : constant N_Patch_Layout := 2;

   type Trace_Log_Callback is access procedure
     (Log_Level : RayLib.Trace_Log_Level; Text : String);
   type Load_File_Data_Callback is access function
     (File_Name : String) return Stream_Element_Array;
   type Save_File_Data_Callback is access function
     (File_Name : String; Data : Stream_Element_Array) return Boolean;
   type Load_File_Text_Callback is access function
     (File_Name : String) return String;
   type Save_File_Text_Callback is access function
     (File_Name, Text : String) return Boolean;

   --  Initialize window and OpenGL context
   procedure Init_Window (Width : Integer; Height : Integer; Title : String);

   --  Check if Key_Escape pressed or Close icon pressed
   function Window_Should_Close return Boolean;

   --  Close window and unload OpenGL context
   procedure Close_Window;

   --  Check if window has been initialized successfully
   function Is_Window_Ready return Boolean;

   --  Check if window is currently fullscreen
   function Is_Window_Fullscreen return Boolean;

   --  Check if window is currently hidden (only PLATFORM_DESKTOP)
   function Is_Window_Hidden return Boolean;

   --  Check if window is currently minimized (only PLATFORM_DESKTOP)
   function Is_Window_Minimized return Boolean;

   --  Check if window is currently maximized (only PLATFORM_DESKTOP)
   function Is_Window_Maximized return Boolean;

   --  Check if window is currently focused (only PLATFORM_DESKTOP)
   function Is_Window_Focused return Boolean;

   --  Check if window has been resized last frame
   function Is_Window_Resized return Boolean;

   --  Check if one specific window flag is enabled
   function Is_Window_State (Flag : Config_Flag) return Boolean;

   --  Set window configuration state using flags (only PLATFORM_DESKTOP)
   procedure Set_Window_State (Flags : Config_Flag);

   --  Clear window configuration state flags
   procedure Clear_Window_State (Flags : Config_Flag);

   --  Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
   procedure Toggle_Fullscreen;

   --  Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
   procedure Maximize_Window;

   --  Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
   procedure Minimize_Window;

   --  Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
   procedure Restore_Window;

   --  Set icon for window (only PLATFORM_DESKTOP)
   procedure Set_Window_Icon (Image : RayLib.Image);

   --  Set title for window (only PLATFORM_DESKTOP)
   procedure Set_Window_Title (Title : String);

   --  Set window position on screen (only PLATFORM_DESKTOP)
   procedure Set_Window_Position (X : Integer; Y : Integer);

   --  Set monitor for the current window (fullscreen mode)
   procedure Set_Window_Monitor (Monitor : Integer);

   --  Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
   procedure Set_Window_Min_Size (Width : Integer; Height : Integer);

   --  Set window dimensions
   procedure Set_Window_Size (Width : Integer; Height : Integer);

   --  Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)
   procedure Set_Window_Opacity (Opacity : Float);

   --  Get native window handle
   function Get_Window_Handle return System.Address;

   --  Get current screen width
   function Get_Screen_Width return Integer;

   --  Get current screen height
   function Get_Screen_Height return Integer;

   --  Get current render width (it considers HiDPI)
   function Get_Render_Width return Integer;

   --  Get current render height (it considers HiDPI)
   function Get_Render_Height return Integer;

   --  Get number of connected monitors
   function Get_Monitor_Count return Integer;

   --  Get current connected monitor
   function Get_Current_Monitor return Integer;

   --  Get specified monitor position
   function Get_Monitor_Position (Monitor : Integer) return RayLib.Vector2;

   --  Get specified monitor width (max available by monitor)
   function Get_Monitor_Width (Monitor : Integer) return Integer;

   --  Get specified monitor height (max available by monitor)
   function Get_Monitor_Height (Monitor : Integer) return Integer;

   --  Get specified monitor physical width in millimetres
   function Get_Monitor_Physical_Width (Monitor : Integer) return Integer;

   --  Get specified monitor physical height in millimetres
   function Get_Monitor_Physical_Height (Monitor : Integer) return Integer;

   --  Get specified monitor refresh rate
   function Get_Monitor_Refresh_Rate (Monitor : Integer) return Integer;

   --  Get window position XY on monitor
   function Get_Window_Position return RayLib.Vector2;

   --  Get window scale DPI factor
   function Get_Window_Scale_DPI return RayLib.Vector2;

   --  Get the human-readable, UTF-8 encoded name of the primary monitor
   function Get_Monitor_Name (Monitor : Integer) return String;

   --  Set clipboard text content
   procedure Set_Clipboard_Text (Text : String);

   --  Get clipboard text content
   function Get_Clipboard_Text return String;

   --  Swap back buffer with front buffer (screen drawing)
   procedure Swap_Screen_Buffer;

   --  Register all input events
   procedure Poll_Input_Events;

   --  Wait for some milliseconds (halt program execution)
   procedure Wait_Time (Ms : Float);

   --  Shows cursor
   procedure Show_Cursor;

   --  Hides cursor
   procedure Hide_Cursor;

   --  Check if cursor is not visible
   function Is_Cursor_Hidden return Boolean;

   --  Enables cursor (unlock cursor)
   procedure Enable_Cursor;

   --  Disables cursor (lock cursor)
   procedure Disable_Cursor;

   --  Check if cursor is on the screen
   function Is_Cursor_On_Screen return Boolean;

   --  Set background color (framebuffer clear color)
   procedure Clear_Background (Color : RayLib.Color);

   --  Setup canvas (framebuffer) to start drawing
   procedure Begin_Drawing;

   --  End canvas drawing and swap buffers (double buffering)
   procedure End_Drawing;

   --  Begin 2D mode with custom camera (2D)
   procedure Begin_Mode2D (Camera : RayLib.Camera2D);

   --  Ends 2D mode with custom camera
   procedure End_Mode2D;

   --  Begin 3D mode with custom camera (3D)
   procedure Begin_Mode3D (Camera : RayLib.Camera3D);

   --  Ends 3D mode and returns to default 2D orthographic mode
   procedure End_Mode3D;

   --  Begin drawing to render texture
   procedure Begin_Texture_Mode (Target : RayLib.Render_Texture2D);

   --  Ends drawing to render texture
   procedure End_Texture_Mode;

   --  Begin custom shader drawing
   procedure Begin_Shader_Mode (Shader : RayLib.Shader);

   --  End custom shader drawing (use default shader)
   procedure End_Shader_Mode;

   --  Begin blending mode (alpha, additive, multiplied, subtract, custom)
   procedure Begin_Blend_Mode (Mode : Integer);

   --  End blending mode (reset to default: alpha blending)
   procedure End_Blend_Mode;

   --  Begin scissor mode (define screen area for following drawing)
   procedure Begin_Scissor_Mode
     (X : Integer; Y : Integer; Width : Integer; Height : Integer);

   --  End scissor mode
   procedure End_Scissor_Mode;

   --  Begin stereo rendering (requires VR simulator)
   procedure Begin_VR_Stereo_Mode (Config : RayLib.VR_Stereo_Config);

   --  End stereo rendering (requires VR simulator)
   procedure End_VR_Stereo_Mode;

   --  Load VR stereo config for VR simulator device parameters
   function Load_VR_Stereo_Config
     (Device : RayLib.VR_Device_Info) return RayLib.VR_Stereo_Config;

   --  Unload VR stereo config
   procedure Unload_VR_Stereo_Config (Config : RayLib.VR_Stereo_Config);

   --  Load shader from files and bind default locations
   function Load_Shader
     (Vs_File_Name : String; Fs_File_Name : String) return RayLib.Shader;

   --  Load shader from code strings and bind default locations
   function Load_Shader_From_Memory
     (Vs_Code : String; Fs_Code : String) return RayLib.Shader;

   --  Get shader uniform location
   function Get_Shader_Location
     (Shader : RayLib.Shader; Uniform_Name : String) return Integer;

   --  Get shader attribute location
   function Get_Shader_Location_Attrib
     (Shader : RayLib.Shader; Attrib_Name : String) return Integer;

   --  Set shader uniform value
   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Value : System.Address;
      Uniform_Type : Integer);

   --  Set shader uniform value vector
   procedure Set_Shader_Value_V
     (Shader : RayLib.Shader; Loc_Index : Integer; Value : System.Address;
      Uniform_Type : Integer; Count : Integer);

   --  Set shader uniform value (matrix 4x4)
   procedure Set_Shader_Value_Matrix
     (Shader : RayLib.Shader; Loc_Index : Integer; Mat : RayLib.Matrix);

   --  Set shader uniform value for texture (sampler2d)
   procedure Set_Shader_Value_Texture
     (Shader : RayLib.Shader; Loc_Index : Integer; Texture : RayLib.Texture2D);

   --  Unload shader from GPU memory (VRAM)
   procedure Unload_Shader (Shader : RayLib.Shader);

   --  Get a ray trace from mouse position
   function Get_Mouse_Ray
     (Mouse_Position : RayLib.Vector2; Camera : RayLib.Camera)
      return RayLib.Ray;

   --  Get camera transform matrix (view matrix)
   function Get_Camera_Matrix (Camera : RayLib.Camera) return RayLib.Matrix;

   --  Get camera 2d transform matrix
   function Get_Camera_Matrix2D
     (Camera : RayLib.Camera2D) return RayLib.Matrix;

   --  Get the screen space position for a 3d world space position
   function Get_World_To_Screen
     (Position : RayLib.Vector3; Camera : RayLib.Camera) return RayLib.Vector2;

   --  Get size position for a 3d world space position
   function Get_World_To_Screen
     (Position : RayLib.Vector3; Camera : RayLib.Camera; Width : Integer;
      Height   : Integer) return RayLib.Vector2;

   --  Get the screen space position for a 2d camera world space position
   function Get_World_To_Screen2D
     (Position : RayLib.Vector2; Camera : RayLib.Camera2D)
      return RayLib.Vector2;

   --  Get the world space position for a 2d camera screen space position
   function Get_Screen_To_World2D
     (Position : RayLib.Vector2; Camera : RayLib.Camera2D)
      return RayLib.Vector2;

   --  Set target FPS (maximum)
   procedure Set_Target_FPS (Fps : Integer);

   --  Get current FPS
   function Get_FPS return Integer;

   --  Get time in seconds for last frame drawn (delta time)
   function Get_Frame_Time return Float;

   --  Get elapsed time in seconds since InitWindow()
   function Get_Time return Long_Float;

   --  Get a random value between min and max (both included)
   function Get_Random_Value (Min : Integer; Max : Integer) return Integer;

   --  Set the seed for the random number generator
   procedure Set_Random_Seed (Seed : Natural);

   --  Takes a screenshot of current screen (filename extension defines format)
   procedure Take_Screenshot (File_Name : String);

   --  Setup init configuration flags (view FLAGS)
   procedure Set_Config_Flags (Flags : Config_Flag);

   --  Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
   procedure Trace_Log (Log_Level : Trace_Log_Level; Text : String);

   --  Set the current threshold (minimum) log level
   procedure Set_Trace_Log_Level (Log_Level : Trace_Log_Level);

   --  Set custom trace log
   procedure Set_Trace_Log_Callback (Callback : RayLib.Trace_Log_Callback);

   --  Set custom file binary data loader
   procedure Set_Load_File_Data_Callback
     (Callback : RayLib.Load_File_Data_Callback);

   --  Set custom file binary data saver
   procedure Set_Save_File_Data_Callback
     (Callback : RayLib.Save_File_Data_Callback);

   --  Set custom file text data loader
   procedure Set_Load_File_Text_Callback
     (Callback : RayLib.Load_File_Text_Callback);

   --  Set custom file text data saver
   procedure Set_Save_File_Text_Callback
     (Callback : RayLib.Save_File_Text_Callback);

   --  Load file data as byte array (read)
   function Load_File_Data (File_Name : String) return Stream_Element_Array;

   --  Save data to file from byte array (write), returns true on success
   function Save_File_Data
     (File_Name : String; Data : Stream_Element_Array) return Boolean;

   --  Load text data from file (read), returns string
   function Load_File_Text (File_Name : String) return String;

   --  Save text data to file (write), returns true on success
   function Save_File_Text (File_Name : String; Text : String) return Boolean;

   --  Check if file exists
   function File_Exists (File_Name : String) return Boolean;

   --  Check if a directory path exists
   function Directory_Exists (Dir_Path : String) return Boolean;

   --  Check file extension (including point: .png, .wav)
   function Is_File_Extension
     (File_Name : String; Ext : String) return Boolean;

   --  Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
   function Get_File_Length (File_Name : String) return Integer;

   --  Get pointer to extension for a filename string (includes dot: '.png')
   function Get_File_Extension (File_Name : String) return String;

   --  Get pointer to filename for a path string
   function Get_File_Name (File_Path : String) return String;

   --  Get filename string without extension (uses static string)
   function Get_File_Name_Without_Ext (File_Path : String) return String;

   --  Get full path for a given fileName with path (uses static string)
   function Get_Directory_Path (File_Path : String) return String;

   --  Get previous directory path for a given path (uses static string)
   function Get_Prev_Directory_Path (Dir_Path : String) return String;

   --  Get current working directory (uses static string)
   function Get_Working_Directory return String;

   --  Get the directory if the running application (uses static string)
   function Get_Application_Directory return String;

   --  Get filenames in a directory path (memory must be freed)
   -- function Get_Directory_Files (Dir_Path : String; Count : RayLib.Int *) return RayLib.Char **;

   --  Change working directory, return true on success
   function Change_Directory (Dir : String) return Boolean;

   --  Check if a file has been dropped into window
   function Is_File_Dropped return Boolean;

   --  Get dropped files names (memory must be freed)
   -- function Get_Dropped_Files (Count : RayLib.Int *) return RayLib.Char **;

   --  Clear dropped files paths buffer (free memory)
   -- procedure Clear_Dropped_Files;

   --  Get file modification time (last write time)
   function Get_File_Mod_Time (File_Name : String) return Time;

   --  Compress data (DEFLATE algorithm)
   function Compress_Data
     (Data : Stream_Element_Array) return Stream_Element_Array;

   --  Decompress data (DEFLATE algorithm)
   function Decompress_Data
     (Comp_Data : Stream_Element_Array) return Stream_Element_Array;

   --  Encode data to Base64 string
   function Encode_Data_Base64 (Data : Stream_Element_Array) return String;

   --  Decode Base64 string data
   function Decode_Data_Base64 (Data : String) return Stream_Element_Array;

   --  Save integer value to storage file (to defined position), returns true on success
   function Save_Storage_Value
     (Position : Natural; Value : Integer) return Boolean;

   --  Load integer value from storage file (from defined position)
   function Load_Storage_Value (Position : Natural) return Integer;

   --  Open URL with default system browser (if available)
   procedure Open_URL (Url : String);

   --  Check if a key has been pressed once
   function Is_Key_Pressed (Key : Keyboard_Key) return Boolean;

   --  Check if a key is being pressed
   function Is_Key_Down (Key : Keyboard_Key) return Boolean;

   --  Check if a key has been released once
   function Is_Key_Released (Key : Keyboard_Key) return Boolean;

   --  Check if a key is NOT being pressed
   function Is_Key_Up (Key : Keyboard_Key) return Boolean;

   --  Set a custom key to exit program (default is ESC)
   procedure Set_Exit_Key (Key : Keyboard_Key);

   --  Get key pressed (keycode), call it multiple times for keys queued, returns Key_Null when the queue is empty
   function Get_Key_Pressed return Keyboard_Key;

   --  Get char pressed (unicode), call it multiple times for chars queued, returns ASCII.NUL when the queue is empty
   function Get_Char_Pressed return Character;

   --  Check if a gamepad is available
   function Is_Gamepad_Available (Gamepad : Integer) return Boolean;

   --  Get gamepad internal name id
   function Get_Gamepad_Name (Gamepad : Integer) return String;

   --  Check if a gamepad button has been pressed once
   function Is_Gamepad_Button_Pressed
     (Gamepad : Integer; Button : Gamepad_Button) return Boolean;

   --  Check if a gamepad button is being pressed
   function Is_Gamepad_Button_Down
     (Gamepad : Integer; Button : Gamepad_Button) return Boolean;

   --  Check if a gamepad button has been released once
   function Is_Gamepad_Button_Released
     (Gamepad : Integer; Button : Gamepad_Button) return Boolean;

   --  Check if a gamepad button is NOT being pressed
   function Is_Gamepad_Button_Up
     (Gamepad : Integer; Button : Gamepad_Button) return Boolean;

   --  Get the last gamepad button pressed
   function Get_Gamepad_Button_Pressed return Gamepad_Button;

   --  Get gamepad axis count for a gamepad
   function Get_Gamepad_Axis_Count (Gamepad : Integer) return Integer;

   --  Get axis movement value for a gamepad axis
   function Get_Gamepad_Axis_Movement
     (Gamepad : Integer; Axis : Gamepad_Axis) return Float;

   --  Set internal gamepad mappings (SDL_GameControllerDB)
   function Set_Gamepad_Mappings (Mappings : String) return Integer;

   --  Check if a mouse button has been pressed once
   function Is_Mouse_Button_Pressed (Button : Mouse_Button) return Boolean;

   --  Check if a mouse button is being pressed
   function Is_Mouse_Button_Down (Button : Mouse_Button) return Boolean;

   --  Check if a mouse button has been released once
   function Is_Mouse_Button_Released (Button : Mouse_Button) return Boolean;

   --  Check if a mouse button is NOT being pressed
   function Is_Mouse_Button_Up (Button : Mouse_Button) return Boolean;

   --  Get mouse position X
   function Get_Mouse_X return Integer;

   --  Get mouse position Y
   function Get_Mouse_Y return Integer;

   --  Get mouse position XY
   function Get_Mouse_Position return RayLib.Vector2;

   --  Get mouse delta between frames
   function Get_Mouse_Delta return RayLib.Vector2;

   --  Set mouse position XY
   procedure Set_Mouse_Position (X : Integer; Y : Integer);

   --  Set mouse offset
   procedure Set_Mouse_Offset (Offset_X : Integer; Offset_Y : Integer);

   --  Set mouse scaling
   procedure Set_Mouse_Scale (Scale_X : Float; Scale_Y : Float);

   --  Get mouse wheel movement Y
   function Get_Mouse_Wheel_Move return Float;

   --  Set mouse cursor
   procedure Set_Mouse_Cursor (Cursor : Mouse_Cursor);

   --  Get touch position X for touch point 0 (relative to screen size)
   function Get_Touch_X return Integer;

   --  Get touch position Y for touch point 0 (relative to screen size)
   function Get_Touch_Y return Integer;

   --  Get touch position XY for a touch point index (relative to screen size)
   function Get_Touch_Position (Index : Integer) return RayLib.Vector2;

   --  Get touch point identifier for given index
   function Get_Touch_Point_Id (Index : Integer) return Integer;

   --  Get number of touch points
   function Get_Touch_Point_Count return Integer;

   --  Enable a set of gestures using flags
   procedure Set_Gestures_Enabled (Flags : Natural);

   --  Check if a gesture have been detected
   function Is_Gesture_Detected (Gesture : RayLib.Gesture) return Boolean;

   --  Get latest detected gesture
   function Get_Gesture_Detected return RayLib.Gesture;

   --  Get gesture hold time in milliseconds
   function Get_Gesture_Hold_Duration return Float;

   --  Get gesture drag vector
   function Get_Gesture_Drag_Vector return RayLib.Vector2;

   --  Get gesture drag angle
   function Get_Gesture_Drag_Angle return Float;

   --  Get gesture pinch delta
   function Get_Gesture_Pinch_Vector return RayLib.Vector2;

   --  Get gesture pinch angle
   function Get_Gesture_Pinch_Angle return Float;

   --  Set camera mode (multiple camera modes available)
   procedure Set_Camera_Mode (Camera : RayLib.Camera; Mode : Camera_Mode);

   --  Update camera position for selected mode
   procedure Update_Camera (Camera : in out RayLib.Camera);

   --  Set camera pan key to combine with mouse movement (free camera)
   procedure Set_Camera_Pan_Control (Key_Pan : Keyboard_Key);

   --  Set camera alt key to combine with mouse movement (free camera)
   procedure Set_Camera_Alt_Control (Key_Alt : Keyboard_Key);

   --  Set camera smooth zoom key to combine with mouse (free camera)
   procedure Set_Camera_Smooth_Zoom_Control (Key_Smooth_Zoom : Keyboard_Key);

   --  Set camera move controls (1st person and 3rd person cameras)
   procedure Set_Camera_Move_Controls
     (Key_Front : Keyboard_Key; Key_Back : Keyboard_Key;
      Key_Right : Keyboard_Key; Key_Left : Keyboard_Key; Key_Up : Keyboard_Key;
      Key_Down  : Keyboard_Key);

   --  Set texture and rectangle to be used on shapes drawing
   procedure Set_Shapes_Texture
     (Texture : RayLib.Texture2D; Source : RayLib.Rectangle);

   --  Draw a pixel
   procedure Draw_Pixel
     (Pos_X : Integer; Pos_Y : Integer; Color : RayLib.Color);

   --  Draw a pixel (Vector version)
   procedure Draw_Pixel (Position : RayLib.Vector2; Color : RayLib.Color);

   --  Draw a line
   procedure Draw_Line
     (Start_Pos_X : Integer; Start_Pos_Y : Integer; End_Pos_X : Integer;
      End_Pos_Y   : Integer; Color : RayLib.Color);

   --  Draw a line (Vector version)
   procedure Draw_Line
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Color     : RayLib.Color);

   --  Draw a line defining thickness
   procedure Draw_Line
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2; Thick : Float;
      Color     : RayLib.Color);

   --  Draw a line using cubic-bezier curves in-out
   procedure Draw_Line_Bezier
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2; Thick : Float;
      Color     : RayLib.Color);

   --  Draw line using quadratic bezier curves with a control point
   procedure Draw_Line_Bezier_Quad
     (Start_Pos   : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Control_Pos : RayLib.Vector2; Thick : Float; Color : RayLib.Color);

   --  Draw line using cubic bezier curves with 2 control points
   procedure Draw_Line_Bezier_Cubic
     (Start_Pos         : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Start_Control_Pos : RayLib.Vector2; End_Control_Pos : RayLib.Vector2;
      Thick             : Float; Color : RayLib.Color);

   --  Draw lines sequence
   procedure Draw_Line_Strip
     (Points : RayLib.Vector2_Array; Color : RayLib.Color);

   --  Draw a color-filled circle
   procedure Draw_Circle
     (Center_X : Integer; Center_Y : Integer; Radius : Float;
      Color    : RayLib.Color);

   --  Draw a piece of a circle
   procedure Draw_Circle_Sector
     (Center    : RayLib.Vector2; Radius : Float; Start_Angle : Float;
      End_Angle : Float; Segments : Integer; Color : RayLib.Color);

   --  Draw circle sector outline
   procedure Draw_Circle_Sector_Lines
     (Center    : RayLib.Vector2; Radius : Float; Start_Angle : Float;
      End_Angle : Float; Segments : Integer; Color : RayLib.Color);

   --  Draw a gradient-filled circle
   procedure Draw_Circle_Gradient
     (Center_X : Integer; Center_Y : Integer; Radius : Float;
      Color1   : RayLib.Color; Color2 : RayLib.Color);

   --  Draw a color-filled circle (Vector version)
   procedure Draw_Circle
     (Center : RayLib.Vector2; Radius : Float; Color : RayLib.Color);

   --  Draw circle outline
   procedure Draw_Circle_Lines
     (Center_X : Integer; Center_Y : Integer; Radius : Float;
      Color    : RayLib.Color);

   --  Draw ellipse
   procedure Draw_Ellipse
     (Center_X : Integer; Center_Y : Integer; Radius_H : Float;
      Radius_V : Float; Color : RayLib.Color);

   --  Draw ellipse outline
   procedure Draw_Ellipse_Lines
     (Center_X : Integer; Center_Y : Integer; Radius_H : Float;
      Radius_V : Float; Color : RayLib.Color);

   --  Draw ring
   procedure Draw_Ring
     (Center      : RayLib.Vector2; Inner_Radius : Float; Outer_Radius : Float;
      Start_Angle : Float; End_Angle : Float; Segments : Integer;
      Color       : RayLib.Color);

   --  Draw ring outline
   procedure Draw_Ring_Lines
     (Center      : RayLib.Vector2; Inner_Radius : Float; Outer_Radius : Float;
      Start_Angle : Float; End_Angle : Float; Segments : Integer;
      Color       : RayLib.Color);

   --  Draw a color-filled rectangle
   procedure Draw_Rectangle
     (Pos_X : Integer; Pos_Y : Integer; Width : Integer; Height : Integer;
      Color : RayLib.Color);

   --  Draw a color-filled rectangle (Vector version)
   procedure Draw_Rectangle
     (Position : RayLib.Vector2; Size : RayLib.Vector2; Color : RayLib.Color);

   --  Draw a color-filled rectangle
   procedure Draw_Rectangle (Rec : RayLib.Rectangle; Color : RayLib.Color);

   --  Draw a color-filled rectangle with pro parameters
   procedure Draw_Rectangle
     (Rec   : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Color : RayLib.Color);

   --  Draw a vertical-gradient-filled rectangle
   procedure Draw_Rectangle_Gradient_Vertical
     (Pos_X  : Integer; Pos_Y : Integer; Width : Integer; Height : Integer;
      Color1 : RayLib.Color; Color2 : RayLib.Color);

   --  Draw a horizontal-gradient-filled rectangle
   procedure Draw_Rectangle_Gradient_Horizontal
     (Pos_X  : Integer; Pos_Y : Integer; Width : Integer; Height : Integer;
      Color1 : RayLib.Color; Color2 : RayLib.Color);

   --  Draw a gradient-filled rectangle with custom vertex colors
   procedure Draw_Rectangle_Gradient
     (Rec  : RayLib.Rectangle; Col1 : RayLib.Color; Col2 : RayLib.Color;
      Col3 : RayLib.Color; Col4 : RayLib.Color);

   --  Draw rectangle outline
   procedure Draw_Rectangle_Lines
     (Pos_X : Integer; Pos_Y : Integer; Width : Integer; Height : Integer;
      Color : RayLib.Color);

   --  Draw rectangle outline with extended parameters
   procedure Draw_Rectangle_Lines
     (Rec : RayLib.Rectangle; Line_Thick : Float; Color : RayLib.Color);

   --  Draw rectangle with rounded edges
   procedure Draw_Rectangle_Rounded
     (Rec   : RayLib.Rectangle; Roundness : Float; Segments : Integer;
      Color : RayLib.Color);

   --  Draw rectangle with rounded edges outline
   procedure Draw_Rectangle_Rounded_Lines
     (Rec        : RayLib.Rectangle; Roundness : Float; Segments : Integer;
      Line_Thick : Float; Color : RayLib.Color);

   --  Draw a color-filled triangle (vertex in counter-clockwise order!)
   procedure Draw_Triangle
     (V1    : RayLib.Vector2; V2 : RayLib.Vector2; V3 : RayLib.Vector2;
      Color : RayLib.Color);

   --  Draw triangle outline (vertex in counter-clockwise order!)
   procedure Draw_Triangle_Lines
     (V1    : RayLib.Vector2; V2 : RayLib.Vector2; V3 : RayLib.Vector2;
      Color : RayLib.Color);

   --  Draw a triangle fan defined by points (first vertex is the center)
   procedure Draw_Triangle_Fan
     (Points : RayLib.Vector2_Array; Color : RayLib.Color);

   --  Draw a triangle strip defined by points
   procedure Draw_Triangle_Strip
     (Points : RayLib.Vector2_Array; Color : RayLib.Color);

   --  Draw a regular polygon (Vector version)
   procedure Draw_Poly
     (Center   : RayLib.Vector2; Sides : Integer; Radius : Float;
      Rotation : Float; Color : RayLib.Color);

   --  Draw a polygon outline of n sides
   procedure Draw_Poly_Lines
     (Center   : RayLib.Vector2; Sides : Integer; Radius : Float;
      Rotation : Float; Color : RayLib.Color);

   --  Draw a polygon outline of n sides with extended parameters
   procedure Draw_Poly_Lines
     (Center   : RayLib.Vector2; Sides : Integer; Radius : Float;
      Rotation : Float; Line_Thick : Float; Color : RayLib.Color);

   --  Check collision between two rectangles
   function Check_Collision_Recs
     (Rec1 : RayLib.Rectangle; Rec2 : RayLib.Rectangle) return Boolean;

   --  Check collision between two circles
   function Check_Collision_Circles
     (Center1 : RayLib.Vector2; Radius1 : Float; Center2 : RayLib.Vector2;
      Radius2 : Float) return Boolean;

   --  Check collision between circle and rectangle
   function Check_Collision_Circle_Rec
     (Center : RayLib.Vector2; Radius : Float; Rec : RayLib.Rectangle)
      return Boolean;

   --  Check if point is inside rectangle
   function Check_Collision_Point_Rec
     (Point : RayLib.Vector2; Rec : RayLib.Rectangle) return Boolean;

   --  Check if point is inside circle
   function Check_Collision_Point_Circle
     (Point : RayLib.Vector2; Center : RayLib.Vector2; Radius : Float)
      return Boolean;

   --  Check if point is inside a triangle
   function Check_Collision_Point_Triangle
     (Point : RayLib.Vector2; P1 : RayLib.Vector2; P2 : RayLib.Vector2;
      P3    : RayLib.Vector2) return Boolean;

   --  Check the collision between two lines defined by two points each, returns collision point by reference
   function Check_Collision_Lines
     (Start_Pos1      :     RayLib.Vector2; End_Pos1 : RayLib.Vector2;
      Start_Pos2      :     RayLib.Vector2; End_Pos2 : RayLib.Vector2;
      Collision_Point : out RayLib.Vector2) return Boolean;

   --  Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
   function Check_Collision_Point_Line
     (Point     : RayLib.Vector2; P1 : RayLib.Vector2; P2 : RayLib.Vector2;
      Threshold : Integer) return Boolean;

   --  Get collision rectangle for two rectangles collision
   function Get_Collision_Rec
     (Rec1 : RayLib.Rectangle; Rec2 : RayLib.Rectangle)
      return RayLib.Rectangle;

   --  Load image from file into CPU memory (RAM)
   function Load_Image (File_Name : String) return RayLib.Image;

   --  Load image from RAW file data
   function Load_Image_Raw
     (File_Name : String; Width : Integer; Height : Integer; Format : Integer;
      Header_Size : Integer) return RayLib.Image;

   --  Load image sequence from file (frames appended to image.data)
   function Load_Image_Anim
     (File_Name : String; Frames : out Integer) return RayLib.Image;

   --  Load image from memory buffer, fileType refers to extension: i.e. '.png'
   function Load_Image_From_Memory
     (File_Type : String; File_Data : Stream_Element_Array)
      return RayLib.Image;

   --  Load image from GPU texture data
   function Load_Image_From_Texture
     (Texture : RayLib.Texture2D) return RayLib.Image;

   --  Load image from screen buffer and (screenshot)
   function Load_Image_From_Screen return RayLib.Image;

   --  Export image data to file, returns true on success
   function Export_Image
     (Image : RayLib.Image; File_Name : String) return Boolean;

   --  Export image as code file defining an array of bytes, returns true on success
   function Export_Image_As_Code
     (Image : RayLib.Image; File_Name : String) return Boolean;

   --  Generate image: plain color
   function Gen_Image_Color
     (Width : Integer; Height : Integer; Color : RayLib.Color)
      return RayLib.Image;

   --  Generate image: vertical gradient
   function Gen_Image_Gradient_Vertical
     (Width  : Integer; Height : Integer; Top : RayLib.Color;
      Bottom : RayLib.Color) return RayLib.Image;

   --  Generate image: horizontal gradient
   function Gen_Image_Gradient_Horizontal
     (Width : Integer; Height : Integer; Left : RayLib.Color;
      Right : RayLib.Color) return RayLib.Image;

   --  Generate image: radial gradient
   function Gen_Image_Gradient_Radial
     (Width : Integer; Height : Integer; Density : Float; Inner : RayLib.Color;
      Outer : RayLib.Color) return RayLib.Image;

   --  Generate image: checked
   function Gen_Image_Checked
     (Width    : Integer; Height : Integer; Checks_X : Integer;
      Checks_Y : Integer; Col1 : RayLib.Color; Col2 : RayLib.Color)
      return RayLib.Image;

   --  Generate image: white noise
   function Gen_Image_White_Noise
     (Width : Integer; Height : Integer; Factor : Float) return RayLib.Image;

   --  Generate image: cellular algorithm, bigger tileSize means bigger cells
   function Gen_Image_Cellular
     (Width : Integer; Height : Integer; Tile_Size : Integer)
      return RayLib.Image;

   --  Create an image duplicate (useful for transformations)
   function Image_Copy (Image : RayLib.Image) return RayLib.Image;

   --  Create an image from another image piece
   function Image_From_Image
     (Image : RayLib.Image; Rec : RayLib.Rectangle) return RayLib.Image;

   --  Create an image from text (default font)
   function Image_Text
     (Text : String; Font_Size : Integer; Color : RayLib.Color)
      return RayLib.Image;

   --  Create an image from text (custom sprite font)
   function Image_Text
     (Font : RayLib.Font; Text : String; Font_Size : Float; Spacing : Float;
      Tint : RayLib.Color) return RayLib.Image;

   --  Convert image data to desired format
   procedure Image_Format (Image : in out RayLib.Image; New_Format : Integer);

   --  Convert image to POT (power-of-two)
   procedure Image_To_POT (Image : in out RayLib.Image; Fill : RayLib.Color);

   --  Crop an image to a defined rectangle
   procedure Image_Crop (Image : in out RayLib.Image; Crop : RayLib.Rectangle);

   --  Crop image depending on alpha value
   procedure Image_Alpha_Crop (Image : in out RayLib.Image; Threshold : Float);

   --  Clear alpha channel to desired color
   procedure Image_Alpha_Clear
     (Image : in out RayLib.Image; Color : RayLib.Color; Threshold : Float);

   --  Apply alpha mask to image
   procedure Image_Alpha_Mask
     (Image : in out RayLib.Image; Alpha_Mask : RayLib.Image);

   --  Premultiply alpha channel
   procedure Image_Alpha_Premultiply (Image : in out RayLib.Image);

   --  Resize image (Bicubic scaling algorithm)
   procedure Image_Resize
     (Image : in out RayLib.Image; New_Width : Integer; New_Height : Integer);

   --  Resize image (Nearest-Neighbor scaling algorithm)
   procedure Image_Resize_NN
     (Image : in out RayLib.Image; New_Width : Integer; New_Height : Integer);

   --  Resize canvas and fill with color
   procedure Image_Resize_Canvas
     (Image : in out RayLib.Image; New_Width : Integer; New_Height : Integer;
      Offset_X :        Integer; Offset_Y : Integer; Fill : RayLib.Color);

   --  Compute all mipmap levels for a provided image
   procedure Image_Mipmaps (Image : in out RayLib.Image);

   --  Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
   procedure Image_Dither
     (Image : in out RayLib.Image; R_Bpp : Integer; G_Bpp : Integer;
      B_Bpp :        Integer; A_Bpp : Integer);

   --  Flip image vertically
   procedure Image_Flip_Vertical (Image : in out RayLib.Image);

   --  Flip image horizontally
   procedure Image_Flip_Horizontal (Image : in out RayLib.Image);

   --  Rotate image clockwise 90deg
   procedure Image_Rotate_CW (Image : in out RayLib.Image);

   --  Rotate image counter-clockwise 90deg
   procedure Image_Rotate_CCW (Image : in out RayLib.Image);

   --  Modify image color: tint
   procedure Image_Color_Tint
     (Image : in out RayLib.Image; Color : RayLib.Color);

   --  Modify image color: invert
   procedure Image_Color_Invert (Image : in out RayLib.Image);

   --  Modify image color: grayscale
   procedure Image_Color_Grayscale (Image : in out RayLib.Image);

   --  Modify image color: contrast (-100 to 100)
   procedure Image_Color_Contrast
     (Image : in out RayLib.Image; Contrast : Float);

   --  Modify image color: brightness (-255 to 255)
   procedure Image_Color_Brightness
     (Image : in out RayLib.Image; Brightness : Integer);

   --  Modify image color: replace color
   procedure Image_Color_Replace
     (Image   : in out RayLib.Image; Color : RayLib.Color;
      Replace :        RayLib.Color);

   --  Load color data from image as a Color array (RGBA - 32bit)
   function Load_Image_Colors (Image : RayLib.Image) return RayLib.Color_Array;

   --  Load colors palette from image as a Color array (RGBA - 32bit)
   function Load_Image_Palette
     (Image : RayLib.Image; Max_Palette_Size : Integer)
      return RayLib.Color_Array;

   --  Get image alpha border rectangle
   function Get_Image_Alpha_Border
     (Image : RayLib.Image; Threshold : Float) return RayLib.Rectangle;

   --  Get image pixel color at (x, y) position
   function Get_Image_Color
     (Image : RayLib.Image; X : Integer; Y : Integer) return RayLib.Color;

   --  Clear image background with given color
   procedure Image_Clear_Background
     (Dst : in out RayLib.Image; Color : RayLib.Color);

   --  Draw pixel within an image
   procedure Image_Draw_Pixel
     (Dst   : in out RayLib.Image; Pos_X : Integer; Pos_Y : Integer;
      Color :        RayLib.Color);

   --  Draw pixel within an image (Vector version)
   procedure Image_Draw_Pixel
     (Dst   : in out RayLib.Image; Position : RayLib.Vector2;
      Color :        RayLib.Color);

   --  Draw line within an image
   procedure Image_Draw_Line
     (Dst : in out RayLib.Image; Start_Pos_X : Integer; Start_Pos_Y : Integer;
      End_Pos_X :        Integer; End_Pos_Y : Integer; Color : RayLib.Color);

   --  Draw line within an image (Vector version)
   procedure Image_Draw_Line
     (Dst    : in out RayLib.Image; Start : RayLib.Vector2;
      Finish :        RayLib.Vector2; Color : RayLib.Color);

   --  Draw circle within an image
   procedure Image_Draw_Circle
     (Dst    : in out RayLib.Image; Center_X : Integer; Center_Y : Integer;
      Radius :        Integer; Color : RayLib.Color);

   --  Draw circle within an image (Vector version)
   procedure Image_Draw_Circle
     (Dst   : in out RayLib.Image; Center : RayLib.Vector2; Radius : Integer;
      Color :        RayLib.Color);

   --  Draw rectangle within an image
   procedure Image_Draw_Rectangle
     (Dst   : in out RayLib.Image; Pos_X : Integer; Pos_Y : Integer;
      Width :        Integer; Height : Integer; Color : RayLib.Color);

   --  Draw rectangle within an image (Vector version)
   procedure Image_Draw_Rectangle
     (Dst  : in out RayLib.Image; Position : RayLib.Vector2;
      Size :        RayLib.Vector2; Color : RayLib.Color);

   --  Draw rectangle within an image
   procedure Image_Draw_Rectangle
     (Dst : in out RayLib.Image; Rec : RayLib.Rectangle; Color : RayLib.Color);

   --  Draw rectangle lines within an image
   procedure Image_Draw_Rectangle_Lines
     (Dst   : in out RayLib.Image; Rec : RayLib.Rectangle; Thick : Integer;
      Color :        RayLib.Color);

   --  Draw a source image within a destination image (tint applied to source)
   procedure Image_Draw
     (Dst     : in out RayLib.Image; Src : RayLib.Image;
      Src_Rec :        RayLib.Rectangle; Dst_Rec : RayLib.Rectangle;
      Tint    :        RayLib.Color);

   --  Draw text (using default font) within an image (destination)
   procedure Image_Draw_Text
     (Dst   : in out RayLib.Image; Text : String; Pos_X : Integer;
      Pos_Y :        Integer; Font_Size : Integer; Color : RayLib.Color);

   --  Draw text (custom sprite font) within an image (destination)
   procedure Image_Draw_Text
     (Dst      : in out RayLib.Image; Font : RayLib.Font; Text : String;
      Position :        RayLib.Vector2; Font_Size : Float; Spacing : Float;
      Tint     :        RayLib.Color);

   --  Load texture from file into GPU memory (VRAM)
   function Load_Texture (File_Name : String) return RayLib.Texture2D;

   --  Load texture from image data
   function Load_Texture_From_Image
     (Image : RayLib.Image) return RayLib.Texture2D;

   --  Load cubemap from image, multiple image cubemap layouts supported
   function Load_Texture_Cubemap
     (Image : RayLib.Image; Layout : Integer) return RayLib.Texture_Cubemap;

   --  Load texture for rendering (framebuffer)
   function Load_Render_Texture
     (Width : Integer; Height : Integer) return RayLib.Render_Texture2D;

   --  Update GPU texture with new data
   procedure Update_Texture
     (Texture : RayLib.Texture2D; Pixels : Stream_Element_Array);

   --  Update GPU texture rectangle with new data
   procedure Update_Texture
     (Texture : RayLib.Texture2D; Rec : RayLib.Rectangle;
      Pixels  : Stream_Element_Array);

   --  Generate GPU mipmaps for a texture
   procedure Gen_Texture_Mipmaps (Texture : in out RayLib.Texture2D);

   --  Set texture scaling filter mode
   procedure Set_Texture_Filter
     (Texture : RayLib.Texture2D; Filter : Texture_Filter);

   --  Set texture wrapping mode
   procedure Set_Texture_Wrap
     (Texture : RayLib.Texture2D; Wrap : Texture_Wrap);

   --  Draw a Texture2D
   procedure Draw_Texture
     (Texture : RayLib.Texture2D; Pos_X : Integer; Pos_Y : Integer;
      Tint    : RayLib.Color);

   --  Draw a Texture2D with position defined as Vector2
   procedure Draw_Texture
     (Texture : RayLib.Texture2D; Position : RayLib.Vector2;
      Tint    : RayLib.Color);

   --  Draw a Texture2D with extended parameters
   procedure Draw_Texture
     (Texture : RayLib.Texture2D; Position : RayLib.Vector2; Rotation : Float;
      Scale   : Float; Tint : RayLib.Color);

   --  Draw a part of a texture defined by a rectangle
   procedure Draw_Texture
     (Texture  : RayLib.Texture2D; Source : RayLib.Rectangle;
      Position : RayLib.Vector2; Tint : RayLib.Color);

   --  Draw texture quad with tiling and offset parameters
   procedure Draw_Texture_Quad
     (Texture : RayLib.Texture2D; Tiling : RayLib.Vector2;
      Offset  : RayLib.Vector2; Quad : RayLib.Rectangle; Tint : RayLib.Color);

   --  Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
   procedure Draw_Texture_Tiled
     (Texture : RayLib.Texture2D; Source : RayLib.Rectangle;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Scale   : Float; Tint : RayLib.Color);

   --  Draw a part of a texture defined by a rectangle with 'pro' parameters
   procedure Draw_Texture_Pro
     (Texture : RayLib.Texture2D; Source : RayLib.Rectangle;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Tint    : RayLib.Color);

   --  Draws a texture (or part of it) that stretches or shrinks nicely
   procedure Draw_Texture_N_Patch
     (Texture : RayLib.Texture2D; N_Patch_Info : RayLib.N_Patch_Info;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Tint    : RayLib.Color);

   --  Draw a textured polygon
   procedure Draw_Texture_Poly
     (Texture : RayLib.Texture2D; Center : RayLib.Vector2;
      Points  : RayLib.Vector2_Array; Texcoords : RayLib.Vector2_Array;
      Tint    : RayLib.Color);

   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f
   function Fade (Color : RayLib.Color; Alpha : Float) return RayLib.Color;

   --  Get hexadecimal value for a Color
   function Color_To_Int (Color : RayLib.Color) return Integer;

   --  Get Color normalized as float [0..1]
   function Color_Normalize (Color : RayLib.Color) return RayLib.Vector4;

   --  Get Color from normalized values [0..1]
   function Color_From_Normalized
     (Normalized : RayLib.Vector4) return RayLib.Color;

   --  Get HSV values for a Color, hue [0..360], saturation/value [0..1]
   function Color_To_HSV (Color : RayLib.Color) return RayLib.Vector3;

   --  Get a Color from HSV values, hue [0..360], saturation/value [0..1]
   function Color_From_HSV
     (Hue : Float; Saturation : Float; Value : Float) return RayLib.Color;

   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f
   function Color_Alpha
     (Color : RayLib.Color; Alpha : Float) return RayLib.Color;

   --  Get src alpha-blended into dst color with tint
   function Color_Alpha_Blend
     (Dst : RayLib.Color; Src : RayLib.Color; Tint : RayLib.Color)
      return RayLib.Color;

   --  Get Color structure from hexadecimal value
   function Get_Color (Hex_Value : Natural) return RayLib.Color;

   --  Get Color from a source pixel pointer of certain format
   function Get_Pixel_Color
     (Src_Ptr : System.Address; Format : Integer) return RayLib.Color;

   --  Set color formatted into destination pixel pointer
   procedure Set_Pixel_Color
     (Dst_Ptr : System.Address; Color : RayLib.Color; Format : Integer);

   --  Get pixel data size in bytes for certain format
   function Get_Pixel_Data_Size
     (Width : Integer; Height : Integer; Format : Integer) return Integer;

   --  Get the default Font
   function Get_Font_Default return RayLib.Font;

   --  Load font from file into GPU memory (VRAM)
   function Load_Font (File_Name : String) return RayLib.Font;

   --  Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set
   -- function Load_Font_Ex (File_Name : String; Font_Size : Integer; Font_Chars : RayLib.Int *; Glyph_Count : Integer) return RayLib.Font;

   --  Load font from Image (XNA style)
   function Load_Font_From_Image
     (Image : RayLib.Image; Key : RayLib.Color; First_Char : Integer)
      return RayLib.Font;

   --  Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
   -- function Load_Font_From_Memory (File_Type : String; File_Data : RayLib.Const unsigned char *; Data_Size : Integer; Font_Size : Integer; Font_Chars : RayLib.Int *; Glyph_Count : Integer) return RayLib.Font;

   --  Load font data for further use
   -- function Load_Font_Data (File_Data : RayLib.Const unsigned char *; Data_Size : Integer; Font_Size : Integer; Font_Chars : RayLib.Int *; Glyph_Count : Integer; Type : Integer) return RayLib.Glyph_Info *;

   --  Generate image font atlas using chars info
   -- function Gen_Image_Font_Atlas (Chars : RayLib.Const _Glyph_Info *; Recs : RayLib.Rectangle **; Glyph_Count : Integer; Font_Size : Integer; Padding : Integer; Pack_Method : Integer) return RayLib.Image;

   --  Export font as code file, returns true on success
   function Export_Font_As_Code
     (Font : RayLib.Font; File_Name : String) return Boolean;

   --  Draw current FPS
   procedure Draw_FPS (Pos_X : Integer; Pos_Y : Integer);

   --  Draw text (using default font)
   procedure Draw_Text
     (Text  : String; Pos_X : Integer; Pos_Y : Integer; Font_Size : Integer;
      Color : RayLib.Color);

   --  Draw text using font and additional parameters
   procedure Draw_Text_Ex
     (Font      : RayLib.Font; Text : String; Position : RayLib.Vector2;
      Font_Size : Float; Spacing : Float; Tint : RayLib.Color);

   --  Draw text using Font and pro parameters (rotation)
   procedure Draw_Text_Pro
     (Font    : RayLib.Font; Text : String; Position : RayLib.Vector2;
      Origin  : RayLib.Vector2; Rotation : Float; Font_Size : Float;
      Spacing : Float; Tint : RayLib.Color);

   --  Draw one character (codepoint)
   procedure Draw_Text_Codepoint
     (Font      : RayLib.Font; Codepoint : Integer; Position : RayLib.Vector2;
      Font_Size : Float; Tint : RayLib.Color);

   --  Draw multiple character (codepoint)
   procedure Draw_Text_Codepoints
     (Font     : RayLib.Font; Codepoints : Integer_Array;
      Position : RayLib.Vector2; Font_Size : Float; Spacing : Float;
      Tint     : RayLib.Color);

   --  Measure string width for default font
   function Measure_Text (Text : String; Font_Size : Integer) return Integer;

   --  Measure string size for Font
   function Measure_Text_Ex
     (Font : RayLib.Font; Text : String; Font_Size : Float; Spacing : Float)
      return RayLib.Vector2;

   --  Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
   function Get_Glyph_Index
     (Font : RayLib.Font; Codepoint : Integer) return Integer;

   --  Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
   function Get_Glyph_Info
     (Font : RayLib.Font; Codepoint : Integer) return RayLib.Glyph_Info;

   --  Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
   function Get_Glyph_Atlas_Rec
     (Font : RayLib.Font; Codepoint : Integer) return RayLib.Rectangle;

   --  Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
   function Load_Codepoints
     (Text : String; Count : out Integer) return Integer_Array;

   --  Get total number of codepoints in a UTF-8 encoded string
   function Get_Codepoint_Count (Text : String) return Integer;

   --  Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   function Get_Codepoint
     (Text : String; Bytes_Processed : out Integer) return Integer;

   --  Encode one codepoint into UTF-8 byte array (array length returned as parameter)
   function Codepoint_To_UTF8 (Codepoint : Integer) return String;

   --  Encode text as codepoints array into UTF-8 text string (WARNING: memory must be freed!)
   function Text_Codepoints_To_UTF8 (Codepoints : Integer_Array) return String;

   --  Copy one string to another, returns bytes copied
   procedure Text_Copy (Dst : out String; Src : String);

   --  Check if two text string are equal
   function Text_Is_Equal (Text1 : String; Text2 : String) return Boolean;

   --  Get text length, checks for '\0' ending
   function Text_Length (Text : String) return Natural;

   --  Text formatting with variables (sprintf() style)
   -- function Text_Format (Text : String; Args : RayLib....) return String;

   --  Get a piece of a text string
   function Text_Subtext
     (Text : String; Position : Integer; Length : Integer) return String;

   --  Replace text string (WARNING: memory must be freed!)
   function Text_Replace
     (Text : in out String; Replace : String; By : String) return String;

   --  Insert text in a position (WARNING: memory must be freed!)
   function Text_Insert
     (Text : String; Insert : String; Position : Integer) return String;

   --  Join text strings with delimiter
   -- function Text_Join (Text_List : RayLib.Const char **; Count : Integer; Delimiter : String) return String;

   --  Split text into multiple strings
   -- function Text_Split (Text : String; Delimiter : RayLib.Char; Count : RayLib.Int *) return RayLib.Const char **;

   --  Append text at specific position and move cursor!
   -- procedure Text_Append (Text : RayLib.Char *; Append : String; Position : RayLib.Int *);

   --  Find first text occurrence within a string
   function Text_Find_Index (Text : String; Find : String) return Integer;

   --  Get upper case version of provided string
   function Text_To_Upper (Text : String) return String;

   --  Get lower case version of provided string
   function Text_To_Lower (Text : String) return String;

   --  Get Pascal case notation version of provided string
   function Text_To_Pascal (Text : String) return String;

   --  Get integer value from text (negative values not supported)
   function Text_To_Integer (Text : String) return Integer;

   --  Draw a line in 3D world space
   procedure Draw_Line3D
     (Start_Pos : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Color     : RayLib.Color);

   --  Draw a point in 3D space, actually a small line
   procedure Draw_Point3D (Position : RayLib.Vector3; Color : RayLib.Color);

   --  Draw a circle in 3D world space
   procedure Draw_Circle3D
     (Center : RayLib.Vector3; Radius : Float; Rotation_Axis : RayLib.Vector3;
      Rotation_Angle : Float; Color : RayLib.Color);

   --  Draw a color-filled triangle (vertex in counter-clockwise order!)
   procedure Draw_Triangle3D
     (V1    : RayLib.Vector3; V2 : RayLib.Vector3; V3 : RayLib.Vector3;
      Color : RayLib.Color);

   --  Draw a triangle strip defined by points
   procedure Draw_Triangle_Strip3D
     (Points : Vector3_Array; Color : RayLib.Color);

   --  Draw cube
   procedure Draw_Cube
     (Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color);

   --  Draw cube (Vector version)
   procedure Draw_Cube_V
     (Position : RayLib.Vector3; Size : RayLib.Vector3; Color : RayLib.Color);

   --  Draw cube wires
   procedure Draw_Cube_Wires
     (Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color);

   --  Draw cube wires (Vector version)
   procedure Draw_Cube_Wires_V
     (Position : RayLib.Vector3; Size : RayLib.Vector3; Color : RayLib.Color);

   --  Draw cube textured
   procedure Draw_Cube_Texture
     (Texture : RayLib.Texture2D; Position : RayLib.Vector3; Width : Float;
      Height  : Float; Length : Float; Color : RayLib.Color);

   --  Draw cube with a region of a texture
   procedure Draw_Cube_Texture_Rec
     (Texture  : RayLib.Texture2D; Source : RayLib.Rectangle;
      Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color);

   --  Draw sphere
   procedure Draw_Sphere
     (Center_Pos : RayLib.Vector3; Radius : Float; Color : RayLib.Color);

   --  Draw sphere with extended parameters
   procedure Draw_Sphere_Ex
     (Center_Pos : RayLib.Vector3; Radius : Float; Rings : Integer;
      Slices     : Integer; Color : RayLib.Color);

   --  Draw sphere wires
   procedure Draw_Sphere_Wires
     (Center_Pos : RayLib.Vector3; Radius : Float; Rings : Integer;
      Slices     : Integer; Color : RayLib.Color);

   --  Draw a cylinder/cone
   procedure Draw_Cylinder
     (Position : RayLib.Vector3; Radius_Top : Float; Radius_Bottom : Float;
      Height   : Float; Slices : Integer; Color : RayLib.Color);

   --  Draw a cylinder with base at startPos and top at endPos
   procedure Draw_Cylinder_Ex
     (Start_Pos    : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Start_Radius : Float; End_Radius : Float; Sides : Integer;
      Color        : RayLib.Color);

   --  Draw a cylinder/cone wires
   procedure Draw_Cylinder_Wires
     (Position : RayLib.Vector3; Radius_Top : Float; Radius_Bottom : Float;
      Height   : Float; Slices : Integer; Color : RayLib.Color);

   --  Draw a cylinder wires with base at startPos and top at endPos
   procedure Draw_Cylinder_Wires_Ex
     (Start_Pos    : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Start_Radius : Float; End_Radius : Float; Sides : Integer;
      Color        : RayLib.Color);

   --  Draw a plane XZ
   procedure Draw_Plane
     (Center_Pos : RayLib.Vector3; Size : RayLib.Vector2;
      Color      : RayLib.Color);

   --  Draw a ray line
   procedure Draw_Ray (Ray : RayLib.Ray; Color : RayLib.Color);

   --  Draw a grid (centered at (0, 0, 0))
   procedure Draw_Grid (Slices : Integer; Spacing : Float);

   --  Load model from files (meshes and materials)
   function Load_Model (File_Name : String) return RayLib.Model;

   --  Load model from generated mesh (default material)
   function Load_Model_From_Mesh (Mesh : RayLib.Mesh) return RayLib.Model;

   --  Unload model (including meshes) from memory (RAM and/or VRAM)
   procedure Unload_Model (Model : RayLib.Model);

   --  Unload model (but not meshes) from memory (RAM and/or VRAM)
   procedure Unload_Model_Keep_Meshes (Model : RayLib.Model);

   --  Compute model bounding box limits (considers all meshes)
   function Get_Model_Bounding_Box
     (Model : RayLib.Model) return RayLib.Bounding_Box;

   --  Draw a model (with texture if set)
   procedure Draw_Model
     (Model : RayLib.Model; Position : RayLib.Vector3; Scale : Float;
      Tint  : RayLib.Color);

   --  Draw a model with extended parameters
   procedure Draw_Model_Ex
     (Model         : RayLib.Model; Position : RayLib.Vector3;
      Rotation_Axis : RayLib.Vector3; Rotation_Angle : Float;
      Scale         : RayLib.Vector3; Tint : RayLib.Color);

   --  Draw a model wires (with texture if set)
   procedure Draw_Model_Wires
     (Model : RayLib.Model; Position : RayLib.Vector3; Scale : Float;
      Tint  : RayLib.Color);

   --  Draw a model wires (with texture if set) with extended parameters
   procedure Draw_Model_Wires_Ex
     (Model         : RayLib.Model; Position : RayLib.Vector3;
      Rotation_Axis : RayLib.Vector3; Rotation_Angle : Float;
      Scale         : RayLib.Vector3; Tint : RayLib.Color);

   --  Draw bounding box (wires)
   procedure Draw_Bounding_Box
     (Box : RayLib.Bounding_Box; Color : RayLib.Color);

   --  Draw a billboard texture
   procedure Draw_Billboard
     (Camera   : RayLib.Camera; Texture : RayLib.Texture2D;
      Position : RayLib.Vector3; Size : Float; Tint : RayLib.Color);

   --  Draw a billboard texture defined by source
   procedure Draw_Billboard_Rec
     (Camera : RayLib.Camera; Texture : RayLib.Texture2D;
      Source : RayLib.Rectangle; Position : RayLib.Vector3;
      Size   : RayLib.Vector2; Tint : RayLib.Color);

   --  Draw a billboard texture defined by source and rotation
   procedure Draw_Billboard_Pro
     (Camera   : RayLib.Camera; Texture : RayLib.Texture2D;
      Source   : RayLib.Rectangle; Position : RayLib.Vector3;
      Up : RayLib.Vector3; Size : RayLib.Vector2; Origin : RayLib.Vector2;
      Rotation : Float; Tint : RayLib.Color);

   --  Upload mesh vertex data in GPU and provide VAO/VBO ids
   procedure Upload_Mesh (Mesh : in out RayLib.Mesh; Dynamic : Boolean);

   --  Update mesh vertex data in GPU for a specific buffer index
   procedure Update_Mesh_Buffer
     (Mesh : RayLib.Mesh; Index : Integer; Data : Stream_Element_Array);

   --  Unload mesh data from CPU and GPU
   procedure Unload_Mesh (Mesh : RayLib.Mesh);

   --  Draw a 3d mesh with material and transform
   procedure Draw_Mesh
     (Mesh      : RayLib.Mesh; Material : RayLib.Material;
      Transform : RayLib.Matrix);

   --  Draw multiple mesh instances with material and different transforms
   procedure Draw_Mesh_Instanced
     (Mesh       : RayLib.Mesh; Material : RayLib.Material;
      Transforms : Transform_Array; Instances : Integer);

   --  Export mesh data to file, returns true on success
   function Export_Mesh
     (Mesh : RayLib.Mesh; File_Name : String) return Boolean;

   --  Compute mesh bounding box limits
   function Get_Mesh_Bounding_Box
     (Mesh : RayLib.Mesh) return RayLib.Bounding_Box;

   --  Compute mesh tangents
   procedure Gen_Mesh_Tangents (Mesh : in out RayLib.Mesh);

   --  Compute mesh binormals
   procedure Gen_Mesh_Binormals (Mesh : in out RayLib.Mesh);

   --  Generate polygonal mesh
   function Gen_Mesh_Poly (Sides : Integer; Radius : Float) return RayLib.Mesh;

   --  Generate plane mesh (with subdivisions)
   function Gen_Mesh_Plane
     (Width : Float; Length : Float; Res_X : Integer; Res_Z : Integer)
      return RayLib.Mesh;

   --  Generate cuboid mesh
   function Gen_Mesh_Cube
     (Width : Float; Height : Float; Length : Float) return RayLib.Mesh;

   --  Generate sphere mesh (standard sphere)
   function Gen_Mesh_Sphere
     (Radius : Float; Rings : Integer; Slices : Integer) return RayLib.Mesh;

   --  Generate half-sphere mesh (no bottom cap)
   function Gen_Mesh_Hemi_Sphere
     (Radius : Float; Rings : Integer; Slices : Integer) return RayLib.Mesh;

   --  Generate cylinder mesh
   function Gen_Mesh_Cylinder
     (Radius : Float; Height : Float; Slices : Integer) return RayLib.Mesh;

   --  Generate cone/pyramid mesh
   function Gen_Mesh_Cone
     (Radius : Float; Height : Float; Slices : Integer) return RayLib.Mesh;

   --  Generate torus mesh
   function Gen_Mesh_Torus
     (Radius : Float; Size : Float; Rad_Seg : Integer; Sides : Integer)
      return RayLib.Mesh;

   --  Generate trefoil knot mesh
   function Gen_Mesh_Knot
     (Radius : Float; Size : Float; Rad_Seg : Integer; Sides : Integer)
      return RayLib.Mesh;

   --  Generate heightmap mesh from image data
   function Gen_Mesh_Heightmap
     (Heightmap : RayLib.Image; Size : RayLib.Vector3) return RayLib.Mesh;

   --  Generate cubes-based map mesh from image data
   function Gen_Mesh_Cubicmap
     (Cubicmap : RayLib.Image; Cube_Size : RayLib.Vector3) return RayLib.Mesh;

   --  Load materials from model file
   function Load_Materials (File_Name : String) return RayLib.Material_Array;

   --  Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
   function Load_Material_Default return RayLib.Material;

   --  Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
   procedure Set_Material_Texture
     (Material : in out RayLib.Material; Map_Type : Integer;
      Texture  :        RayLib.Texture2D);

   --  Set material for a mesh
   procedure Set_Model_Mesh_Material
     (Model : in out RayLib.Model; Mesh_Id : Integer; Material_Id : Integer);

   --  Load model animations from file
   function Load_Model_Animations
     (File_Name : String) return RayLib.Model_Animation_Array;

   --  Update model animation pose
   procedure Update_Model_Animation
     (Model : RayLib.Model; Anim : RayLib.Model_Animation; Frame : Integer);

   --  Check model animation skeleton match
   function Is_Model_Animation_Valid
     (Model : RayLib.Model; Anim : RayLib.Model_Animation) return Boolean;

   --  Check collision between two spheres
   function Check_Collision_Spheres
     (Center1 : RayLib.Vector3; Radius1 : Float; Center2 : RayLib.Vector3;
      Radius2 : Float) return Boolean;

   --  Check collision between two bounding boxes
   function Check_Collision_Boxes
     (Box1 : RayLib.Bounding_Box; Box2 : RayLib.Bounding_Box) return Boolean;

   --  Check collision between box and sphere
   function Check_Collision_Box_Sphere
     (Box : RayLib.Bounding_Box; Center : RayLib.Vector3; Radius : Float)
      return Boolean;

   --  Get collision info between ray and sphere
   function Get_Ray_Collision_Sphere
     (Ray : RayLib.Ray; Center : RayLib.Vector3; Radius : Float)
      return RayLib.Ray_Collision;

   --  Get collision info between ray and box
   function Get_Ray_Collision_Box
     (Ray : RayLib.Ray; Box : RayLib.Bounding_Box) return RayLib.Ray_Collision;

   --  Get collision info between ray and mesh
   function Get_Ray_Collision_Mesh
     (Ray : RayLib.Ray; Mesh : RayLib.Mesh; Transform : RayLib.Matrix)
      return RayLib.Ray_Collision;

   --  Get collision info between ray and triangle
   function Get_Ray_Collision_Triangle
     (Ray : RayLib.Ray; P1 : RayLib.Vector3; P2 : RayLib.Vector3;
      P3  : RayLib.Vector3) return RayLib.Ray_Collision;

   --  Get collision info between ray and quad
   function Get_Ray_Collision_Quad
     (Ray : RayLib.Ray; P1 : RayLib.Vector3; P2 : RayLib.Vector3;
      P3  : RayLib.Vector3; P4 : RayLib.Vector3) return RayLib.Ray_Collision;

   --  Initialize audio device and context
   procedure Init_Audio_Device;

   --  Close the audio device and context
   procedure Close_Audio_Device;

   --  Check if audio device has been initialized successfully
   function Is_Audio_Device_Ready return Boolean;

   --  Set master volume (listener)
   procedure Set_Master_Volume (Volume : Float);

   --  Load wave data from file
   function Load_Wave (File_Name : String) return RayLib.Wave;

   --  Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
   function Load_Wave_From_Memory
     (File_Type : String; File_Data : Stream_Element_Array) return RayLib.Wave;

   --  Load sound from file
   function Load_Sound (File_Name : String) return RayLib.Sound;

   --  Load sound from wave data
   function Load_Sound_From_Wave (Wave : RayLib.Wave) return RayLib.Sound;

   --  Update sound buffer with new data
   procedure Update_Sound
     (Sound        : RayLib.Sound; Data : Stream_Element_Array;
      Sample_Count : Integer);

   --  Unload wave data
   procedure Unload_Wave (Wave : RayLib.Wave);

   --  Unload sound
   procedure Unload_Sound (Sound : RayLib.Sound);

   --  Export wave data to file, returns true on success
   function Export_Wave
     (Wave : RayLib.Wave; File_Name : String) return Boolean;

   --  Export wave sample data to code (.h), returns true on success
   function Export_Wave_As_Code
     (Wave : RayLib.Wave; File_Name : String) return Boolean;

   --  Play a sound
   procedure Play_Sound (Sound : RayLib.Sound);

   --  Stop playing a sound
   procedure Stop_Sound (Sound : RayLib.Sound);

   --  Pause a sound
   procedure Pause_Sound (Sound : RayLib.Sound);

   --  Resume a paused sound
   procedure Resume_Sound (Sound : RayLib.Sound);

   --  Play a sound (using multichannel buffer pool)
   procedure Play_Sound_Multi (Sound : RayLib.Sound);

   --  Stop any sound playing (using multichannel buffer pool)
   procedure Stop_Sound_Multi;

   --  Get number of sounds playing in the multichannel
   function Get_Sounds_Playing return Integer;

   --  Check if a sound is currently playing
   function Is_Sound_Playing (Sound : RayLib.Sound) return Boolean;

   --  Set volume for a sound (1.0 is max level)
   procedure Set_Sound_Volume (Sound : RayLib.Sound; Volume : Float);

   --  Set pitch for a sound (1.0 is base level)
   procedure Set_Sound_Pitch (Sound : RayLib.Sound; Pitch : Float);

   --  Set pan for a sound (0.5 is center)
   procedure Set_Sound_Pan (Sound : RayLib.Sound; Pan : Float);

   --  Copy a wave to a new wave
   function Wave_Copy (Wave : RayLib.Wave) return RayLib.Wave;

   --  Crop a wave to defined samples range
   procedure Wave_Crop
     (Wave         : in out RayLib.Wave; Init_Sample : Integer;
      Final_Sample :        Integer);

   --  Convert wave data to desired format
   procedure Wave_Format
     (Wave : in out RayLib.Wave; Sample_Rate : Integer; Sample_Size : Integer;
      Channels :        Integer);

   --  Load samples data from wave as a 32bit float data array
   function Load_Wave_Samples (Wave : RayLib.Wave) return RayLib.Float_Array;

   --  Load music stream from file
   function Load_Music_Stream (File_Name : String) return RayLib.Music;

   --  Load music stream from data
   function Load_Music_Stream_From_Memory
     (File_Type : String; Data : Stream_Element_Array) return RayLib.Music;

   --  Unload music stream
   procedure Unload_Music_Stream (Music : RayLib.Music);

   --  Start music playing
   procedure Play_Music_Stream (Music : RayLib.Music);

   --  Check if music is playing
   function Is_Music_Stream_Playing (Music : RayLib.Music) return Boolean;

   --  Updates buffers for music streaming
   procedure Update_Music_Stream (Music : RayLib.Music);

   --  Stop music playing
   procedure Stop_Music_Stream (Music : RayLib.Music);

   --  Pause music playing
   procedure Pause_Music_Stream (Music : RayLib.Music);

   --  Resume playing paused music
   procedure Resume_Music_Stream (Music : RayLib.Music);

   --  Seek music to a position (in seconds)
   procedure Seek_Music_Stream (Music : RayLib.Music; Position : Float);

   --  Set volume for music (1.0 is max level)
   procedure Set_Music_Volume (Music : RayLib.Music; Volume : Float);

   --  Set pitch for a music (1.0 is base level)
   procedure Set_Music_Pitch (Music : RayLib.Music; Pitch : Float);

   --  Set pan for a music (0.5 is center)
   procedure Set_Music_Pan (Music : RayLib.Music; Pan : Float);

   --  Get music time length (in seconds)
   function Get_Music_Time_Length (Music : RayLib.Music) return Float;

   --  Get current music time played (in seconds)
   function Get_Music_Time_Played (Music : RayLib.Music) return Float;

   --  Load audio stream (to stream raw audio pcm data)
   function Load_Audio_Stream
     (Sample_Rate : Natural; Sample_Size : Natural; Channels : Natural)
      return RayLib.Audio_Stream;

   --  Unload audio stream and free memory
   procedure Unload_Audio_Stream (Stream : RayLib.Audio_Stream);

   --  Update audio stream buffers with data
   procedure Update_Audio_Stream
     (Stream      : RayLib.Audio_Stream; Data : Stream_Element_Array;
      Frame_Count : Integer);

   --  Check if any audio stream buffers requires refill
   function Is_Audio_Stream_Processed
     (Stream : RayLib.Audio_Stream) return Boolean;

   --  Play audio stream
   procedure Play_Audio_Stream (Stream : RayLib.Audio_Stream);

   --  Pause audio stream
   procedure Pause_Audio_Stream (Stream : RayLib.Audio_Stream);

   --  Resume audio stream
   procedure Resume_Audio_Stream (Stream : RayLib.Audio_Stream);

   --  Check if audio stream is playing
   function Is_Audio_Stream_Playing
     (Stream : RayLib.Audio_Stream) return Boolean;

   --  Stop audio stream
   procedure Stop_Audio_Stream (Stream : RayLib.Audio_Stream);

   --  Set volume for audio stream (1.0 is max level)
   procedure Set_Audio_Stream_Volume
     (Stream : RayLib.Audio_Stream; Volume : Float);

   --  Set pitch for audio stream (1.0 is base level)
   procedure Set_Audio_Stream_Pitch
     (Stream : RayLib.Audio_Stream; Pitch : Float);

   --  Set pan for audio stream (0.5 is centered)
   procedure Set_Audio_Stream_Pan (Stream : RayLib.Audio_Stream; Pan : Float);

   --  Default size for new audio streams
   procedure Set_Audio_Stream_Buffer_Size_Default (Size : Integer);
end RayLib;

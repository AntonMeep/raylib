with Ada.Streams;           use Ada.Streams;
with Ada.Calendar;          use Ada.Calendar;
with Ada.Finalization;
with Ada.Numerics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces.C;
with System;

package RayLib is
   ------------------------------
   --  Basic defines
   ------------------------------

   Max_Material_Maps : constant := 12;
   --  TODO: move to global configuration
   Max_Mesh_Vertex_Buffers : constant := 7;
   --  TODO: move to global configuration
   Rl_Max_Shader_Locations : constant := 32;
   --  TODO: move to global configuration

   Version : constant String := "4.0";
   --  Version of the underlying raylib

   Pi : constant := Ada.Numerics.Pi;
   --  Convenience Pi constant

   DEG2RAD : constant := Pi / 180.0;
   --  Provded only for compatibility with raylib.h

   RAD2DEG : constant := 180.0 / Pi;
   --  Provded only for compatibility with raylib.h

   type OpenGL_Id is new Interfaces.C.unsigned;

   type OpenGL_Id_Array is array (Natural range <>) of OpenGL_Id;

   type Float_Array is array (Natural range <>) of Float;

   type Integer_Array is array (Natural range <>) of Integer;

   type Unbounded_String_Array is array (Natural range <>) of Unbounded_String;

   type Color_Component is mod 256;
   --  Single 8-bit color component
   for Color_Component'Size use 8;

   type Monitor_Id is new Natural;
   --  An Id of a connected monitor

   type Gamepad_Id is new Natural;
   --  An Id of a connected gamepad

   ------------------------------
   --  Enumerations definition
   ------------------------------

   type Config_Flag is new Interfaces.C.unsigned;
   --  System/Window config flags

   Flag_Vsync_Hint : constant Config_Flag := 16#0000_0040#;
   --  Set to try enabling V-Sync on GPU

   Flag_Fullscreen_Mode : constant Config_Flag := 16#0000_0002#;
   --  Set to run program in fullscreen

   Flag_Window_Resizable : constant Config_Flag := 16#0000_0004#;
   --  Set to allow resizable window

   Flag_Window_Undecorated : constant Config_Flag := 16#0000_0008#;
   --  Set to disable window decoration (frame and buttons)

   Flag_Window_Hidden : constant Config_Flag := 16#0000_0080#;
   --  Set to hide window

   Flag_Window_Minimized : constant Config_Flag := 16#0000_0200#;
   --  Set to minimize window (iconify)

   Flag_Window_Maximized : constant Config_Flag := 16#0000_0400#;
   --  Set to maximize window (expanded to monitor)

   Flag_Window_Unfocused : constant Config_Flag := 16#0000_0800#;
   --  Set to window non focused
   Flag_Window_Topmost : constant Config_Flag := 16#0000_1000#;
   --  Set to window always on top

   Flag_Window_Always_Run : constant Config_Flag := 16#0000_0100#;
   --  Set to allow windows running while minimized

   Flag_Window_Transparent : constant Config_Flag := 16#0000_0010#;
   --  Set to allow transparent framebuffer

   Flag_Window_HighDPI : constant Config_Flag := 16#0000_2000#;
   --  Set to support HighDPI

   Flag_MSAA_4x_Hint : constant Config_Flag := 16#0000_0020#;
   --  Set to try enabling MSAA 4X

   Flag_Interlaced_Hint : constant Config_Flag := 16#0001_0000#;
   --  Set to try enabling interlaced video format (for V3D)

   type Trace_Log_Level is new Interfaces.C.unsigned;
   --  Trace log level

   Log_All : constant Trace_Log_Level := 0;
   --  Display all logs

   Log_Trace : constant Trace_Log_Level := 1;
   --  Trace logging, intended for internal use only

   Log_Debug : constant Trace_Log_Level := 2;
   --  Debug logging, used for internal debugging,
   --  it should be disabled on release builds

   Log_Info : constant Trace_Log_Level := 3;
   --  Info logging, used for program execution info

   Log_Warning : constant Trace_Log_Level := 4;
   --  Warning logging, used on recoverable failures

   Log_Error : constant Trace_Log_Level := 5;
   --  Error logging, used on unrecoverable failures

   Log_Fatal : constant Trace_Log_Level := 6;
   --  Fatal logging, used to abort program: exit(EXIT_FAILURE)

   Log_None : constant Trace_Log_Level := 7;
   --  Disable logging

   type Keyboard_Key is new Interfaces.C.unsigned;
   --  Keyboard keys (US keyboard layout)

   Key_Null : constant Keyboard_Key := 0;
   --  Key: NULL, used for no key pressed

   Key_Apostrophe : constant Keyboard_Key := 39;
   --  Key: '

   Key_Comma : constant Keyboard_Key := 44;
   --  Key: ,

   Key_Minus : constant Keyboard_Key := 45;
   --  Key: -

   Key_Period : constant Keyboard_Key := 46;
   --  Key: .

   Key_Slash : constant Keyboard_Key := 47;
   --  Key: /

   Key_Zero : constant Keyboard_Key := 48;
   --  Key: 0

   Key_One : constant Keyboard_Key := 49;
   --  Key: 1

   Key_Two : constant Keyboard_Key := 50;
   --  Key: 2

   Key_Three : constant Keyboard_Key := 51;
   --  Key: 3

   Key_Four : constant Keyboard_Key := 52;
   --  Key: 4

   Key_Five : constant Keyboard_Key := 53;
   --  Key: 5

   Key_Six : constant Keyboard_Key := 54;
   --  Key: 6

   Key_Seven : constant Keyboard_Key := 55;
   --  Key: 7

   Key_Eight : constant Keyboard_Key := 56;
   --  Key: 8

   Key_Nine : constant Keyboard_Key := 57;
   --  Key: 9

   Key_Semicolon : constant Keyboard_Key := 59;
   --  Key: ;

   Key_Equal : constant Keyboard_Key := 61;
   --  Key: =

   Key_A : constant Keyboard_Key := 65;
   --  Key: A | a

   Key_B : constant Keyboard_Key := 66;
   --  Key: B | b

   Key_C : constant Keyboard_Key := 67;
   --  Key: C | c

   Key_D : constant Keyboard_Key := 68;
   --  Key: D | d

   Key_E : constant Keyboard_Key := 69;
   --  Key: E | e

   Key_F : constant Keyboard_Key := 70;
   --  Key: F | f

   Key_G : constant Keyboard_Key := 71;
   --  Key: G | g

   Key_H : constant Keyboard_Key := 72;
   --  Key: H | h

   Key_I : constant Keyboard_Key := 73;
   --  Key: I | i

   Key_J : constant Keyboard_Key := 74;
   --  Key: J | j

   Key_K : constant Keyboard_Key := 75;
   --  Key: K | k

   Key_L : constant Keyboard_Key := 76;
   --  Key: L | l

   Key_M : constant Keyboard_Key := 77;
   --  Key: M | m

   Key_N : constant Keyboard_Key := 78;
   --  Key: N | n

   Key_O : constant Keyboard_Key := 79;
   --  Key: O | o

   Key_P : constant Keyboard_Key := 80;
   --  Key: P | p

   Key_Q : constant Keyboard_Key := 81;
   --  Key: Q | q

   Key_R : constant Keyboard_Key := 82;
   --  Key: R | r

   Key_S : constant Keyboard_Key := 83;
   --  Key: S | s

   Key_T : constant Keyboard_Key := 84;
   --  Key: T | t

   Key_U : constant Keyboard_Key := 85;
   --  Key: U | u

   Key_V : constant Keyboard_Key := 86;
   --  Key: V | v

   Key_W : constant Keyboard_Key := 87;
   --  Key: W | w

   Key_X : constant Keyboard_Key := 88;
   --  Key: X | x

   Key_Y : constant Keyboard_Key := 89;
   --  Key: Y | y

   Key_Z : constant Keyboard_Key := 90;
   --  Key: Z | z

   Key_Left_Bracket : constant Keyboard_Key := 91;
   --  Key: [

   Key_Backslash : constant Keyboard_Key := 92;
   --  Key: '\'

   Key_Right_Bracket : constant Keyboard_Key := 93;
   --  Key: ]

   Key_Grave : constant Keyboard_Key := 96;
   --  Key: `

   Key_Space : constant Keyboard_Key := 32;
   --  Key: Space

   Key_Escape : constant Keyboard_Key := 256;
   --  Key: Esc

   Key_Enter : constant Keyboard_Key := 257;
   --  Key: Enter

   Key_Tab : constant Keyboard_Key := 258;
   --  Key: Tab

   Key_Backspace : constant Keyboard_Key := 259;
   --  Key: Backspace

   Key_Insert : constant Keyboard_Key := 260;
   --  Key: Ins

   Key_Delete : constant Keyboard_Key := 261;
   --  Key: Del

   Key_Right : constant Keyboard_Key := 262;
   --  Key: Cursor right

   Key_Left : constant Keyboard_Key := 263;
   --  Key: Cursor left

   Key_Down : constant Keyboard_Key := 264;
   --  Key: Cursor down

   Key_Up : constant Keyboard_Key := 265;
   --  Key: Cursor up

   Key_Page_Up : constant Keyboard_Key := 266;
   --  Key: Page up

   Key_Page_Down : constant Keyboard_Key := 267;
   --  Key: Page down

   Key_Home : constant Keyboard_Key := 268;
   --  Key: Home

   Key_End : constant Keyboard_Key := 269;
   --  Key: End

   Key_Caps_Lock : constant Keyboard_Key := 280;
   --  Key: Caps lock

   Key_Scroll_Lock : constant Keyboard_Key := 281;
   --  Key: Scroll down

   Key_Num_Lock : constant Keyboard_Key := 282;
   --  Key: Num lock

   Key_Print_Screen : constant Keyboard_Key := 283;
   --  Key: Print screen

   Key_Pause : constant Keyboard_Key := 284;
   --  Key: Pause

   Key_F1 : constant Keyboard_Key := 290;
   --  Key: F1

   Key_F2 : constant Keyboard_Key := 291;
   --  Key: F2

   Key_F3 : constant Keyboard_Key := 292;
   --  Key: F3

   Key_F4 : constant Keyboard_Key := 293;
   --  Key: F4

   Key_F5 : constant Keyboard_Key := 294;
   --  Key: F5

   Key_F6 : constant Keyboard_Key := 295;
   --  Key: F6

   Key_F7 : constant Keyboard_Key := 296;
   --  Key: F7

   Key_F8 : constant Keyboard_Key := 297;
   --  Key: F8

   Key_F9 : constant Keyboard_Key := 298;
   --  Key: F9

   Key_F10 : constant Keyboard_Key := 299;
   --  Key: F10

   Key_F11 : constant Keyboard_Key := 300;
   --  Key: F11

   Key_F12 : constant Keyboard_Key := 301;
   --  Key: F12

   Key_Left_Shift : constant Keyboard_Key := 340;
   --  Key: Shift left

   Key_Left_Control : constant Keyboard_Key := 341;
   --  Key: Control left

   Key_Left_Alt : constant Keyboard_Key := 342;
   --  Key: Alt left

   Key_Left_Super : constant Keyboard_Key := 343;
   --  Key: Super left

   Key_Right_Shift : constant Keyboard_Key := 344;
   --  Key: Shift right

   Key_Right_Control : constant Keyboard_Key := 345;
   --  Key: Control right

   Key_Right_Alt : constant Keyboard_Key := 346;
   --  Key: Alt right

   Key_Right_Super : constant Keyboard_Key := 347;
   --  Key: Super right

   Key_Kb_Menu : constant Keyboard_Key := 348;
   --  Key: KB menu

   Key_Kp_0 : constant Keyboard_Key := 320;
   --  Key: Keypad 0

   Key_Kp_1 : constant Keyboard_Key := 321;
   --  Key: Keypad 1

   Key_Kp_2 : constant Keyboard_Key := 322;
   --  Key: Keypad 2

   Key_Kp_3 : constant Keyboard_Key := 323;
   --  Key: Keypad 3

   Key_Kp_4 : constant Keyboard_Key := 324;
   --  Key: Keypad 4

   Key_Kp_5 : constant Keyboard_Key := 325;
   --  Key: Keypad 5

   Key_Kp_6 : constant Keyboard_Key := 326;
   --  Key: Keypad 6

   Key_Kp_7 : constant Keyboard_Key := 327;
   --  Key: Keypad 7

   Key_Kp_8 : constant Keyboard_Key := 328;
   --  Key: Keypad 8

   Key_Kp_9 : constant Keyboard_Key := 329;
   --  Key: Keypad 9

   Key_Kp_Decimal : constant Keyboard_Key := 330;
   --  Key: Keypad .

   Key_Kp_Divide : constant Keyboard_Key := 331;
   --  Key: Keypad /

   Key_Kp_Multiply : constant Keyboard_Key := 332;
   --  Key: Keypad *

   Key_Kp_Subtract : constant Keyboard_Key := 333;
   --  Key: Keypad -

   Key_Kp_Add : constant Keyboard_Key := 334;
   --  Key: Keypad +

   Key_Kp_Enter : constant Keyboard_Key := 335;
   --  Key: Keypad Enter

   Key_Kp_Equal : constant Keyboard_Key := 336;
   --  Key: Keypad =

   Key_Back : constant Keyboard_Key := 4;
   --  Key: Android back button

   Key_Menu : constant Keyboard_Key := 82;
   --  Key: Android menu button

   Key_Volume_Up : constant Keyboard_Key := 24;
   --  Key: Android volume up button

   Key_Volume_Down : constant Keyboard_Key := 25;
   --  Key: Android volume down button

   type Mouse_Button is new Interfaces.C.unsigned;
   --  Mouse buttons

   Mouse_Button_Left : constant Mouse_Button := 0;
   --  Mouse button left

   Mouse_Button_Right : constant Mouse_Button := 1;
   --  Mouse button right

   Mouse_Button_Middle : constant Mouse_Button := 2;
   --  Mouse button middle (pressed wheel)

   Mouse_Button_Side : constant Mouse_Button := 3;
   --  Mouse button side (advanced mouse device)

   Mouse_Button_Extra : constant Mouse_Button := 4;
   --  Mouse button extra (advanced mouse device)

   Mouse_Button_Forward : constant Mouse_Button := 5;
   --  Mouse button fordward (advanced mouse device)

   Mouse_Button_Back : constant Mouse_Button := 6;
   --  Mouse button back (advanced mouse device)

   type Mouse_Cursor is new Interfaces.C.unsigned;
   --  Mouse cursor

   Mouse_Cursor_Default : constant Mouse_Cursor := 0;
   --  Default pointer shape

   Mouse_Cursor_Arrow : constant Mouse_Cursor := 1;
   --  Arrow shape

   Mouse_Cursor_Ibeam : constant Mouse_Cursor := 2;
   --  Text writing cursor shape

   Mouse_Cursor_Crosshair : constant Mouse_Cursor := 3;
   --  Cross shape

   Mouse_Cursor_Pointing_Hand : constant Mouse_Cursor := 4;
   --  Pointing hand cursor

   Mouse_Cursor_Resize_EW : constant Mouse_Cursor := 5;
   --  Horizontal resize/move arrow shape

   Mouse_Cursor_Resize_NS : constant Mouse_Cursor := 6;
   --  Vertical resize/move arrow shape

   Mouse_Cursor_Resize_NWSE : constant Mouse_Cursor := 7;
   --  Top-left to bottom-right diagonal resize/move arrow shape

   Mouse_Cursor_Resize_NESW : constant Mouse_Cursor := 8;
   --  The top-right to bottom-left diagonal resize/move arrow shape

   Mouse_Cursor_Resize_All : constant Mouse_Cursor := 9;
   --  The omni-directional resize/move cursor shape

   Mouse_Cursor_Not_Allowed : constant Mouse_Cursor := 10;
   --  The operation-not-allowed shape

   type Gamepad_Button is new Interfaces.C.unsigned;
   --  Gamepad buttons

   Gamepad_Button_Unknown : constant Gamepad_Button := 0;
   --  Unknown button, just for error checking

   Gamepad_Button_Left_Face_Up : constant Gamepad_Button := 1;
   --  Gamepad left DPAD up button

   Gamepad_Button_Left_Face_Right : constant Gamepad_Button := 2;
   --  Gamepad left DPAD right button

   Gamepad_Button_Left_Face_Down : constant Gamepad_Button := 3;
   --  Gamepad left DPAD down button

   Gamepad_Button_Left_Face_Left : constant Gamepad_Button := 4;
   --  Gamepad left DPAD left button

   Gamepad_Button_Right_Face_Up : constant Gamepad_Button := 5;
   --  Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)

   Gamepad_Button_Right_Face_Right : constant Gamepad_Button := 6;
   --  Gamepad right button right (i.e. PS3: Square, Xbox: X)

   Gamepad_Button_Right_Face_Down : constant Gamepad_Button := 7;
   --  Gamepad right button down (i.e. PS3: Cross, Xbox: A)

   Gamepad_Button_Right_Face_Left : constant Gamepad_Button := 8;
   --  Gamepad right button left (i.e. PS3: Circle, Xbox: B)

   Gamepad_Button_Left_Trigger_1 : constant Gamepad_Button := 9;
   --  Gamepad top/back trigger left (first), it could be a trailing button

   Gamepad_Button_Left_Trigger_2 : constant Gamepad_Button := 10;
   --  Gamepad top/back trigger left (second), it could be a trailing button

   Gamepad_Button_Right_Trigger_1 : constant Gamepad_Button := 11;
   --  Gamepad top/back trigger right (one), it could be a trailing button

   Gamepad_Button_Right_Trigger_2 : constant Gamepad_Button := 12;
   --  Gamepad top/back trigger right (second), it could be a trailing button

   Gamepad_Button_Middle_Left : constant Gamepad_Button := 13;
   --  Gamepad center buttons, left one (i.e. PS3: Select)

   Gamepad_Button_Middle : constant Gamepad_Button := 14;
   --  Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)

   Gamepad_Button_Middle_Right : constant Gamepad_Button := 15;
   --  Gamepad center buttons, right one (i.e. PS3: Start)

   Gamepad_Button_Left_Thumb : constant Gamepad_Button := 16;
   --  Gamepad joystick pressed button left

   Gamepad_Button_Right_Thumb : constant Gamepad_Button := 17;
   --  Gamepad joystick pressed button right

   type Gamepad_Axis is new Interfaces.C.unsigned;
   --  Gamepad axis

   Gamepad_Axis_Left_X : constant Gamepad_Axis := 0;
   --  Gamepad left stick X axis

   Gamepad_Axis_Left_Y : constant Gamepad_Axis := 1;
   --  Gamepad left stick Y axis

   Gamepad_Axis_Right_X : constant Gamepad_Axis := 2;
   --  Gamepad right stick X axis

   Gamepad_Axis_Right_Y : constant Gamepad_Axis := 3;
   --  Gamepad right stick Y axis

   Gamepad_Axis_Left_Trigger : constant Gamepad_Axis := 4;
   --  Gamepad back trigger left, pressure level: [1..-1]

   Gamepad_Axis_Right_Trigger : constant Gamepad_Axis := 5;
   --  Gamepad back trigger right, pressure level: [1..-1]

   type Material_Map_Index is new Interfaces.C.unsigned;
   --  Material map index

   Material_Map_Albedo : constant Material_Map_Index := 0;
   --  Albedo material (same as: MATERIAL_MAP_DIFFUSE)

   Material_Map_Metalness : constant Material_Map_Index := 1;
   --  Metalness material (same as: MATERIAL_MAP_SPECULAR)

   Material_Map_Normal : constant Material_Map_Index := 2;
   --  Normal material

   Material_Map_Roughness : constant Material_Map_Index := 3;
   --  Roughness material

   Material_Map_Occlusion : constant Material_Map_Index := 4;
   --  Ambient occlusion material

   Material_Map_Emission : constant Material_Map_Index := 5;
   --  Emission material

   Material_Map_Height : constant Material_Map_Index := 6;
   --  Heightmap material

   Material_Map_Cubemap : constant Material_Map_Index := 7;
   --  Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)

   Material_Map_Irradiance : constant Material_Map_Index := 8;
   --  Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)

   Material_Map_Prefilter : constant Material_Map_Index := 9;
   --  Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)

   Material_Map_Brdf : constant Material_Map_Index := 10;
   --  Brdf material

   Material_Map_Diffuse : constant Material_Map_Index := Material_Map_Albedo;

   Material_Map_Specular : constant Material_Map_Index :=
     Material_Map_Metalness;

   type Shader_Location_Index is new Interfaces.C.unsigned;
   --  Shader location index

   Shader_Loc_Vertex_Position : constant Shader_Location_Index := 0;
   --  Shader location: vertex attribute: position

   Shader_Loc_Vertex_Texcoord01 : constant Shader_Location_Index := 1;
   --  Shader location: vertex attribute: texcoord01

   Shader_Loc_Vertex_Texcoord02 : constant Shader_Location_Index := 2;
   --  Shader location: vertex attribute: texcoord02

   Shader_Loc_Vertex_Normal : constant Shader_Location_Index := 3;
   --  Shader location: vertex attribute: normal

   Shader_Loc_Vertex_Tangent : constant Shader_Location_Index := 4;
   --  Shader location: vertex attribute: tangent

   Shader_Loc_Vertex_Color : constant Shader_Location_Index := 5;
   --  Shader location: vertex attribute: color

   Shader_Loc_Matrix_Mvp : constant Shader_Location_Index := 6;
   --  Shader location: matrix uniform: model-view-projection

   Shader_Loc_Matrix_View : constant Shader_Location_Index := 7;
   --  Shader location: matrix uniform: view (camera transform)

   Shader_Loc_Matrix_Projection : constant Shader_Location_Index := 8;
   --  Shader location: matrix uniform: projection

   Shader_Loc_Matrix_Model : constant Shader_Location_Index := 9;
   --  Shader location: matrix uniform: model (transform)

   Shader_Loc_Matrix_Normal : constant Shader_Location_Index := 10;
   --  Shader location: matrix uniform: normal

   Shader_Loc_Vector_View : constant Shader_Location_Index := 11;
   --  Shader location: vector uniform: view

   Shader_Loc_Color_Diffuse : constant Shader_Location_Index := 12;
   --  Shader location: vector uniform: diffuse color

   Shader_Loc_Color_Specular : constant Shader_Location_Index := 13;
   --  Shader location: vector uniform: specular color

   Shader_Loc_Color_Ambient : constant Shader_Location_Index := 14;
   --  Shader location: vector uniform: ambient color

   Shader_Loc_Map_Albedo : constant Shader_Location_Index := 15;
   --  Shader location: sampler2d texture: albedo
   --  (same as: Shader_Loc_Map_Diffuse)

   Shader_Loc_Map_Metalness : constant Shader_Location_Index := 16;
   --  Shader location: sampler2d texture: metalness
   --  (same as: Shader_Loc_Map_Specular)

   Shader_Loc_Map_Normal : constant Shader_Location_Index := 17;
   --  Shader location: sampler2d texture: normal

   Shader_Loc_Map_Roughness : constant Shader_Location_Index := 18;
   --  Shader location: sampler2d texture: roughness

   Shader_Loc_Map_Occlusion : constant Shader_Location_Index := 19;
   --  Shader location: sampler2d texture: occlusion

   Shader_Loc_Map_Emission : constant Shader_Location_Index := 20;
   --  Shader location: sampler2d texture: emission

   Shader_Loc_Map_Height : constant Shader_Location_Index := 21;
   --  Shader location: sampler2d texture: height

   Shader_Loc_Map_Cubemap : constant Shader_Location_Index := 22;
   --  Shader location: samplerCube texture: cubemap

   Shader_Loc_Map_Irradiance : constant Shader_Location_Index := 23;
   --  Shader location: samplerCube texture: irradiance

   Shader_Loc_Map_Prefilter : constant Shader_Location_Index := 24;
   --  Shader location: samplerCube texture: prefilter

   Shader_Loc_Map_Brdf : constant Shader_Location_Index := 25;
   --  Shader location: sampler2d texture: brdf

   Shader_Loc_Map_Diffuse : constant Shader_Location_Index :=
     Shader_Loc_Map_Albedo;

   Shader_Loc_Map_Specular : constant Shader_Location_Index :=
     Shader_Loc_Map_Metalness;

   type Shader_Uniform_Data_Type is new Interfaces.C.unsigned;
   --  Shader uniform data type

   Shader_Uniform_Float : constant Shader_Uniform_Data_Type := 0;
   --  Shader uniform type: float

   Shader_Uniform_Vec2 : constant Shader_Uniform_Data_Type := 1;
   --  Shader uniform type: vec2 (2 float)

   Shader_Uniform_Vec3 : constant Shader_Uniform_Data_Type := 2;
   --  Shader uniform type: vec3 (3 float)

   Shader_Uniform_Vec4 : constant Shader_Uniform_Data_Type := 3;
   --  Shader uniform type: vec4 (4 float)

   Shader_Uniform_Int : constant Shader_Uniform_Data_Type := 4;
   --  Shader uniform type: int

   Shader_Uniform_Ivec2 : constant Shader_Uniform_Data_Type := 5;
   --  Shader uniform type: ivec2 (2 int)

   Shader_Uniform_Ivec3 : constant Shader_Uniform_Data_Type := 6;
   --  Shader uniform type: ivec3 (3 int)

   Shader_Uniform_Ivec4 : constant Shader_Uniform_Data_Type := 7;
   --  Shader uniform type: ivec4 (4 int)

   Shader_Uniform_Sampler2D : constant Shader_Uniform_Data_Type := 8;
   --  Shader uniform type: sampler2d

   type Shader_Attribute_Data_Type is new Interfaces.C.unsigned;
   --  Shader attribute data types

   Shader_Attrib_Float : constant Shader_Attribute_Data_Type := 0;
   --  Shader attribute type: float

   Shader_Attrib_Vec2 : constant Shader_Attribute_Data_Type := 1;
   --  Shader attribute type: vec2 (2 float)

   Shader_Attrib_Vec3 : constant Shader_Attribute_Data_Type := 2;
   --  Shader attribute type: vec3 (3 float)

   Shader_Attrib_Vec4 : constant Shader_Attribute_Data_Type := 3;
   --  Shader attribute type: vec4 (4 float)

   type Pixel_Format is new Interfaces.C.unsigned;
   --  Pixel formats

   Pixel_Format_Uncompressed_Grayscale : constant Pixel_Format := 1;
   --  8 bit per pixel (no alpha)

   Pixel_Format_Uncompressed_Gray_Alpha : constant Pixel_Format := 2;
   --  8*2 bpp (2 channels)

   Pixel_Format_Uncompressed_R5G6B5 : constant Pixel_Format := 3;
   --  16 bpp

   Pixel_Format_Uncompressed_R8G8B8 : constant Pixel_Format := 4;
   --  24 bpp

   Pixel_Format_Uncompressed_R5G5B5A1 : constant Pixel_Format := 5;
   --  16 bpp (1 bit alpha)

   Pixel_Format_Uncompressed_R4G4B4A4 : constant Pixel_Format := 6;
   --  16 bpp (4 bit alpha)

   Pixel_Format_Uncompressed_R8G8B8A8 : constant Pixel_Format := 7;
   --  32 bpp

   Pixel_Format_Uncompressed_R32 : constant Pixel_Format := 8;
   --  32 bpp (1 channel - float)

   Pixel_Format_Uncompressed_R32G32B32 : constant Pixel_Format := 9;
   --  32*3 bpp (3 channels - float)

   Pixel_Format_Uncompressed_R32G32B32A32 : constant Pixel_Format := 10;
   --  32*4 bpp (4 channels - float)

   Pixel_Format_Compressed_DXT1_RGB : constant Pixel_Format := 11;
   --  4 bpp (no alpha)

   Pixel_Format_Compressed_DXT1_RGBA : constant Pixel_Format := 12;
   --  4 bpp (1 bit alpha)

   Pixel_Format_Compressed_DXT3_RGBA : constant Pixel_Format := 13;
   --  8 bpp

   Pixel_Format_Compressed_DXT5_RGBA : constant Pixel_Format := 14;
   --  8 bpp

   Pixel_Format_Compressed_ETC1_RGB : constant Pixel_Format := 15;
   --  4 bpp

   Pixel_Format_Compressed_ETC2_RGB : constant Pixel_Format := 16;
   --  4 bpp

   Pixel_Format_Compressed_ETC2_EAC_RGBA : constant Pixel_Format := 17;
   --  8 bpp

   Pixel_Format_Compressed_PVRT_RGB : constant Pixel_Format := 18;
   --  4 bpp

   Pixel_Format_Compressed_PVRT_RGBA : constant Pixel_Format := 19;
   --  4 bpp

   Pixel_Format_Compressed_ASTC_4X4_RGBA : constant Pixel_Format := 20;
   --  8 bpp

   Pixel_Format_Compressed_ASTC_8X8_RGBA : constant Pixel_Format := 21;
   --  2 bpp

   type Texture_Filter is new Interfaces.C.unsigned;
   --  Texture parameters: filter mode

   Texture_Filter_Point : constant Texture_Filter := 0;
   --  No filter, just pixel approximation

   Texture_Filter_Bilinear : constant Texture_Filter := 1;
   --  Linear filtering

   Texture_Filter_Trilinear : constant Texture_Filter := 2;
   --  Trilinear filtering (linear with mipmaps)

   Texture_Filter_Anisotropic_4x : constant Texture_Filter := 3;
   --  Anisotropic filtering 4x

   Texture_Filter_Anisotropic_8x : constant Texture_Filter := 4;
   --  Anisotropic filtering 8x

   Texture_Filter_Anisotropic_16x : constant Texture_Filter := 5;
   --  Anisotropic filtering 16x

   type Texture_Wrap is new Interfaces.C.unsigned;
   --  Texture parameters: wrap mode

   Texture_Wrap_Repeat : constant Texture_Wrap := 0;
   --  Repeats texture in tiled mode

   Texture_Wrap_Clamp : constant Texture_Wrap := 1;
   --  Clamps texture to edge pixel in tiled mode

   Texture_Wrap_Mirror_Repeat : constant Texture_Wrap := 2;
   --  Mirrors and repeats the texture in tiled mode

   Texture_Wrap_Mirror_Clamp : constant Texture_Wrap := 3;
   --  Mirrors and clamps to border the texture in tiled mode

   type Cubemap_Layout is new Interfaces.C.unsigned;
   --  Cubemap layouts

   Cubemap_Layout_Auto_Detect : constant Cubemap_Layout := 0;
   --  Automatically detect layout type

   Cubemap_Layout_Line_Vertical : constant Cubemap_Layout := 1;
   --  Layout is defined by a vertical line with faces

   Cubemap_Layout_Line_Horizontal : constant Cubemap_Layout := 2;
   --  Layout is defined by an horizontal line with faces

   Cubemap_Layout_Cross_Three_By_Four : constant Cubemap_Layout := 3;
   --  Layout is defined by a 3x4 cross with cubemap faces

   Cubemap_Layout_Cross_Four_By_Three : constant Cubemap_Layout := 4;
   --  Layout is defined by a 4x3 cross with cubemap faces

   Cubemap_Layout_Panorama : constant Cubemap_Layout := 5;
   --  Layout is defined by a panorama image (equirectangular map)

   type Font_Type is new Interfaces.C.unsigned;
   --  Font type, defines generation method

   Font_Default : constant Font_Type := 0;
   --  Default font generation, anti-aliased

   Font_Bitmap : constant Font_Type := 1;
   --  Bitmap font generation, no anti-aliasing

   Font_SDF : constant Font_Type := 2;
   --  SDF font generation, requires external shader

   type Blend_Mode is new Interfaces.C.unsigned;
   --  Color blending modes (pre-defined)

   Blend_Alpha : constant Blend_Mode := 0;
   --  Blend textures considering alpha (default)

   Blend_Additive : constant Blend_Mode := 1;
   --  Blend textures adding colors

   Blend_Multiplied : constant Blend_Mode := 2;
   --  Blend textures multiplying colors

   Blend_Add_Colors : constant Blend_Mode := 3;
   --  Blend textures adding colors (alternative)

   Blend_Subtract_Colors : constant Blend_Mode := 4;
   --  Blend textures subtracting colors (alternative)

   Blend_Alpha_Premul : constant Blend_Mode := 5;
   --  Blend premultiplied textures considering alpha

   Blend_Custom : constant Blend_Mode := 6;
   --  Blend textures using custom src/dst factors (use rlSetBlendMode())

   type Gesture is new Interfaces.C.unsigned;
   --  Gesture

   Gesture_None : constant Gesture := 0;
   --  No gesture

   Gesture_Tap : constant Gesture := 1;
   --  Tap gesture

   Gesture_Doubletap : constant Gesture := 2;
   --  Double tap gesture

   Gesture_Hold : constant Gesture := 4;
   --  Hold gesture

   Gesture_Drag : constant Gesture := 8;
   --  Drag gesture

   Gesture_Swipe_Right : constant Gesture := 16;
   --  Swipe right gesture

   Gesture_Swipe_Left : constant Gesture := 32;
   --  Swipe left gesture

   Gesture_Swipe_Up : constant Gesture := 64;
   --  Swipe up gesture

   Gesture_Swipe_Down : constant Gesture := 128;
   --  Swipe down gesture

   Gesture_Pinch_In : constant Gesture := 256;
   --  Pinch in gesture

   Gesture_Pinch_Out : constant Gesture := 512;
   --  Pinch out gesture

   type Camera_Mode is new Interfaces.C.unsigned;
   --  Camera system modes

   Camera_Custom : constant Camera_Mode := 0;
   --  Custom camera

   Camera_Free : constant Camera_Mode := 1;
   --  Free camera

   Camera_Orbital : constant Camera_Mode := 2;
   --  Orbital camera

   Camera_First_Person : constant Camera_Mode := 3;
   --  First person camera

   Camera_Third_Person : constant Camera_Mode := 4;
   --  Third person camera

   type Camera_Projection is new Interfaces.C.unsigned;
   --  Camera projection

   Camera_Perspective : constant Camera_Projection := 0;
   --  Perspective projection

   Camera_Orthographic : constant Camera_Projection := 1;
   --  Orthographic projection

   type N_Patch_Layout is new Interfaces.C.unsigned;
   --  N-patch layout

   Npatch_Nine_Patch : constant N_Patch_Layout := 0;
   --  Npatch layout: 3x3 tiles

   Npatch_Three_Patch_Vertical : constant N_Patch_Layout := 1;
   --  Npatch layout: 1x3 tiles

   Npatch_Three_Patch_Horizontal : constant N_Patch_Layout := 2;
   --  Npatch layout: 3x1 tiles

   --  Callback types
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

   type Vector2 is record
      X : Float;
      --  Vector x component
      Y : Float;
      --  Vector y component
   end record;
   --  Vector2, 2 components

   type Vector2_Array is array (Natural range <>) of Vector2;

   type Vector3 is record
      X : Float;
      --  Vector x component
      Y : Float;
      --  Vector y component
      Z : Float;
      --  Vector z component
   end record;
   --  Vector3, 3 components

   type Vector3_Array is array (Natural range <>) of Vector3;

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
   --  Vector4, 4 components

   subtype Quaternion is RayLib.Vector4;
   --  Quaternion, 4 components (Vector4 alias)

   type Vector4_Array is array (Natural range <>) of Vector4;

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
   --  Matrix, 4x4 components, column major, OpenGL style, right handed

   type Matrix_Array is array (Natural range <>) of Matrix;

   type Color is record
      R : Color_Component;
      --  Color red value
      G : Color_Component;
      --  Color green value
      B : Color_Component;
      --  Color blue value
      A : Color_Component;
      --  Color alpha value
   end record;
   --  Color, 4 components, R8G8B8A8 (32bit)

   type Color_Array is array (Natural range <>) of Color;

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
   --  Rectangle, 4 components

   type Rectangle_Array is array (Natural range <>) of Rectangle;

   type Image is tagged private;
   --  Image, pixel data stored in CPU memory (RAM)

   function Data (Self : Image'Class) return Stream_Element_Array;
   --  Image raw data

   function Width (Self : Image'Class) return Integer;
   --  Image base width

   function Height (Self : Image'Class) return Integer;
   --  Image base height

   function Mipmaps (Self : Image'Class) return Integer;
   --  Mipmap levels, 1 by default

   function Format (Self : Image'Class) return Pixel_Format;
   --  Data format (Pixel_Format type)

   type Texture is tagged private;
   --  Texture, tex data stored in GPU memory (VRAM)

   function Id (Self : Texture'Class) return OpenGL_Id;
   --  OpenGL texture id

   function Width (Self : Texture'Class) return Integer;
   --  Texture base width

   function Height (Self : Texture'Class) return Integer;
   --  Texture base height

   function Mipmaps (Self : Texture'Class) return Integer;
   --  Mipmap levels, 1 by default

   function Format (Self : Texture'Class) return Pixel_Format;
   --  Data format (Pixel_Format type)

   subtype Texture2D is RayLib.Texture;
   --  Texture2D, same as Texture

   subtype Texture_Cubemap is RayLib.Texture;
   --  TextureCubemap, same as Texture

   type Render_Texture is tagged private;
   --  RenderTexture, fbo for texture rendering

   function Id (Self : Render_Texture'Class) return OpenGL_Id;
   --  OpenGL framebuffer object id

   function Get_Texture
     (Self : Render_Texture'Class) return RayLib.Texture'Class;
   --  Color buffer attachment texture

   function Depth (Self : Render_Texture'Class) return RayLib.Texture'Class;
   --  Depth buffer attachment texture

   subtype Render_Texture2D is RayLib.Render_Texture;
   --  RenderTexture2D, same as RenderTexture

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
      Layout : N_Patch_Layout;
      --  Layout of the n-patch: 3x3, 1x3 or 3x1
   end record;
   --  NPatchInfo, n-patch layout info

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
   --  GlyphInfo, font characters glyphs info

   type Glyph_Info_Array is array (Natural range <>) of Glyph_Info;

   type Font is tagged private;
   --  Font, font texture and GlyphInfo array data

   function Glyph_Count (Self : Font'Class) return Natural;
   --  Number of glyph characters

   function Base_Size (Self : Font'Class) return Integer;
   --  Base size (default chars height)

   function Glyph_Padding (Self : Font'Class) return Natural;
   --  Padding around the glyph characters

   function Get_Texture (Self : Font'Class) return RayLib.Texture2D'Class;
   --  Texture atlas containing the glyphs

   function Recs (Self : Font'Class) return Rectangle_Array;
   --  Rectangles in texture for the glyphs

   function Glyphs (Self : Font'Class) return Glyph_Info_Array;
   --  Glyphs info data

   type Camera3D is record
      Position : RayLib.Vector3;
      --  Camera position
      Target : RayLib.Vector3;
      --  Camera target it looks-at
      Up : RayLib.Vector3;
      --  Camera up vector (rotation over its axis)
      Fovy : Float;
      --  Camera field-of-view apperture in Y (degrees) in perspective,
      --  used as near plane width in orthographic
      Projection : Camera_Projection;
      --  Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
   end record;
   --  Camera, defines position/orientation in 3d space

   subtype Camera is RayLib.Camera3D;
   --  Camera type fallback, defaults to Camera3D

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
   --  Camera2D, defines position/orientation in 2d space

   type Mesh is tagged private;
   --  Mesh, vertex data and vao/vbo

   function Vertex_Count (Self : Mesh'Class) return Natural;
   --  Number of vertices stored in arrays

   function Triangle_Count (Self : Mesh'Class) return Natural;
   --  Number of triangles stored (indexed or not)

   function Vertices (Self : Mesh'Class) return Vector3_Array;
   --  Vertex position (XYZ - 3 components per vertex) (shader-location = 0)

   function Texcoords (Self : Mesh'Class) return Vector2_Array;
   --  Vertex texture coordinates (UV - 2 components per vertex)
   --  (shader-location = 1)

   function Texcoords2 (Self : Mesh'Class) return Vector2_Array;
   --  Vertex texture second coordinates (UV - 2 components per vertex)
   --  (shader-location = 5)

   function Normals (Self : Mesh'Class) return Vector3_Array;
   --  Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)

   function Tangents (Self : Mesh'Class) return Vector4_Array;
   --  Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)

   function Colors (Self : Mesh'Class) return Color_Array;
   --  Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)

   function Indices (Self : Mesh'Class) return Integer_Array;
   --  Vertex indices (in case vertex data comes indexed)

   function Animated_Vertices (Self : Mesh'Class) return Vector3_Array;
   --  Animated vertex positions (after bones transformations)

   function Animated_Normals (Self : Mesh'Class) return Vector3_Array;
   --  Animated normals (after bones transformations)

   function Bone_Ids (Self : Mesh'Class) return Stream_Element_Array;
   --  Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex
   --  (skinning)

   function Bone_Weights (Self : Mesh'Class) return Float_Array;
   --  Vertex bone weight, up to 4 bones influence by vertex (skinning)

   function VAO_Id (Self : Mesh'Class) return OpenGL_Id;
   --  OpenGL Vertex Array Object id

   function VBO_Id (Self : Mesh'Class) return OpenGL_Id_Array;
   --  OpenGL Vertex Buffer Objects id (default vertex data)

   type Mesh_Array is array (Natural range <>) of Mesh;

   type Shader is tagged private;
   --  Shader

   function Id (Self : Shader'Class) return OpenGL_Id;
   --  Shader program id

   function Locations (Self : Shader'Class) return Integer_Array;
   --  Shader locations array (RL_MAX_SHADER_LOCATIONS)

   type Material_Map is record
      Texture : RayLib.Texture2D;
      --  Material map texture
      Color : RayLib.Color;
      --  Material map color
      Value : Float;
      --  Material map value
   end record;
   --  MaterialMap

   type Material_Map_Array is array (Natural range <>) of Material_Map;

   type Material is tagged private;
   --  Material, includes shader and maps

   function Get_Shader (Self : Material'Class) return RayLib.Shader'Class;
   --  Material shader

   function Maps (Self : Material'Class) return Material_Map_Array;
   --  Material maps array (MAX_MATERIAL_MAPS)

   function Parameters (Self : Material'Class) return Float_Array;
   --  Material generic parameters (if required)

   type Material_Array is array (Natural range <>) of Material;

   type Transform is record
      Translation : RayLib.Vector3;
      --  Translation
      Rotation : RayLib.Quaternion;
      --  Rotation
      Scale : RayLib.Vector3;
      --  Scale
   end record;
   --  Transform, vectex transformation data

   type Transform_Array is array (Natural range <>) of Transform;

   type Bone_Info is record
      Name : String (1 .. 32);
      --  Bone name
      Parent : Integer;
      --  Bone parent
   end record;
   --  Bone, skeletal animation bone

   type Bone_Info_Array is array (Natural range <>) of Bone_Info;

   type Model is tagged private;
   --  Model, meshes, materials and animation data

   function Mesh_Count (Self : Model'Class) return Natural;
   --  Number of meshes

   function Material_Count (Self : Model'Class) return Natural;
   --  Number of materials

   function Bone_Count (Self : Model'Class) return Natural;
   --  Number of bones

   function Get_Transform (Self : Model'Class) return RayLib.Matrix;
   --  Local transform matrix

   function Meshes (Self : Model'Class) return Mesh_Array;
   --  Meshes array

   function Materials (Self : Model'Class) return Material_Array;
   --  Materials array

   function Mesh_Material (Self : Model'Class) return Integer_Array;
   --  Mesh material number

   function Bones (Self : Model'Class) return Bone_Info_Array;
   --  Bones information (skeleton)

   function Bind_Pose (Self : Model'Class) return Natural;
   --  Bones base transformation (pose)

   type Model_Animation is tagged private;
   --  ModelAnimation

   function Bone_Count (Self : Model_Animation'Class) return Natural;
   --  Number of bones

   function Frame_Count (Self : Model_Animation'Class) return Natural;
   --  Number of animation frames

   function Bones (Self : Model_Animation'Class) return Bone_Info_Array;
   --  Bones information (skeleton)

   function Frame_Poses (Self : Model_Animation'Class) return Transform_Array;
   --  Poses array by frame

   type Model_Animation_Array is array (Natural range <>) of Model_Animation;

   type Ray is record
      Position : RayLib.Vector3;
      --  Ray position (origin)
      Direction : RayLib.Vector3;
      --  Ray direction
   end record;
   --  Ray, ray for raycasting

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
   --  RayCollision, ray hit information

   type Bounding_Box is record
      Min : RayLib.Vector3;
      --  Minimum vertex box-corner
      Max : RayLib.Vector3;
      --  Maximum vertex box-corner
   end record;
   --  BoundingBox

   type Wave is tagged private;
   --  Wave, audio wave data

   function Frame_Count (Self : Wave'Class) return Natural;
   --  Total number of frames (considering channels)

   function Sample_Rate (Self : Wave'Class) return Natural;
   --  Frequency (samples per second)

   function Sample_Size (Self : Wave'Class) return Natural;
   --  Bit depth (bits per sample): 8, 16, 32 (24 not supported)

   function Channels (Self : Wave'Class) return Natural;
   --  Number of channels (1-mono, 2-stereo, ...)

   function Data (Self : Wave'Class) return Stream_Element_Array;

   type Audio_Stream is tagged private;
   --  AudioStream, custom audio stream

   function Buffer (Self : Audio_Stream'Class) return Stream_Element_Array;
   --  Pointer to internal data used by the audio system

   --  TODO: Audio_Stream's processor

   function Sample_Rate (Self : Audio_Stream'Class) return Natural;
   --  Frequency (samples per second)

   function Sample_Size (Self : Audio_Stream'Class) return Natural;
   --  Bit depth (bits per sample): 8, 16, 32 (24 not supported)

   function Channels (Self : Audio_Stream'Class) return Natural;
   --  Number of channels (1-mono, 2-stereo, ...)

   type Sound is tagged private;
   --  Sound

   function Stream (Self : Sound'Class) return Audio_Stream;
   --  Audio stream

   function Frame_Count (Self : Sound'Class) return Natural;
   --  Total number of frames (considering channels)

   type Music is tagged private;
   --  Music, audio stream, anything longer than ~10 seconds should be streamed

   function Stream (Self : Music'Class) return Audio_Stream;
   --  Audio stream

   function Frame_Count (Self : Music'Class) return Natural;
   --  Total number of frames (considering channels)

   function Looping (Self : Music'Class) return Boolean;
   --  Music looping enable

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
   --  VrDeviceInfo, Head-Mounted-Display device parameters

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
   --  VrStereoConfig, VR stereo rendering configuration for simulator

   --  Some Basic Colors
   --  NOTE: Custom raylib color palette for amazing visuals
   --   on WHITE background

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

   ------------------------------
   --  Windows and graphics device functions
   ------------------------------

   procedure Init_Window (Width : Natural; Height : Natural; Title : String);
   --  Initialize window and OpenGL context

   function Window_Should_Close return Boolean;
   --  Check if Key_Escape pressed or Close icon pressed

   procedure Close_Window with
      Import,
      Convention    => C,
      External_Name => "CloseWindow";
      --  Close window and unload OpenGL context

   function Is_Window_Ready return Boolean;
   --  Check if window has been initialized successfully

   function Is_Window_Fullscreen return Boolean;
   --  Check if window is currently fullscreen

   function Is_Window_Hidden return Boolean;
   --  Check if window is currently hidden (only PLATFORM_DESKTOP)

   function Is_Window_Minimized return Boolean;
   --  Check if window is currently minimized (only PLATFORM_DESKTOP)

   function Is_Window_Maximized return Boolean;
   --  Check if window is currently maximized (only PLATFORM_DESKTOP)

   function Is_Window_Focused return Boolean;
   --  Check if window is currently focused (only PLATFORM_DESKTOP)

   function Is_Window_Resized return Boolean;
   --  Check if window has been resized last frame

   function Is_Window_State (Flag : Config_Flag) return Boolean;
   --  Check if one specific window flag is enabled

   procedure Set_Window_State (Flags : Config_Flag);
   --  Set window configuration state using flags (only PLATFORM_DESKTOP)

   procedure Clear_Window_State (Flags : Config_Flag);
   --  Clear window configuration state flags

   procedure Toggle_Fullscreen with
      Import,
      Convention    => C,
      External_Name => "ToggleFullscreen";
      --  Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)

   procedure Maximize_Window with
      Import,
      Convention    => C,
      External_Name => "MaximizeWindow";
      --  Set window state: maximized, if resizable (only PLATFORM_DESKTOP)

   procedure Minimize_Window with
      Import,
      Convention    => C,
      External_Name => "MinimizeWindow";
      --  Set window state: minimized, if resizable (only PLATFORM_DESKTOP)

   procedure Restore_Window with
      Import,
      Convention    => C,
      External_Name => "RestoreWindow";
      --  Set window state: not minimized/maximized (only PLATFORM_DESKTOP)

   procedure Set_Window_Icon (Image : RayLib.Image'Class);
   --  Set icon for window (only PLATFORM_DESKTOP)

   procedure Set_Window_Title (Title : String);
   --  Set title for window (only PLATFORM_DESKTOP)

   procedure Set_Window_Position (X : Natural; Y : Natural);
   --  Set window position on screen (only PLATFORM_DESKTOP)

   procedure Set_Window_Monitor (Monitor : Monitor_Id);
   --  Set monitor for the current window (fullscreen mode)

   procedure Set_Window_Min_Size (Width : Natural; Height : Natural);
   --  Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)

   procedure Set_Window_Size (Width : Natural; Height : Natural);
   --  Set window dimensions

   procedure Set_Window_Opacity (Opacity : Float) with
      Pre => Opacity >= 0.0 and Opacity <= 1.0;
      --  Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)

   function Get_Window_Handle return System.Address;
   --  Get native window handle

   function Get_Screen_Width return Natural;
   --  Get current screen width

   function Get_Screen_Height return Natural;
   --  Get current screen height

   function Get_Render_Width return Natural;
   --  Get current render width (it considers HiDPI)

   function Get_Render_Height return Natural;
   --  Get current render height (it considers HiDPI)

   function Get_Monitor_Count return Natural;
   --  Get number of connected monitors

   function Get_Current_Monitor return Monitor_Id;
   --  Get current connected monitor

   function Get_Monitor_Position (Monitor : Monitor_Id) return RayLib.Vector2;
   --  Get specified monitor position

   function Get_Monitor_Width (Monitor : Monitor_Id) return Natural;
   --  Get specified monitor width (max available by monitor)

   function Get_Monitor_Height (Monitor : Monitor_Id) return Natural;
   --  Get specified monitor height (max available by monitor)

   function Get_Monitor_Physical_Width (Monitor : Monitor_Id) return Natural;
   --  Get specified monitor physical width in millimetres

   function Get_Monitor_Physical_Height (Monitor : Monitor_Id) return Natural;
   --  Get specified monitor physical height in millimetres

   function Get_Monitor_Refresh_Rate (Monitor : Monitor_Id) return Natural;
   --  Get specified monitor refresh rate

   function Get_Window_Position return RayLib.Vector2;
   --  Get window position XY on monitor

   function Get_Window_Scale_DPI return RayLib.Vector2;
   --  Get window scale DPI factor

   function Get_Monitor_Name (Monitor : Monitor_Id) return String;
   --  Get the human-readable, UTF-8 encoded name of the primary monitor

   procedure Set_Clipboard_Text (Text : String);
   --  Set clipboard text content

   function Get_Clipboard_Text return String;
   --  Get clipboard text content

   procedure Swap_Screen_Buffer with
      Import,
      Convention    => C,
      External_Name => "SwapScreenBuffer";
      --  Swap back buffer with front buffer (screen drawing)

   procedure Poll_Input_Events with
      Import,
      Convention    => C,
      External_Name => "PollInputEvents";
      --  Register all input events

   procedure Wait_Time (Ms : Float);
   --  Wait for some milliseconds (halt program execution)

   procedure Show_Cursor with
      Import,
      Convention    => C,
      External_Name => "ShowCursor";
      --  Shows cursor

   procedure Hide_Cursor with
      Import,
      Convention    => C,
      External_Name => "HideCursor";
      --  Hides cursor

   function Is_Cursor_Hidden return Boolean;
   --  Check if cursor is not visible

   procedure Enable_Cursor with
      Import,
      Convention    => C,
      External_Name => "EnableCursor";
      --  Enables cursor (unlock cursor)

   procedure Disable_Cursor with
      Import,
      Convention    => C,
      External_Name => "DisableCursor";
      --  Disables cursor (lock cursor)

   function Is_Cursor_On_Screen return Boolean;
   --  Check if cursor is on the screen

   procedure Clear_Background (Color : RayLib.Color);
   --  Set background color (framebuffer clear color)

   procedure Begin_Drawing with
      Import,
      Convention    => C,
      External_Name => "BeginDrawing";
      --  Setup canvas (framebuffer) to start drawing

   procedure End_Drawing with
      Import,
      Convention    => C,
      External_Name => "EndDrawing";
      --  End canvas drawing and swap buffers (double buffering)

   procedure Begin_Mode2D (Camera : RayLib.Camera2D);
   --  Begin 2D mode with custom camera (2D)

   procedure End_Mode2D with
      Import,
      Convention    => C,
      External_Name => "EndMode2D";
      --  Ends 2D mode with custom camera

   procedure Begin_Mode3D (Camera : RayLib.Camera3D);
   --  Begin 3D mode with custom camera (3D)

   procedure End_Mode3D with
      Import,
      Convention    => C,
      External_Name => "EndMode3D";
      --  Ends 3D mode and returns to default 2D orthographic mode

   procedure Begin_Texture_Mode (Target : RayLib.Render_Texture2D);
   --  Begin drawing to render texture

   procedure End_Texture_Mode with
      Import,
      Convention    => C,
      External_Name => "EndTextureMode";
      --  Ends drawing to render texture

   procedure Begin_Shader_Mode (Shader : RayLib.Shader);
   --  Begin custom shader drawing

   procedure End_Shader_Mode with
      Import,
      Convention    => C,
      External_Name => "EndShaderMode";
      --  End custom shader drawing (use default shader)

   procedure Begin_Blend_Mode (Mode : Blend_Mode);
   --  Begin blending mode (alpha, additive, multiplied, subtract, custom)

   procedure End_Blend_Mode with
      Import,
      Convention    => C,
      External_Name => "EndBlendMode";
      --  End blending mode (reset to default: alpha blending)

   procedure Begin_Scissor_Mode (X, Y, Width, Height : Natural);
   --  Begin scissor mode (define screen area for following drawing)

   procedure End_Scissor_Mode with
      Import,
      Convention    => C,
      External_Name => "EndScissorMode";
      --  End scissor mode

   procedure Begin_VR_Stereo_Mode (Config : RayLib.VR_Stereo_Config);
   --  Begin stereo rendering (requires VR simulator)

   procedure End_VR_Stereo_Mode with
      Import,
      Convention    => C,
      External_Name => "EndVrStereoMode";
      --  End stereo rendering (requires VR simulator)

   function Load_VR_Stereo_Config
     (Device : RayLib.VR_Device_Info) return RayLib.VR_Stereo_Config;
   --  Load VR stereo config for VR simulator device parameters

   function Load_Shader
     (Vs_File_Name : String; Fs_File_Name : String) return RayLib.Shader;
   --  Load shader from files and bind default locations

   function Load_Shader_From_Memory
     (Vs_Code : String; Fs_Code : String) return RayLib.Shader;
   --  Load shader from code strings and bind default locations

   function Get_Shader_Location
     (Shader : RayLib.Shader; Uniform_Name : String) return Integer;
   --  Get shader uniform location

   function Get_Shader_Location_Attrib
     (Shader : RayLib.Shader; Attrib_Name : String) return Integer;
   --  Get shader attribute location

   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Value : System.Address;
      Uniform_Type : Integer);
   --  Set shader uniform value

   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Value : System.Address;
      Uniform_Type : Integer; Count : Integer);
   --  Set shader uniform value vector

   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Mat : RayLib.Matrix);
   --  Set shader uniform value (matrix 4x4)

   procedure Set_Shader_Value_Texture
     (Shader  : RayLib.Shader'Class; Loc_Index : Integer;
      Texture : RayLib.Texture2D'Class);
   --  Set shader uniform value for texture (sampler2d)

   function Get_Mouse_Ray
     (Mouse_Position : RayLib.Vector2; Camera : RayLib.Camera)
      return RayLib.Ray;
   --  Get a ray trace from mouse position

   function Get_Camera_Matrix (Camera : RayLib.Camera) return RayLib.Matrix;
   --  Get camera transform matrix (view matrix)

   function Get_Camera_Matrix2D
     (Camera : RayLib.Camera2D) return RayLib.Matrix;
   --  Get camera 2d transform matrix

   function Get_World_To_Screen
     (Position : RayLib.Vector3; Camera : RayLib.Camera) return RayLib.Vector2;
   --  Get the screen space position for a 3d world space position

   function Get_World_To_Screen
     (Position : RayLib.Vector3; Camera : RayLib.Camera; Width : Natural;
      Height   : Natural) return RayLib.Vector2;
   --  Get size position for a 3d world space position

   function Get_World_To_Screen2D
     (Position : RayLib.Vector2; Camera : RayLib.Camera2D)
      return RayLib.Vector2;
   --  Get the screen space position for a 2d camera world space position

   function Get_Screen_To_World2D
     (Position : RayLib.Vector2; Camera : RayLib.Camera2D)
      return RayLib.Vector2;
   --  Get the world space position for a 2d camera screen space position

   procedure Set_Target_FPS (Fps : Natural);
   --  Set target FPS (maximum)

   function Get_FPS return Natural;
   --  Get current FPS

   function Get_Frame_Time return Float;
   --  Get time in seconds for last frame drawn (delta time)

   function Get_Time return Long_Float;
   --  Get elapsed time in seconds since InitWindow()

   function Get_Random_Value (Min : Integer; Max : Integer) return Integer;
   --  Get a random value between min and max (both included)

   procedure Set_Random_Seed (Seed : Natural);
   --  Set the seed for the random number generator

   procedure Take_Screenshot (File_Name : String);
   --  Takes a screenshot of current screen (filename extension defines format)

   procedure Set_Config_Flags (Flags : Config_Flag);
   --  Setup init configuration flags (view FLAGS)

   procedure Trace_Log (Log_Level : Trace_Log_Level; Text : String);
   --  Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)

   procedure Set_Trace_Log_Level (Log_Level : Trace_Log_Level);
   --  Set the current threshold (minimum) log level

   procedure Set_Trace_Log_Callback (Callback : RayLib.Trace_Log_Callback);
   --  Set custom trace log

   procedure Set_Load_File_Data_Callback
     (Callback : RayLib.Load_File_Data_Callback);
   --  Set custom file binary data loader

   procedure Set_Save_File_Data_Callback
     (Callback : RayLib.Save_File_Data_Callback);
   --  Set custom file binary data saver

   procedure Set_Load_File_Text_Callback
     (Callback : RayLib.Load_File_Text_Callback);
   --  Set custom file text data loader

   procedure Set_Save_File_Text_Callback
     (Callback : RayLib.Save_File_Text_Callback);
   --  Set custom file text data saver

   function Load_File_Data (File_Name : String) return Stream_Element_Array;
   --  Load file data as byte array (read)

   function Save_File_Data
     (File_Name : String; Data : Stream_Element_Array) return Boolean;
   --  Save data to file from byte array (write), returns true on success

   function Load_File_Text (File_Name : String) return String;
   --  Load text data from file (read), returns string

   function Save_File_Text (File_Name : String; Text : String) return Boolean;
   --  Save text data to file (write), returns true on success

   function File_Exists (File_Name : String) return Boolean;
   --  Check if file exists

   function Directory_Exists (Dir_Path : String) return Boolean;
   --  Check if a directory path exists

   function Is_File_Extension
     (File_Name : String; Ext : String) return Boolean;
   --  Check file extension (including point: .png, .wav)

   function Get_File_Length (File_Name : String) return Natural;
   --  Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)

   function Get_File_Extension (File_Name : String) return String;
   --  Get pointer to extension for a filename string (includes dot: '.png')

   function Get_File_Name (File_Path : String) return String;
   --  Get pointer to filename for a path string

   function Get_File_Name_Without_Ext (File_Path : String) return String;
   --  Get filename string without extension (uses static string)

   function Get_Directory_Path (File_Path : String) return String;
   --  Get full path for a given fileName with path (uses static string)

   function Get_Prev_Directory_Path (Dir_Path : String) return String;
   --  Get previous directory path for a given path (uses static string)

   function Get_Working_Directory return String;
   --  Get current working directory (uses static string)

   function Get_Application_Directory return String;
   --  Get the directory if the running application (uses static string)

   function Get_Directory_Files
     (Dir_Path : String) return Unbounded_String_Array;
   --  Get filenames in a directory path

   function Change_Directory (Dir : String) return Boolean;
   --  Change working directory, return true on success

   function Is_File_Dropped return Boolean;
   --  Check if a file has been dropped into window

   function Get_Dropped_Files return Unbounded_String_Array;
   --  Get dropped files name

   function Get_File_Mod_Time (File_Name : String) return Time;
   --  Get file modification time (last write time)

   function Compress_Data
     (Data : Stream_Element_Array) return Stream_Element_Array;
   --  Compress data (DEFLATE algorithm)

   function Decompress_Data
     (Comp_Data : Stream_Element_Array) return Stream_Element_Array;
   --  Decompress data (DEFLATE algorithm)

   function Encode_Data_Base64 (Data : Stream_Element_Array) return String;
   --  Encode data to Base64 string

   function Decode_Data_Base64 (Data : String) return Stream_Element_Array;
   --  Decode Base64 string data

   function Save_Storage_Value
     (Position : Natural; Value : Integer) return Boolean;
   --  Save integer value to storage file (to defined position),
   --  returns true on success

   function Load_Storage_Value (Position : Natural) return Integer;
   --  Load integer value from storage file (from defined position)

   procedure Open_URL (Url : String);
   --  Open URL with default system browser (if available)

   function Is_Key_Pressed (Key : Keyboard_Key) return Boolean;
   --  Check if a key has been pressed once

   function Is_Key_Down (Key : Keyboard_Key) return Boolean;
   --  Check if a key is being pressed

   function Is_Key_Released (Key : Keyboard_Key) return Boolean;
   --  Check if a key has been released once

   function Is_Key_Up (Key : Keyboard_Key) return Boolean;
   --  Check if a key is NOT being pressed

   procedure Set_Exit_Key (Key : Keyboard_Key);
   --  Set a custom key to exit program (default is ESC)

   function Get_Key_Pressed return Keyboard_Key;
   --  Get key pressed (keycode), call it multiple times for keys queued,
   --  returns Key_Null when the queue is empty

   function Get_Char_Pressed return Character;
   --  Get char pressed (unicode), call it multiple times for chars queued,
   --  returns ASCII.NUL when the queue is empty

   function Is_Gamepad_Available (Gamepad : Gamepad_Id) return Boolean;
   --  Check if a gamepad is available

   function Get_Gamepad_Name (Gamepad : Gamepad_Id) return String;
   --  Get gamepad internal name id

   function Is_Gamepad_Button_Pressed
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean;
   --  Check if a gamepad button has been pressed once

   function Is_Gamepad_Button_Down
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean;
   --  Check if a gamepad button is being pressed

   function Is_Gamepad_Button_Released
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean;
   --  Check if a gamepad button has been released once

   function Is_Gamepad_Button_Up
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean;
   --  Check if a gamepad button is NOT being pressed

   function Get_Gamepad_Button_Pressed return Gamepad_Button;
   --  Get the last gamepad button pressed

   function Get_Gamepad_Axis_Count (Gamepad : Gamepad_Id) return Integer;
   --  Get gamepad axis count for a gamepad

   function Get_Gamepad_Axis_Movement
     (Gamepad : Gamepad_Id; Axis : Gamepad_Axis) return Float;
   --  Get axis movement value for a gamepad axis

   function Set_Gamepad_Mappings (Mappings : String) return Integer;
   --  Set internal gamepad mappings (SDL_GameControllerDB)

   function Is_Mouse_Button_Pressed (Button : Mouse_Button) return Boolean;
   --  Check if a mouse button has been pressed once

   function Is_Mouse_Button_Down (Button : Mouse_Button) return Boolean;
   --  Check if a mouse button is being pressed

   function Is_Mouse_Button_Released (Button : Mouse_Button) return Boolean;
   --  Check if a mouse button has been released once

   function Is_Mouse_Button_Up (Button : Mouse_Button) return Boolean;
   --  Check if a mouse button is NOT being pressed

   function Get_Mouse_X return Natural;
   --  Get mouse position X

   function Get_Mouse_Y return Natural;
   --  Get mouse position Y

   function Get_Mouse_Position return RayLib.Vector2;
   --  Get mouse position XY

   function Get_Mouse_Delta return RayLib.Vector2;
   --  Get mouse delta between frames

   procedure Set_Mouse_Position (X : Natural; Y : Natural);
   --  Set mouse position XY

   procedure Set_Mouse_Offset (Offset_X : Integer; Offset_Y : Integer);
   --  Set mouse offset

   procedure Set_Mouse_Scale (Scale_X : Float; Scale_Y : Float);
   --  Set mouse scaling

   function Get_Mouse_Wheel_Move return Float;
   --  Get mouse wheel movement Y

   procedure Set_Mouse_Cursor (Cursor : Mouse_Cursor);
   --  Set mouse cursor

   function Get_Touch_X return Natural;
   --  Get touch position X for touch point 0 (relative to screen size)

   function Get_Touch_Y return Natural;
   --  Get touch position Y for touch point 0 (relative to screen size)

   function Get_Touch_Position (Index : Integer) return RayLib.Vector2;
   --  Get touch position XY for a touch point index (relative to screen size)

   function Get_Touch_Point_Id (Index : Integer) return Integer;
   --  Get touch point identifier for given index

   function Get_Touch_Point_Count return Integer;
   --  Get number of touch points

   procedure Set_Gestures_Enabled (Flags : RayLib.Gesture);
   --  Enable a set of gestures using flags

   function Is_Gesture_Detected (Gesture : RayLib.Gesture) return Boolean;
   --  Check if a gesture have been detected

   function Get_Gesture_Detected return RayLib.Gesture;
   --  Get latest detected gesture

   function Get_Gesture_Hold_Duration return Float;
   --  Get gesture hold time in milliseconds

   function Get_Gesture_Drag_Vector return RayLib.Vector2;
   --  Get gesture drag vector

   function Get_Gesture_Drag_Angle return Float;
   --  Get gesture drag angle

   function Get_Gesture_Pinch_Vector return RayLib.Vector2;
   --  Get gesture pinch delta

   function Get_Gesture_Pinch_Angle return Float;
   --  Get gesture pinch angle

   procedure Set_Camera_Mode (Camera : RayLib.Camera; Mode : Camera_Mode);
   --  Set camera mode (multiple camera modes available)

   procedure Update_Camera (Camera : in out RayLib.Camera);
   --  Update camera position for selected mode

   procedure Set_Camera_Pan_Control (Key_Pan : Keyboard_Key);
   --  Set camera pan key to combine with mouse movement (free camera)

   procedure Set_Camera_Alt_Control (Key_Alt : Keyboard_Key);
   --  Set camera alt key to combine with mouse movement (free camera)

   procedure Set_Camera_Smooth_Zoom_Control (Key_Smooth_Zoom : Keyboard_Key);
   --  Set camera smooth zoom key to combine with mouse (free camera)

   procedure Set_Camera_Move_Controls
     (Key_Front : Keyboard_Key; Key_Back : Keyboard_Key;
      Key_Right : Keyboard_Key; Key_Left : Keyboard_Key; Key_Up : Keyboard_Key;
      Key_Down  : Keyboard_Key);
   --  Set camera move controls (1st person and 3rd person cameras)

   procedure Set_Shapes_Texture
     (Texture : RayLib.Texture2D; Source : RayLib.Rectangle);
   --  Set texture and rectangle to be used on shapes drawing

   procedure Draw_Pixel
     (Pos_X : Natural; Pos_Y : Natural; Color : RayLib.Color);
   --  Draw a pixel

   procedure Draw_Pixel (Position : RayLib.Vector2; Color : RayLib.Color);
   --  Draw a pixel (Vector version)

   procedure Draw_Line
     (Start_Pos_X : Natural; Start_Pos_Y : Natural; End_Pos_X : Natural;
      End_Pos_Y   : Natural; Color : RayLib.Color);
   --  Draw a line

   procedure Draw_Line
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Color     : RayLib.Color);
   --  Draw a line (Vector version)

   procedure Draw_Line
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2; Thick : Float;
      Color     : RayLib.Color);
   --  Draw a line defining thickness

   procedure Draw_Line_Bezier
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2; Thick : Float;
      Color     : RayLib.Color);
   --  Draw a line using cubic-bezier curves in-out

   procedure Draw_Line_Bezier_Quad
     (Start_Pos   : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Control_Pos : RayLib.Vector2; Thick : Float; Color : RayLib.Color);
   --  Draw line using quadratic bezier curves with a control point

   procedure Draw_Line_Bezier_Cubic
     (Start_Pos         : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Start_Control_Pos : RayLib.Vector2; End_Control_Pos : RayLib.Vector2;
      Thick             : Float; Color : RayLib.Color);
   --  Draw line using cubic bezier curves with 2 control points

   procedure Draw_Line_Strip
     (Points : RayLib.Vector2_Array; Color : RayLib.Color);
   --  Draw lines sequence

   procedure Draw_Circle
     (Center_X : Natural; Center_Y : Natural; Radius : Float;
      Color    : RayLib.Color);
   --  Draw a color-filled circle

   procedure Draw_Circle_Sector
     (Center    : RayLib.Vector2; Radius : Float; Start_Angle : Float;
      End_Angle : Float; Segments : Natural; Color : RayLib.Color);
   --  Draw a piece of a circle

   procedure Draw_Circle_Sector_Lines
     (Center    : RayLib.Vector2; Radius : Float; Start_Angle : Float;
      End_Angle : Float; Segments : Natural; Color : RayLib.Color);
   --  Draw circle sector outline

   procedure Draw_Circle_Gradient
     (Center_X : Natural; Center_Y : Natural; Radius : Float;
      Color1   : RayLib.Color; Color2 : RayLib.Color);
   --  Draw a gradient-filled circle

   procedure Draw_Circle
     (Center : RayLib.Vector2; Radius : Float; Color : RayLib.Color);
   --  Draw a color-filled circle (Vector version)

   procedure Draw_Circle_Lines
     (Center_X : Natural; Center_Y : Natural; Radius : Float;
      Color    : RayLib.Color);
   --  Draw circle outline

   procedure Draw_Ellipse
     (Center_X : Natural; Center_Y : Natural; Radius_H : Float;
      Radius_V : Float; Color : RayLib.Color);
   --  Draw ellipse

   procedure Draw_Ellipse_Lines
     (Center_X : Natural; Center_Y : Natural; Radius_H : Float;
      Radius_V : Float; Color : RayLib.Color);
   --  Draw ellipse outline

   procedure Draw_Ring
     (Center      : RayLib.Vector2; Inner_Radius : Float; Outer_Radius : Float;
      Start_Angle : Float; End_Angle : Float; Segments : Natural;
      Color       : RayLib.Color);
   --  Draw ring

   procedure Draw_Ring_Lines
     (Center      : RayLib.Vector2; Inner_Radius : Float; Outer_Radius : Float;
      Start_Angle : Float; End_Angle : Float; Segments : Integer;
      Color       : RayLib.Color);
   --  Draw ring outline

   procedure Draw_Rectangle
     (Pos_X : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color : RayLib.Color);
   --  Draw a color-filled rectangle

   procedure Draw_Rectangle
     (Position : RayLib.Vector2; Size : RayLib.Vector2; Color : RayLib.Color);
   --  Draw a color-filled rectangle (Vector version)

   procedure Draw_Rectangle (Rec : RayLib.Rectangle; Color : RayLib.Color);
   --  Draw a color-filled rectangle

   procedure Draw_Rectangle
     (Rec   : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Color : RayLib.Color);
   --  Draw a color-filled rectangle with pro parameters

   procedure Draw_Rectangle_Gradient_Vertical
     (Pos_X  : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color1 : RayLib.Color; Color2 : RayLib.Color);
   --  Draw a vertical-gradient-filled rectangle

   procedure Draw_Rectangle_Gradient_Horizontal
     (Pos_X  : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color1 : RayLib.Color; Color2 : RayLib.Color);
   --  Draw a horizontal-gradient-filled rectangle

   procedure Draw_Rectangle_Gradient
     (Rec  : RayLib.Rectangle; Col1 : RayLib.Color; Col2 : RayLib.Color;
      Col3 : RayLib.Color; Col4 : RayLib.Color);
   --  Draw a gradient-filled rectangle with custom vertex colors

   procedure Draw_Rectangle_Lines
     (Pos_X : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color : RayLib.Color);
   --  Draw rectangle outline

   procedure Draw_Rectangle_Lines
     (Rec : RayLib.Rectangle; Line_Thick : Float; Color : RayLib.Color);
   --  Draw rectangle outline with extended parameters

   procedure Draw_Rectangle_Rounded
     (Rec   : RayLib.Rectangle; Roundness : Float; Segments : Natural;
      Color : RayLib.Color);
   --  Draw rectangle with rounded edges

   procedure Draw_Rectangle_Rounded_Lines
     (Rec        : RayLib.Rectangle; Roundness : Float; Segments : Natural;
      Line_Thick : Float; Color : RayLib.Color);
   --  Draw rectangle with rounded edges outline

   procedure Draw_Triangle
     (V1    : RayLib.Vector2; V2 : RayLib.Vector2; V3 : RayLib.Vector2;
      Color : RayLib.Color);
   --  Draw a color-filled triangle (vertex in counter-clockwise order!)

   procedure Draw_Triangle_Lines
     (V1    : RayLib.Vector2; V2 : RayLib.Vector2; V3 : RayLib.Vector2;
      Color : RayLib.Color);
   --  Draw triangle outline (vertex in counter-clockwise order!)

   procedure Draw_Triangle_Fan
     (Points : RayLib.Vector2_Array; Color : RayLib.Color);
   --  Draw a triangle fan defined by points (first vertex is the center)

   procedure Draw_Triangle_Strip
     (Points : RayLib.Vector2_Array; Color : RayLib.Color);
   --  Draw a triangle strip defined by points

   procedure Draw_Poly
     (Center   : RayLib.Vector2; Sides : Natural; Radius : Float;
      Rotation : Float; Color : RayLib.Color);
   --  Draw a regular polygon (Vector version)

   procedure Draw_Poly_Lines
     (Center   : RayLib.Vector2; Sides : Natural; Radius : Float;
      Rotation : Float; Color : RayLib.Color);
   --  Draw a polygon outline of n sides

   procedure Draw_Poly_Lines
     (Center   : RayLib.Vector2; Sides : Natural; Radius : Float;
      Rotation : Float; Line_Thick : Float; Color : RayLib.Color);
   --  Draw a polygon outline of n sides with extended parameters

   function Check_Collision_Recs
     (Rec1 : RayLib.Rectangle; Rec2 : RayLib.Rectangle) return Boolean;
   --  Check collision between two rectangles

   function Check_Collision_Circles
     (Center1 : RayLib.Vector2; Radius1 : Float; Center2 : RayLib.Vector2;
      Radius2 : Float) return Boolean;
   --  Check collision between two circles

   function Check_Collision_Circle_Rec
     (Center : RayLib.Vector2; Radius : Float; Rec : RayLib.Rectangle)
      return Boolean;
   --  Check collision between circle and rectangle

   function Check_Collision_Point_Rec
     (Point : RayLib.Vector2; Rec : RayLib.Rectangle) return Boolean;
   --  Check if point is inside rectangle

   function Check_Collision_Point_Circle
     (Point : RayLib.Vector2; Center : RayLib.Vector2; Radius : Float)
      return Boolean;
   --  Check if point is inside circle

   function Check_Collision_Point_Triangle
     (Point : RayLib.Vector2; P1 : RayLib.Vector2; P2 : RayLib.Vector2;
      P3    : RayLib.Vector2) return Boolean;
   --  Check if point is inside a triangle

   function Check_Collision_Lines
     (Start_Pos1      :     RayLib.Vector2; End_Pos1 : RayLib.Vector2;
      Start_Pos2      :     RayLib.Vector2; End_Pos2 : RayLib.Vector2;
      Collision_Point : out RayLib.Vector2) return Boolean;
   --  Check the collision between two lines defined by two points each,
   --  returns collision point by reference

   function Check_Collision_Point_Line
     (Point     : RayLib.Vector2; P1 : RayLib.Vector2; P2 : RayLib.Vector2;
      Threshold : Natural) return Boolean;
   --  Check if point belongs to line created between two points [p1] and [p2]
   --  with defined margin in pixels [threshold]

   function Get_Collision_Rec
     (Rec1 : RayLib.Rectangle; Rec2 : RayLib.Rectangle)
      return RayLib.Rectangle;
   --  Get collision rectangle for two rectangles collision

   function Load_Image (File_Name : String) return RayLib.Image;
   --  Load image from file into CPU memory (RAM)

   function Load_Image_Raw
     (File_Name : String; Width : Natural; Height : Natural;
      Format    : Pixel_Format; Header_Size : Natural) return RayLib.Image;
   --  Load image from RAW file data

   function Load_Image_Anim
     (File_Name : String; Frames : out Natural) return RayLib.Image;
   --  Load image sequence from file (frames appended to image.data)

   function Load_Image_From_Memory
     (File_Type : String; File_Data : Stream_Element_Array)
      return RayLib.Image;
   --  Load image from memory buffer, fileType refers to extension: i.e. '.png'

   function Load_Image_From_Texture
     (Texture : RayLib.Texture2D'Class) return RayLib.Image'Class;
   --  Load image from GPU texture data

   function Load_Image_From_Screen return RayLib.Image;
   --  Load image from screen buffer and (screenshot)

   function Export_Image
     (Image : RayLib.Image; File_Name : String) return Boolean;
   --  Export image data to file, returns true on success

   function Export_Image_As_Code
     (Image : RayLib.Image; File_Name : String) return Boolean;
   --  Export image as code file defining an array of bytes,
   --  returns true on success

   function Gen_Image_Color
     (Width : Natural; Height : Natural; Color : RayLib.Color)
      return RayLib.Image;
   --  Generate image: plain color

   function Gen_Image_Gradient_Vertical
     (Width  : Natural; Height : Natural; Top : RayLib.Color;
      Bottom : RayLib.Color) return RayLib.Image;
   --  Generate image: vertical gradient

   function Gen_Image_Gradient_Horizontal
     (Width : Natural; Height : Natural; Left : RayLib.Color;
      Right : RayLib.Color) return RayLib.Image;
   --  Generate image: horizontal gradient

   function Gen_Image_Gradient_Radial
     (Width : Natural; Height : Natural; Density : Float; Inner : RayLib.Color;
      Outer : RayLib.Color) return RayLib.Image;
   --  Generate image: radial gradient

   function Gen_Image_Checked
     (Width    : Natural; Height : Natural; Checks_X : Natural;
      Checks_Y : Natural; Col1 : RayLib.Color; Col2 : RayLib.Color)
      return RayLib.Image;
   --  Generate image: checked

   function Gen_Image_White_Noise
     (Width : Natural; Height : Natural; Factor : Float) return RayLib.Image;
   --  Generate image: white noise

   function Gen_Image_Cellular
     (Width : Natural; Height : Natural; Tile_Size : Natural)
      return RayLib.Image;
   --  Generate image: cellular algorithm, bigger tileSize means bigger cells

   function Image_Copy (Image : RayLib.Image) return RayLib.Image;
   --  Create an image duplicate (useful for transformations)

   function Image_From_Image
     (Image : RayLib.Image; Rec : RayLib.Rectangle) return RayLib.Image;
   --  Create an image from another image piece

   function Image_Text
     (Text : String; Font_Size : Natural; Color : RayLib.Color)
      return RayLib.Image;
   --  Create an image from text (default font)

   function Image_Text
     (Font    : RayLib.Font'Class; Text : String; Font_Size : Float;
      Spacing : Float; Tint : RayLib.Color) return RayLib.Image'Class;
   --  Create an image from text (custom sprite font)

   procedure Image_Format
     (Image : in out RayLib.Image; New_Format : Pixel_Format);
   --  Convert image data to desired format

   procedure Image_To_POT (Image : in out RayLib.Image; Fill : RayLib.Color);
   --  Convert image to POT (power-of-two)

   procedure Image_Crop (Image : in out RayLib.Image; Crop : RayLib.Rectangle);
   --  Crop an image to a defined rectangle

   procedure Image_Alpha_Crop (Image : in out RayLib.Image; Threshold : Float);
   --  Crop image depending on alpha value

   procedure Image_Alpha_Clear
     (Image : in out RayLib.Image; Color : RayLib.Color; Threshold : Float);
   --  Clear alpha channel to desired color

   procedure Image_Alpha_Mask
     (Image : in out RayLib.Image; Alpha_Mask : RayLib.Image);
   --  Apply alpha mask to image

   procedure Image_Alpha_Premultiply (Image : in out RayLib.Image);
   --  Premultiply alpha channel

   procedure Image_Resize
     (Image : in out RayLib.Image; New_Width : Natural; New_Height : Natural);
   --  Resize image (Bicubic scaling algorithm)

   procedure Image_Resize_NN
     (Image : in out RayLib.Image; New_Width : Natural; New_Height : Natural);
   --  Resize image (Nearest-Neighbor scaling algorithm)

   procedure Image_Resize_Canvas
     (Image : in out RayLib.Image; New_Width : Natural; New_Height : Natural;
      Offset_X :        Natural; Offset_Y : Natural; Fill : RayLib.Color);
   --  Resize canvas and fill with color

   procedure Image_Mipmaps (Image : in out RayLib.Image);
   --  Compute all mipmap levels for a provided image

   procedure Image_Dither
     (Image : in out RayLib.Image; R_Bpp : Natural; G_Bpp : Natural;
      B_Bpp :        Natural; A_Bpp : Natural);
   --  Dither image data to 16bpp or lower (Floyd-Steinberg dithering)

   procedure Image_Flip_Vertical (Image : in out RayLib.Image);
   --  Flip image vertically

   procedure Image_Flip_Horizontal (Image : in out RayLib.Image);
   --  Flip image horizontally

   procedure Image_Rotate_CW (Image : in out RayLib.Image);
   --  Rotate image clockwise 90deg

   procedure Image_Rotate_CCW (Image : in out RayLib.Image);
   --  Rotate image counter-clockwise 90deg

   procedure Image_Color_Tint
     (Image : in out RayLib.Image; Color : RayLib.Color);
   --  Modify image color: tint

   procedure Image_Color_Invert (Image : in out RayLib.Image);
   --  Modify image color: invert

   procedure Image_Color_Grayscale (Image : in out RayLib.Image);
   --  Modify image color: grayscale

   procedure Image_Color_Contrast
     (Image : in out RayLib.Image; Contrast : Float);
   --  Modify image color: contrast (-100 to 100)

   procedure Image_Color_Brightness
     (Image : in out RayLib.Image; Brightness : Integer);
   --  Modify image color: brightness (-255 to 255)

   procedure Image_Color_Replace
     (Image   : in out RayLib.Image; Color : RayLib.Color;
      Replace :        RayLib.Color);
   --  Modify image color: replace color

   function Load_Image_Colors (Image : RayLib.Image) return RayLib.Color_Array;
   --  Load color data from image as a Color array (RGBA - 32bit)

   function Load_Image_Palette
     (Image : RayLib.Image; Max_Palette_Size : Natural)
      return RayLib.Color_Array;
   --  Load colors palette from image as a Color array (RGBA - 32bit)

   function Get_Image_Alpha_Border
     (Image : RayLib.Image; Threshold : Float) return RayLib.Rectangle;
   --  Get image alpha border rectangle

   function Get_Image_Color
     (Image : RayLib.Image; X : Natural; Y : Natural) return RayLib.Color;
   --  Get image pixel color at (x, y) position

   procedure Image_Clear_Background
     (Dst : in out RayLib.Image; Color : RayLib.Color);
   --  Clear image background with given color

   procedure Image_Draw_Pixel
     (Dst   : in out RayLib.Image; Pos_X : Natural; Pos_Y : Natural;
      Color :        RayLib.Color);
   --  Draw pixel within an image

   procedure Image_Draw_Pixel
     (Dst   : in out RayLib.Image; Position : RayLib.Vector2;
      Color :        RayLib.Color);
   --  Draw pixel within an image (Vector version)

   procedure Image_Draw_Line
     (Dst : in out RayLib.Image; Start_Pos_X : Natural; Start_Pos_Y : Natural;
      End_Pos_X :        Natural; End_Pos_Y : Natural; Color : RayLib.Color);
   --  Draw line within an image

   procedure Image_Draw_Line
     (Dst    : in out RayLib.Image; Start : RayLib.Vector2;
      Finish :        RayLib.Vector2; Color : RayLib.Color);
   --  Draw line within an image (Vector version)

   procedure Image_Draw_Circle
     (Dst    : in out RayLib.Image; Center_X : Natural; Center_Y : Natural;
      Radius :        Natural; Color : RayLib.Color);
   --  Draw circle within an image

   procedure Image_Draw_Circle
     (Dst   : in out RayLib.Image; Center : RayLib.Vector2; Radius : Integer;
      Color :        RayLib.Color);
   --  Draw circle within an image (Vector version)

   procedure Image_Draw_Rectangle
     (Dst   : in out RayLib.Image; Pos_X : Natural; Pos_Y : Natural;
      Width :        Natural; Height : Natural; Color : RayLib.Color);
   --  Draw rectangle within an image

   procedure Image_Draw_Rectangle
     (Dst  : in out RayLib.Image; Position : RayLib.Vector2;
      Size :        RayLib.Vector2; Color : RayLib.Color);
   --  Draw rectangle within an image (Vector version)

   procedure Image_Draw_Rectangle
     (Dst : in out RayLib.Image; Rec : RayLib.Rectangle; Color : RayLib.Color);
   --  Draw rectangle within an image

   procedure Image_Draw_Rectangle_Lines
     (Dst   : in out RayLib.Image; Rec : RayLib.Rectangle; Thick : Natural;
      Color :        RayLib.Color);
   --  Draw rectangle lines within an image

   procedure Image_Draw
     (Dst     : in out RayLib.Image; Src : RayLib.Image;
      Src_Rec :        RayLib.Rectangle; Dst_Rec : RayLib.Rectangle;
      Tint    :        RayLib.Color);
   --  Draw a source image within a destination image (tint applied to source)

   procedure Image_Draw_Text
     (Dst   : in out RayLib.Image; Text : String; Pos_X : Natural;
      Pos_Y :        Natural; Font_Size : Natural; Color : RayLib.Color);
   --  Draw text (using default font) within an image (destination)

   procedure Image_Draw_Text
     (Dst : in out RayLib.Image'Class; Font : RayLib.Font'Class; Text : String;
      Position :        RayLib.Vector2; Font_Size : Float; Spacing : Float;
      Tint     :        RayLib.Color);
   --  Draw text (custom sprite font) within an image (destination)

   function Load_Texture (File_Name : String) return RayLib.Texture2D;
   --  Load texture from file into GPU memory (VRAM)

   function Load_Texture_From_Image
     (Image : RayLib.Image'Class) return RayLib.Texture2D'Class;
   --  Load texture from image data

   function Load_Texture_Cubemap
     (Image : RayLib.Image'Class; Layout : Cubemap_Layout)
      return RayLib.Texture_Cubemap'Class;
   --  Load cubemap from image, multiple image cubemap layouts supported

   function Load_Render_Texture
     (Width : Natural; Height : Natural) return RayLib.Render_Texture2D;
   --  Load texture for rendering (framebuffer)

   procedure Update_Texture
     (Texture : RayLib.Texture2D; Pixels : Stream_Element_Array);
   --  Update GPU texture with new data

   procedure Update_Texture
     (Texture : RayLib.Texture2D; Rec : RayLib.Rectangle;
      Pixels  : Stream_Element_Array);
   --  Update GPU texture rectangle with new data

   procedure Gen_Texture_Mipmaps (Texture : in out RayLib.Texture2D);
   --  Generate GPU mipmaps for a texture

   procedure Set_Texture_Filter
     (Texture : RayLib.Texture2D; Filter : Texture_Filter);
   --  Set texture scaling filter mode

   procedure Set_Texture_Wrap
     (Texture : RayLib.Texture2D; Wrap : Texture_Wrap);
   --  Set texture wrapping mode

   procedure Draw_Texture
     (Texture : RayLib.Texture2D; Pos_X : Natural; Pos_Y : Natural;
      Tint    : RayLib.Color);
   --  Draw a Texture2D

   procedure Draw_Texture
     (Texture : RayLib.Texture2D; Position : RayLib.Vector2;
      Tint    : RayLib.Color);
   --  Draw a Texture2D with position defined as Vector2

   procedure Draw_Texture
     (Texture : RayLib.Texture2D; Position : RayLib.Vector2; Rotation : Float;
      Scale   : Float; Tint : RayLib.Color);
   --  Draw a Texture2D with extended parameters

   procedure Draw_Texture
     (Texture  : RayLib.Texture2D; Source : RayLib.Rectangle;
      Position : RayLib.Vector2; Tint : RayLib.Color);
   --  Draw a part of a texture defined by a rectangle

   procedure Draw_Texture_Quad
     (Texture : RayLib.Texture2D; Tiling : RayLib.Vector2;
      Offset  : RayLib.Vector2; Quad : RayLib.Rectangle; Tint : RayLib.Color);
   --  Draw texture quad with tiling and offset parameters

   procedure Draw_Texture_Tiled
     (Texture : RayLib.Texture2D; Source : RayLib.Rectangle;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Scale   : Float; Tint : RayLib.Color);
   --  Draw part of a texture (defined by a rectangle) with rotation
   --  and scale tiled into dest.

   procedure Draw_Texture_Pro
     (Texture : RayLib.Texture2D; Source : RayLib.Rectangle;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Tint    : RayLib.Color);
   --  Draw a part of a texture defined by a rectangle with 'pro' parameters

   procedure Draw_Texture_N_Patch
     (Texture : RayLib.Texture2D; N_Patch_Info : RayLib.N_Patch_Info;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Tint    : RayLib.Color);
   --  Draws a texture (or part of it) that stretches or shrinks nicely

   procedure Draw_Texture_Poly
     (Texture : RayLib.Texture2D; Center : RayLib.Vector2;
      Points  : RayLib.Vector2_Array; Texcoords : RayLib.Vector2_Array;
      Tint    : RayLib.Color);
   --  Draw a textured polygon

   function Fade (Color : RayLib.Color; Alpha : Float) return RayLib.Color;
   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f

   function Color_To_Int (Color : RayLib.Color) return Integer;
   --  Get hexadecimal value for a Color

   function Color_Normalize (Color : RayLib.Color) return RayLib.Vector4;
   --  Get Color normalized as float [0..1]

   function Color_From_Normalized
     (Normalized : RayLib.Vector4) return RayLib.Color;
   --  Get Color from normalized values [0..1]

   function Color_To_HSV (Color : RayLib.Color) return RayLib.Vector3;
   --  Get HSV values for a Color, hue [0..360], saturation/value [0..1]

   function Color_From_HSV
     (Hue : Float; Saturation : Float; Value : Float) return RayLib.Color;
   --  Get a Color from HSV values, hue [0..360], saturation/value [0..1]

   function Color_Alpha
     (Color : RayLib.Color; Alpha : Float) return RayLib.Color;
   --  Get color with alpha applied, alpha goes from 0.0f to 1.0f

   function Color_Alpha_Blend
     (Dst : RayLib.Color; Src : RayLib.Color; Tint : RayLib.Color)
      return RayLib.Color;
   --  Get src alpha-blended into dst color with tint

   function Get_Color (Hex_Value : Natural) return RayLib.Color;
   --  Get Color structure from hexadecimal value

   function Get_Pixel_Color
     (Src_Ptr : System.Address; Format : Pixel_Format) return RayLib.Color;
   --  Get Color from a source pixel pointer of certain format

   procedure Set_Pixel_Color
     (Dst_Ptr : System.Address; Color : RayLib.Color; Format : Pixel_Format);
   --  Set color formatted into destination pixel pointer

   function Get_Pixel_Data_Size
     (Width : Natural; Height : Natural; Format : Pixel_Format) return Natural;
   --  Get pixel data size in bytes for certain format

   function Get_Font_Default return RayLib.Font;
   --  Get the default Font

   function Load_Font (File_Name : String) return RayLib.Font;
   --  Load font from file into GPU memory (VRAM)

   --  function Load_Font_Ex (File_Name : String; Font_Size : Integer;
   --  Font_Chars : RayLib.Int *; Glyph_Count : Integer) return RayLib.Font;
   --  Load font from file with extended parameters, use NULL for fontChars
   --  and 0 for glyphCount to load the default character set

   function Load_Font_From_Image
     (Image : RayLib.Image'Class; Key : RayLib.Color; First_Char : Integer)
      return RayLib.Font'Class;
   --  Load font from Image (XNA style)

   --  Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
   --  function Load_Font_From_Memory (File_Type : String;
   --  File_Data : RayLib.Const unsigned char *; Data_Size : Integer;
   --  Font_Size : Integer; Font_Chars : RayLib.Int *; Glyph_Count : Integer)
   --    return RayLib.Font;

   --  Load font data for further use
   --  function Load_Font_Data (File_Data : RayLib.Const unsigned char *;
   --  Data_Size : Integer; Font_Size : Integer; Font_Chars : RayLib.Int *;
   --  Glyph_Count : Integer; Type : Integer) return RayLib.Glyph_Info *;

   --  Generate image font atlas using chars info
   --  function Gen_Image_Font_Atlas (Chars : RayLib.Const _Glyph_Info *;
   --  Recs : RayLib.Rectangle **; Glyph_Count : Integer; Font_Size : Integer;
   --  Padding : Integer; Pack_Method : Integer) return RayLib.Image;

   function Export_Font_As_Code
     (Font : RayLib.Font; File_Name : String) return Boolean;
   --  Export font as code file, returns true on success

   procedure Draw_FPS (Pos_X : Natural; Pos_Y : Natural);
   --  Draw current FPS

   procedure Draw_Text
     (Text  : String; Pos_X : Natural; Pos_Y : Natural; Font_Size : Natural;
      Color : RayLib.Color);
   --  Draw text (using default font)

   procedure Draw_Text
     (Font      : RayLib.Font; Text : String; Position : RayLib.Vector2;
      Font_Size : Float; Spacing : Float; Tint : RayLib.Color);
   --  Draw text using font and additional parameters

   procedure Draw_Text
     (Font    : RayLib.Font; Text : String; Position : RayLib.Vector2;
      Origin  : RayLib.Vector2; Rotation : Float; Font_Size : Float;
      Spacing : Float; Tint : RayLib.Color);
   --  Draw text using Font and pro parameters (rotation)

   procedure Draw_Text_Codepoint
     (Font      : RayLib.Font; Codepoint : Integer; Position : RayLib.Vector2;
      Font_Size : Float; Tint : RayLib.Color);
   --  Draw one character (codepoint)

   procedure Draw_Text_Codepoints
     (Font     : RayLib.Font; Codepoints : Integer_Array;
      Position : RayLib.Vector2; Font_Size : Float; Spacing : Float;
      Tint     : RayLib.Color);
   --  Draw multiple character (codepoint)

   function Measure_Text (Text : String; Font_Size : Integer) return Integer;
   --  Measure string width for default font

   function Measure_Text
     (Font : RayLib.Font; Text : String; Font_Size : Float; Spacing : Float)
      return RayLib.Vector2;
   --  Measure string size for Font

   function Get_Glyph_Index
     (Font : RayLib.Font; Codepoint : Integer) return Integer;
   --  Get glyph index position in font for a codepoint (unicode character),
   --  fallback to '?' if not found

   function Get_Glyph_Info
     (Font : RayLib.Font; Codepoint : Integer) return RayLib.Glyph_Info;
   --  Get glyph font info data for a codepoint (unicode character),
   --  fallback to '?' if not found

   function Get_Glyph_Atlas
     (Font : RayLib.Font; Codepoint : Integer) return RayLib.Rectangle;
   --  Get glyph rectangle in font atlas for a codepoint (unicode character),
   --  fallback to '?' if not found

   function Load_Codepoints (Text : String) return Integer_Array;
   --  Load all codepoints from a UTF-8 text string,
   --  codepoints count returned by parameter

   function Get_Codepoint_Count (Text : String) return Natural;
   --  Get total number of codepoints in a UTF-8 encoded string

   function Get_Codepoint
     (Text : String; Bytes_Processed : out Integer) return Integer;
   --  Get next codepoint in a UTF-8 encoded string,
   --  0x3f('?') is returned on failure

   function Codepoint_To_UTF8 (Codepoint : Integer) return String;
   --  Encode one codepoint into UTF-8 byte array
   --  (array length returned as parameter)

   function Text_Codepoints_To_UTF8 (Codepoints : Integer_Array) return String;
   --  Encode text as codepoints array into UTF-8 text string

   procedure Text_Copy (Dst : out String; Src : String);
   --  Copy one string to another, returns bytes copied

   function Text_Is_Equal (Text1 : String; Text2 : String) return Boolean;
   --  Check if two text string are equal

   function Text_Length (Text : String) return Natural;
   --  Get text length, checks for '\0' ending

   function Text_Subtext
     (Text : String; Position : Integer; Length : Integer) return String;
   --  Get a piece of a text string

   function Text_Replace
     (Text : in out String; Replace : String; By : String) return String;
   --  Replace text string (WARNING: memory must be freed!)

   function Text_Insert
     (Text : String; Insert : String; Position : Integer) return String;
   --  Insert text in a position (WARNING: memory must be freed!)

   function Text_Join
     (Text_List : Unbounded_String_Array; Delimiter : String) return String;
   --  Join text strings with delimiter

   function Text_Split
     (Text : String; Delimiter : Character) return Unbounded_String_Array;
   -- Split text into multiple strings

   procedure Text_Append
     (Text : Unbounded_String; Append : String; Position : out Positive);
   --  Append text at specific position and move cursor!

   function Text_Find_Index (Text : String; Find : String) return Integer;
   --  Find first text occurrence within a string

   function Text_To_Upper (Text : String) return String;
   --  Get upper case version of provided string

   function Text_To_Lower (Text : String) return String;
   --  Get lower case version of provided string

   function Text_To_Pascal (Text : String) return String;
   --  Get Pascal case notation version of provided string

   function Text_To_Integer (Text : String) return Integer;
   --  Get integer value from text (negative values not supported)

   procedure Draw_Line3D
     (Start_Pos : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Color     : RayLib.Color);
   --  Draw a line in 3D world space

   procedure Draw_Point3D (Position : RayLib.Vector3; Color : RayLib.Color);
   --  Draw a point in 3D space, actually a small line

   procedure Draw_Circle3D
     (Center : RayLib.Vector3; Radius : Float; Rotation_Axis : RayLib.Vector3;
      Rotation_Angle : Float; Color : RayLib.Color);
   --  Draw a circle in 3D world space

   procedure Draw_Triangle3D
     (V1    : RayLib.Vector3; V2 : RayLib.Vector3; V3 : RayLib.Vector3;
      Color : RayLib.Color);
   --  Draw a color-filled triangle (vertex in counter-clockwise order!)

   procedure Draw_Triangle_Strip3D
     (Points : Vector3_Array; Color : RayLib.Color);
   --  Draw a triangle strip defined by points

   procedure Draw_Cube
     (Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color);
   --  Draw cube

   procedure Draw_Cube
     (Position : RayLib.Vector3; Size : RayLib.Vector3; Color : RayLib.Color);
   --  Draw cube (Vector version)

   procedure Draw_Cube_Wires
     (Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color);
   --  Draw cube wires

   procedure Draw_Cube_Wires
     (Position : RayLib.Vector3; Size : RayLib.Vector3; Color : RayLib.Color);
   --  Draw cube wires (Vector version)

   procedure Draw_Cube_Texture
     (Texture : RayLib.Texture2D; Position : RayLib.Vector3; Width : Float;
      Height  : Float; Length : Float; Color : RayLib.Color);
   --  Draw cube textured

   procedure Draw_Cube_Texture
     (Texture  : RayLib.Texture2D; Source : RayLib.Rectangle;
      Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color);
   --  Draw cube with a region of a texture

   procedure Draw_Sphere
     (Center_Pos : RayLib.Vector3; Radius : Float; Color : RayLib.Color);
   --  Draw sphere

   procedure Draw_Sphere
     (Center_Pos : RayLib.Vector3; Radius : Float; Rings : Integer;
      Slices     : Integer; Color : RayLib.Color);
   --  Draw sphere with extended parameters

   procedure Draw_Sphere_Wires
     (Center_Pos : RayLib.Vector3; Radius : Float; Rings : Integer;
      Slices     : Integer; Color : RayLib.Color);
   --  Draw sphere wires

   procedure Draw_Cylinder
     (Position : RayLib.Vector3; Radius_Top : Float; Radius_Bottom : Float;
      Height   : Float; Slices : Integer; Color : RayLib.Color);
   --  Draw a cylinder/cone

   procedure Draw_Cylinder
     (Start_Pos    : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Start_Radius : Float; End_Radius : Float; Sides : Integer;
      Color        : RayLib.Color);
   --  Draw a cylinder with base at startPos and top at endPos

   procedure Draw_Cylinder_Wires
     (Position : RayLib.Vector3; Radius_Top : Float; Radius_Bottom : Float;
      Height   : Float; Slices : Integer; Color : RayLib.Color);
   --  Draw a cylinder/cone wires

   procedure Draw_Cylinder_Wires
     (Start_Pos    : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Start_Radius : Float; End_Radius : Float; Sides : Integer;
      Color        : RayLib.Color);
   --  Draw a cylinder wires with base at startPos and top at endPos

   procedure Draw_Plane
     (Center_Pos : RayLib.Vector3; Size : RayLib.Vector2;
      Color      : RayLib.Color);
   --  Draw a plane XZ

   procedure Draw_Ray (Ray : RayLib.Ray; Color : RayLib.Color);
   --  Draw a ray line

   procedure Draw_Grid (Slices : Integer; Spacing : Float);
   --  Draw a grid (centered at (0, 0, 0))

   function Load_Model (File_Name : String) return RayLib.Model;
   --  Load model from files (meshes and materials)

   function Load_Model_From_Mesh
     (Mesh : RayLib.Mesh'Class) return RayLib.Model'Class;
   --  Load model from generated mesh (default material)

   function Get_Model_Bounding_Box
     (Model : RayLib.Model) return RayLib.Bounding_Box;
   --  Compute model bounding box limits (considers all meshes)

   procedure Draw_Model
     (Model : RayLib.Model; Position : RayLib.Vector3; Scale : Float;
      Tint  : RayLib.Color);
   --  Draw a model (with texture if set)

   procedure Draw_Model
     (Model         : RayLib.Model; Position : RayLib.Vector3;
      Rotation_Axis : RayLib.Vector3; Rotation_Angle : Float;
      Scale         : RayLib.Vector3; Tint : RayLib.Color);
   --  Draw a model with extended parameters

   procedure Draw_Model_Wires
     (Model : RayLib.Model; Position : RayLib.Vector3; Scale : Float;
      Tint  : RayLib.Color);
   --  Draw a model wires (with texture if set)

   procedure Draw_Model_Wires
     (Model         : RayLib.Model; Position : RayLib.Vector3;
      Rotation_Axis : RayLib.Vector3; Rotation_Angle : Float;
      Scale         : RayLib.Vector3; Tint : RayLib.Color);
   --  Draw a model wires (with texture if set) with extended parameters

   procedure Draw_Bounding_Box
     (Box : RayLib.Bounding_Box; Color : RayLib.Color);
   --  Draw bounding box (wires)

   procedure Draw_Billboard
     (Camera   : RayLib.Camera; Texture : RayLib.Texture2D;
      Position : RayLib.Vector3; Size : Float; Tint : RayLib.Color);
   --  Draw a billboard texture

   procedure Draw_Billboard
     (Camera : RayLib.Camera; Texture : RayLib.Texture2D;
      Source : RayLib.Rectangle; Position : RayLib.Vector3;
      Size   : RayLib.Vector2; Tint : RayLib.Color);
   --  Draw a billboard texture defined by source

   procedure Draw_Billboard
     (Camera   : RayLib.Camera; Texture : RayLib.Texture2D;
      Source   : RayLib.Rectangle; Position : RayLib.Vector3;
      Up : RayLib.Vector3; Size : RayLib.Vector2; Origin : RayLib.Vector2;
      Rotation : Float; Tint : RayLib.Color);
   --  Draw a billboard texture defined by source and rotation

   procedure Upload_Mesh (Mesh : in out RayLib.Mesh; Dynamic : Boolean);
   --  Upload mesh vertex data in GPU and provide VAO/VBO ids

   procedure Update_Mesh_Buffer
     (Mesh : RayLib.Mesh; Index : Integer; Data : Stream_Element_Array);
   --  Update mesh vertex data in GPU for a specific buffer index

   procedure Draw_Mesh
     (Mesh      : RayLib.Mesh'Class; Material : RayLib.Material'Class;
      Transform : RayLib.Matrix);
   --  Draw a 3d mesh with material and transform

   procedure Draw_Mesh_Instanced
     (Mesh       : RayLib.Mesh'Class; Material : RayLib.Material'Class;
      Transforms : Transform_Array; Instances : Natural);
   --  Draw multiple mesh instances with material and different transforms

   function Export_Mesh
     (Mesh : RayLib.Mesh; File_Name : String) return Boolean;
   --  Export mesh data to file, returns true on success

   function Get_Mesh_Bounding_Box
     (Mesh : RayLib.Mesh) return RayLib.Bounding_Box;
   --  Compute mesh bounding box limits

   procedure Gen_Mesh_Tangents (Mesh : in out RayLib.Mesh);
   --  Compute mesh tangents

   procedure Gen_Mesh_Binormals (Mesh : in out RayLib.Mesh);
   --  Compute mesh binormals

   function Gen_Mesh_Poly (Sides : Natural; Radius : Float) return RayLib.Mesh;
   --  Generate polygonal mesh

   function Gen_Mesh_Plane
     (Width : Float; Length : Float; Res_X : Integer; Res_Z : Natural)
      return RayLib.Mesh;
   --  Generate plane mesh (with subdivisions)

   function Gen_Mesh_Cube
     (Width : Float; Height : Float; Length : Float) return RayLib.Mesh;
   --  Generate cuboid mesh

   function Gen_Mesh_Sphere
     (Radius : Float; Rings : Integer; Slices : Natural) return RayLib.Mesh;
   --  Generate sphere mesh (standard sphere)

   function Gen_Mesh_Hemi_Sphere
     (Radius : Float; Rings : Integer; Slices : Natural) return RayLib.Mesh;
   --  Generate half-sphere mesh (no bottom cap)

   function Gen_Mesh_Cylinder
     (Radius : Float; Height : Float; Slices : Natural) return RayLib.Mesh;
   --  Generate cylinder mesh

   function Gen_Mesh_Cone
     (Radius : Float; Height : Float; Slices : Natural) return RayLib.Mesh;
   --  Generate cone/pyramid mesh

   function Gen_Mesh_Torus
     (Radius : Float; Size : Float; Rad_Seg : Natural; Sides : Natural)
      return RayLib.Mesh;
   --  Generate torus mesh

   function Gen_Mesh_Knot
     (Radius : Float; Size : Float; Rad_Seg : Natural; Sides : Natural)
      return RayLib.Mesh;
   --  Generate trefoil knot mesh

   function Gen_Mesh_Heightmap
     (Heightmap : RayLib.Image'Class; Size : RayLib.Vector3)
      return RayLib.Mesh'Class;
   --  Generate heightmap mesh from image data

   function Gen_Mesh_Cubicmap
     (Cubicmap : RayLib.Image'Class; Cube_Size : RayLib.Vector3)
      return RayLib.Mesh'Class;
   --  Generate cubes-based map mesh from image data

   function Load_Materials (File_Name : String) return RayLib.Material_Array;
   --  Load materials from model file

   function Load_Material_Default return RayLib.Material;
   --  Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)

   procedure Set_Material_Texture
     (Material : in out RayLib.Material'Class; Map_Type : Material_Map_Index;
      Texture  :        RayLib.Texture2D'Class);
   --  Set texture for a material map type
   --  (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)

   procedure Set_Model_Mesh_Material
     (Model : in out RayLib.Model; Mesh_Id : Integer; Material_Id : Integer);
   --  Set material for a mesh

   function Load_Model_Animations
     (File_Name : String) return RayLib.Model_Animation_Array;
   --  Load model animations from file

   procedure Update_Model_Animation
     (Model : RayLib.Model'Class; Anim : RayLib.Model_Animation'Class;
      Frame : Integer);
   --  Update model animation pose

   function Is_Model_Animation_Valid
     (Model : RayLib.Model'Class; Anim : RayLib.Model_Animation'Class)
      return Boolean;
   --  Check model animation skeleton match

   function Check_Collision_Spheres
     (Center1 : RayLib.Vector3; Radius1 : Float; Center2 : RayLib.Vector3;
      Radius2 : Float) return Boolean;
   --  Check collision between two spheres

   function Check_Collision_Boxes
     (Box1 : RayLib.Bounding_Box; Box2 : RayLib.Bounding_Box) return Boolean;
   --  Check collision between two bounding boxes

   function Check_Collision_Box_Sphere
     (Box : RayLib.Bounding_Box; Center : RayLib.Vector3; Radius : Float)
      return Boolean;
   --  Check collision between box and sphere

   function Get_Ray_Collision_Sphere
     (Ray : RayLib.Ray; Center : RayLib.Vector3; Radius : Float)
      return RayLib.Ray_Collision;
   --  Get collision info between ray and sphere

   function Get_Ray_Collision_Box
     (Ray : RayLib.Ray; Box : RayLib.Bounding_Box) return RayLib.Ray_Collision;
   --  Get collision info between ray and box

   function Get_Ray_Collision_Mesh
     (Ray : RayLib.Ray; Mesh : RayLib.Mesh; Transform : RayLib.Matrix)
      return RayLib.Ray_Collision;
   --  Get collision info between ray and mesh

   function Get_Ray_Collision_Triangle
     (Ray : RayLib.Ray; P1 : RayLib.Vector3; P2 : RayLib.Vector3;
      P3  : RayLib.Vector3) return RayLib.Ray_Collision;
   --  Get collision info between ray and triangle

   function Get_Ray_Collision_Quad
     (Ray : RayLib.Ray; P1 : RayLib.Vector3; P2 : RayLib.Vector3;
      P3  : RayLib.Vector3; P4 : RayLib.Vector3) return RayLib.Ray_Collision;
   --  Get collision info between ray and quad

   procedure Init_Audio_Device with
      Import,
      Convention    => C,
      External_Name => "InitAudioDevice";
      --  Initialize audio device and context

   procedure Close_Audio_Device with
      Import,
      Convention    => C,
      External_Name => "CloseAudioDevice";
      --  Close the audio device and context

   function Is_Audio_Device_Ready return Boolean;
   --  Check if audio device has been initialized successfully

   procedure Set_Master_Volume (Volume : Float);
   --  Set master volume (listener)

   function Load_Wave (File_Name : String) return RayLib.Wave;
   --  Load wave data from file

   function Load_Wave_From_Memory
     (File_Type : String; File_Data : Stream_Element_Array) return RayLib.Wave;
   --  Load wave from memory buffer, fileType refers to extension: i.e. '.wav'

   function Load_Sound (File_Name : String) return RayLib.Sound;
   --  Load sound from file

   function Load_Sound_From_Wave
     (Wave : RayLib.Wave'Class) return RayLib.Sound'Class;
   --  Load sound from wave data

   procedure Update_Sound
     (Sound        : RayLib.Sound; Data : Stream_Element_Array;
      Sample_Count : Integer);
   --  Update sound buffer with new data

   function Export_Wave
     (Wave : RayLib.Wave; File_Name : String) return Boolean;
   --  Export wave data to file, returns true on success

   function Export_Wave_As_Code
     (Wave : RayLib.Wave; File_Name : String) return Boolean;
   --  Export wave sample data to code (.h), returns true on success

   procedure Play_Sound (Sound : RayLib.Sound);
   --  Play a sound

   procedure Stop_Sound (Sound : RayLib.Sound);
   --  Stop playing a sound

   procedure Pause_Sound (Sound : RayLib.Sound);
   --  Pause a sound

   procedure Resume_Sound (Sound : RayLib.Sound);
   --  Resume a paused sound

   procedure Play_Sound_Multi (Sound : RayLib.Sound);
   --  Play a sound (using multichannel buffer pool)

   procedure Stop_Sound_Multi with
      Import,
      Convention    => C,
      External_Name => "StopSoundMulti";
      --  Stop any sound playing (using multichannel buffer pool)

   function Get_Sounds_Playing return Natural;
   --  Get number of sounds playing in the multichannel

   function Is_Sound_Playing (Sound : RayLib.Sound) return Boolean;
   --  Check if a sound is currently playing

   procedure Set_Sound_Volume (Sound : RayLib.Sound; Volume : Float);
   --  Set volume for a sound (1.0 is max level)

   procedure Set_Sound_Pitch (Sound : RayLib.Sound; Pitch : Float);
   --  Set pitch for a sound (1.0 is base level)

   procedure Set_Sound_Pan (Sound : RayLib.Sound; Pan : Float);
   --  Set pan for a sound (0.5 is center)

   function Wave_Copy (Wave : RayLib.Wave) return RayLib.Wave;
   --  Copy a wave to a new wave

   procedure Wave_Crop
     (Wave         : in out RayLib.Wave; Init_Sample : Natural;
      Final_Sample :        Natural);
   --  Crop a wave to defined samples range

   procedure Wave_Format
     (Wave : in out RayLib.Wave; Sample_Rate : Natural; Sample_Size : Natural;
      Channels :        Natural);
   --  Convert wave data to desired format

   function Load_Wave_Samples (Wave : RayLib.Wave) return RayLib.Float_Array;
   --  Load samples data from wave as a 32bit float data array

   function Load_Music_Stream (File_Name : String) return RayLib.Music;
   --  Load music stream from file

   function Load_Music_Stream_From_Memory
     (File_Type : String; Data : Stream_Element_Array) return RayLib.Music;
   --  Load music stream from data

   procedure Play_Music_Stream (Music : RayLib.Music);
   --  Start music playing

   function Is_Music_Stream_Playing (Music : RayLib.Music) return Boolean;
   --  Check if music is playing

   procedure Update_Music_Stream (Music : RayLib.Music);
   --  Updates buffers for music streaming

   procedure Stop_Music_Stream (Music : RayLib.Music);
   --  Stop music playing

   procedure Pause_Music_Stream (Music : RayLib.Music);
   --  Pause music playing

   procedure Resume_Music_Stream (Music : RayLib.Music);
   --  Resume playing paused music

   procedure Seek_Music_Stream (Music : RayLib.Music; Position : Float);
   --  Seek music to a position (in seconds)

   procedure Set_Music_Volume (Music : RayLib.Music; Volume : Float) with
      Pre => Volume <= 1.0;
      --  Set volume for music (1.0 is max level)

   procedure Set_Music_Pitch (Music : RayLib.Music; Pitch : Float);
   --  Set pitch for a music (1.0 is base level)

   procedure Set_Music_Pan (Music : RayLib.Music; Pan : Float);
   --  Set pan for a music (0.5 is center)

   function Get_Music_Time_Length (Music : RayLib.Music) return Float;
   --  Get music time length (in seconds)

   function Get_Music_Time_Played (Music : RayLib.Music) return Float;
   --  Get current music time played (in seconds)

   function Load_Audio_Stream
     (Sample_Rate : Natural; Sample_Size : Natural; Channels : Natural)
      return RayLib.Audio_Stream;
   --  Load audio stream (to stream raw audio pcm data)

   procedure Update_Audio_Stream
     (Stream      : RayLib.Audio_Stream; Data : Stream_Element_Array;
      Frame_Count : Natural);
   --  Update audio stream buffers with data

   function Is_Audio_Stream_Processed
     (Stream : RayLib.Audio_Stream) return Boolean;
   --  Check if any audio stream buffers requires refill

   procedure Play_Audio_Stream (Stream : RayLib.Audio_Stream);
   --  Play audio stream

   procedure Pause_Audio_Stream (Stream : RayLib.Audio_Stream);
   --  Pause audio stream

   procedure Resume_Audio_Stream (Stream : RayLib.Audio_Stream);
   --  Resume audio stream

   function Is_Audio_Stream_Playing
     (Stream : RayLib.Audio_Stream) return Boolean;
   --  Check if audio stream is playing

   procedure Stop_Audio_Stream (Stream : RayLib.Audio_Stream);
   --  Stop audio stream

   procedure Set_Audio_Stream_Volume
     (Stream : RayLib.Audio_Stream; Volume : Float);
   --  Set volume for audio stream (1.0 is max level)

   procedure Set_Audio_Stream_Pitch
     (Stream : RayLib.Audio_Stream; Pitch : Float);
   --  Set pitch for audio stream (1.0 is base level)

   procedure Set_Audio_Stream_Pan (Stream : RayLib.Audio_Stream; Pan : Float);
   --  Set pan for audio stream (0.5 is centered)

   procedure Set_Audio_Stream_Buffer_Size_Default (Size : Natural);
   --  Default size for new audio streams
private
   type Image_Payload is record
      Data    : System.Address;
      Width   : Integer;
      Height  : Integer;
      Mipmaps : Integer;
      Format  : Pixel_Format;
   end record;
   type Image_Payload_Access is access all Image_Payload;

   type Image is new Ada.Finalization.Controlled with record
      Payload : Image_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Image);
   overriding procedure Finalize (Self : in out Image);

   type Texture_Payload is record
      Id      : OpenGL_Id;
      Width   : Integer;
      Height  : Integer;
      Mipmaps : Integer;
      Format  : Pixel_Format;
   end record;
   type Texture_Payload_Access is access all Texture_Payload;

   type Texture is new Ada.Finalization.Controlled with record
      Payload : Texture_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Texture);
   overriding procedure Finalize (Self : in out Texture);

   type Render_Texture_Payload is record
      Id      : OpenGL_Id;
      Texture : RayLib.Texture;
      Depth   : RayLib.Texture;
   end record;
   type Render_Texture_Payload_Access is access all Render_Texture_Payload;

   type Render_Texture is new Ada.Finalization.Controlled with record
      Payload : Render_Texture_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Render_Texture);
   overriding procedure Finalize (Self : in out Render_Texture);

   type Font_Payload (Glyph_Count : Natural) is record
      Base_Size     : Integer;
      Glyph_Padding : Integer;
      Texture       : RayLib.Texture2D;
      Recs          : Rectangle_Array (1 .. Glyph_Count);
      Glyphs        : Glyph_Info_Array (1 .. Glyph_Count);
   end record;
   type Font_Payload_Access is access all Font_Payload;

   type Font is new Ada.Finalization.Controlled with record
      Payload : Font_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Font);
   overriding procedure Finalize (Self : in out Font);

   type Mesh_Payload (Vertex_Count, Triangle_Count : Natural) is record
      Vertices      : Vector3_Array (1 .. Vertex_Count);
      Texcoords     : Vector2_Array (1 .. Vertex_Count);
      Texcoords2    : Vector2_Array (1 .. Vertex_Count);
      Normals       : Vector3_Array (1 .. Vertex_Count);
      Tangents      : Vector4_Array (1 .. Vertex_Count);
      Colors        : Color_Array (1 .. Vertex_Count);
      Indices       : Integer_Array (1 .. Vertex_Count);
      Anim_Vertices : Vector3_Array (1 .. Vertex_Count);
      Anim_Normals  : Vector3_Array (1 .. Vertex_Count);
      Bone_Ids      : Stream_Element_Array (1 .. 255);
      Bone_Weights  : Float_Array (1 .. Vertex_Count); -- * 4
      VAO_Id        : OpenGL_Id;
      VBO_Id        : OpenGL_Id_Array (1 .. Max_Mesh_Vertex_Buffers);
   end record;
   type Mesh_Payload_Access is access all Mesh_Payload;

   type Mesh is new Ada.Finalization.Controlled with record
      Payload : Mesh_Payload_Access;
   end record;

   type Shader_Payload is record
      Id        : OpenGL_Id;
      Locations : Integer_Array (1 .. Rl_Max_Shader_Locations);
   end record;
   type Shader_Payload_Access is access all Shader_Payload;

   type Shader is new Ada.Finalization.Controlled with record
      Payload : Shader_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Shader);
   overriding procedure Finalize (Self : in out Shader);

   type Material_Payload is record
      Shader : RayLib.Shader;
      Maps   : Material_Map_Array (1 .. Max_Material_Maps);
      Params : Float_Array (1 .. 4);
   end record;
   type Material_Payload_Access is access all Material_Payload;

   type Material is new Ada.Finalization.Controlled with record
      Payload : Material_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Material);
   overriding procedure Finalize (Self : in out Material);

   type Model_Payload (Mesh_Count, Material_Count, Bone_Count : Natural)
   is record
      Transform     : RayLib.Matrix;
      Meshes        : Mesh_Array (1 .. Mesh_Count);
      Materials     : Material_Array (1 .. Material_Count);
      Mesh_Material : Integer_Array (1 .. Mesh_Count);
      Bones         : Bone_Info_Array (1 .. Bone_Count);
      Bind_Pose     : Transform_Array (1 .. Bone_Count);
   end record;
   type Model_Payload_Access is access all Model_Payload;

   type Model is new Ada.Finalization.Controlled with record
      Payload : Model_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Model);
   overriding procedure Finalize (Self : in out Model);

   type Model_Animation_Payload (Bone_Count, Frame_Count : Natural) is record
      Bones       : Bone_Info_Array (1 .. Bone_Count);
      Frame_Poses : Transform_Array (1 .. Frame_Count);
   end record;
   type Model_Animation_Payload_Access is access all Model_Animation_Payload;

   type Model_Animation is new Ada.Finalization.Controlled with record
      Payload : Model_Animation_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Model_Animation);
   overriding procedure Finalize (Self : in out Model_Animation);

   type Wave_Payload is record
      Frame_Count : Natural;
      Sample_Rate : Natural;
      Sample_Size : Natural;
      Channels    : Natural;
      Data        : System.Address;
   end record;
   type Wave_Payload_Access is access all Wave_Payload;

   type Wave is new Ada.Finalization.Controlled with record
      Payload : Wave_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Wave);
   overriding procedure Finalize (Self : in out Wave);

   type Audio_Stream_Payload is record
      Buffer      : System.Address;
      Processor   : System.Address;
      Sample_Rate : Natural;
      Sample_Size : Natural;
      Channels    : Natural;
   end record;
   type Audio_Stream_Payload_Access is access all Audio_Stream_Payload;

   type Audio_Stream is new Ada.Finalization.Controlled with record
      Payload : Audio_Stream_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Audio_Stream);
   overriding procedure Finalize (Self : in out Audio_Stream);

   type Sound_Payload is record
      Stream      : RayLib.Audio_Stream;
      Frame_Count : Natural;
   end record;
   type Sound_Payload_Access is access all Sound_Payload;

   type Sound is new Ada.Finalization.Controlled with record
      Payload : Sound_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Sound);
   overriding procedure Finalize (Self : in out Sound);

   type Music_Payload is record
      Stream      : RayLib.Audio_Stream;
      Frame_Count : Natural;
      Looping     : Boolean;
      Ctx_Type    : Integer;
      Ctx_Data    : System.Address;
   end record;
   type Music_Payload_Access is access all Music_Payload;

   type Music is new Ada.Finalization.Controlled with record
      Payload : Music_Payload_Access;
   end record;

   overriding procedure Adjust (Self : in out Music);
   overriding procedure Finalize (Self : in out Music);
end RayLib;

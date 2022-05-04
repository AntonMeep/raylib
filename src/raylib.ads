with Interfaces;

with Ada.Calendar; use Ada.Calendar;
with Ada.Streams; use Ada.Streams;

package RayLib is
   Version : constant String := "4.0";

   --  Enumerations

   --  TODO: config flags

   type Trace_Log_Level is (
      Log_All,
      Log_Trace,
      Log_Debug,
      Log_Info,
      Log_Waring,
      Log_Error,
      Log_Fatal,
      Log_None
   )
   with Convention => C;

   type Vector2 is record
      X, Y : Float;
   end record with
      Convention => C_Pass_By_Copy;

   type Vector3 is record
      X, Y, Z : Float;
   end record with
      Convention => C_Pass_By_Copy;

   type Vector4 is record
      X, Y, Z, W : Float;
   end record with
      Convention => C_Pass_By_Copy;

   subtype Quaternion is Vector4;

   type Matrix is record
      M0, M4, M8, M12  : Float;
      M1, M5, M9, M13  : Float;
      M2, M6, M10, M14 : Float;
      M3, M7, M11, M15 : Float;
   end record with
      Convention => C_Pass_By_Copy;

   subtype Unsigned_8 is Interfaces.Unsigned_8;

   type Color is record
      R, G, B, A : Unsigned_8;
   end record with
      Convention => C_Pass_By_Copy;

   type Rectangle is record
      X, Y, Width, Height : Float;
   end record with
      Convention => C_Pass_By_Copy;

   type Image is record
      Data : System.Address;
      Width, Height : Integer;
      Mipmaps : Integer;
      Format : Pixel_Format;
   end record with
      Convention => C_Pass_By_Copy;

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

   procedure Init_Window (Width, Height : Integer; Title : String);
   function Window_Should_Close return Boolean;
   procedure Close_Window;
   function Is_Window_Ready return Boolean;
   function Is_Window_Fullscreen return Boolean;
   function Is_Window_Hidden return Boolean;
   function Is_Window_Minimized return Boolean;
   function Is_Window_Maximized return Boolean;
   function Is_Window_Focused return Boolean;
   function Is_Window_Resized return Boolean;
   function Is_Window_State (Flags : Config_Flag);
   procedure Set_Window_State (Flags : Config_Flag);
   procedure Clear_Window_State (Flags : Config_Flag);
   procedure Toggle_Fullscreen;
   procedure Maximize_Window;
   procedure Minimize_Window;
   procedure Restore_Window;
   procedure Set_Window_Icon(Image : RayLib.Image);
   procedure Set_Window_Title(Title : String);
   procedure Set_Window_Posisition(X, Y : Integer);
   procedure Set_Window_Monitor(Monitor : Integer);
   procedure Set_Window_Min_Size(Width, Height : Integer);
   procedure Set_Window_Size(Width, Height : Integer);
   function Get_Window_Handle return System.Address; --  TODO
   function Get_Screen_Width return Integer;
   function Get_Screen_Height return Integer;
   function Get_Monitor_Count return Integer;
   function Get_Monitor_Position(Monitor : Integer) return Vector2;
   function Get_Monitor_Width(Monitor : Integer) return Integer;
   function Get_Monitor_Physical_Width(Monitor : Integer) return Integer;
   function Get_Monitor_Physical_Height(Monitor : Integer) return Integer;
   function Get_Monitor_Refresh_Rate(Monitor : Integer) return Integer;
   function Get_Window_Position return Vector2;
   function Get_Window_Scale_DPI return Vector2;
   function Get_Monitor_Name(Monitor : Integer) return String;
   procedure Set_Clipboard_Text(Text : String);
   function Get_Clipboard_Text return String;

   procedure Swap_Screen_Buffer;
   procedure Poll_Input_Events;
   procedure Wait_Time(Ms : Float);

   procedure Show_Cursor;
   procedure Hide_Cursor;
   function Is_Cursor_Hidden return Boolean;
   procedure Enable_Cursor;
   procedure Disable_Cursor;
   function Is_Cursor_On_Screen return Boolean;

   procedure Clear_Background (Color : RayLib.Color);
   procedure Begin_Drawing;
   procedure End_Drawing;
   procedure Begin_Mode_2D(Camera : Camera_2D);
   procedure End_Mode_2D;
   procedure Begin_Mode_3D(Camera : Camera_3D);
   procedure End_Mode_3D;
   procedure Begin_Texture_Mode(Target : Render_Texture_2D);
   procedure End_Texture_Mode;
   procedure Begin_Shader_Mode(Shader : RayLib.Shader);
   procedure End_Shader_Mode;
   procedure Begin_Blend_Mode(Mode : Blend_Mode);
   procedure End_Blend_Mode;
   procedure Begin_Scissor_Mode(X, Y, Width, Height : Integer);
   procedure End_Scissor_Mode;
   procedure Begin_VR_Stereo_Mode(Config : VR_Stereo_Config);
   procedure End_VR_Stereo_Mode;

   function Load_VR_Stereo_Config(Device : VR_Device_Info) return VR_Stereo_Config;
   procedure Unload_VR_Stereo_Config(Config : VR_Stereo_Config);

   function Load_Shader(VS_Name, FS_Name : String) return Shader;
   function Load_Shader_From_Memory  (VS_Code, FS_Code : String) return Shader;
   function Get_Shader_Location(Shader : RayLib.Shader; Uniform_Name : String) return Integer;
   function Get_Shader_Location_Attrib(Shader : RayLib.Shader; Attrib_Name : String) return Integer;
   --  TODO: Set_Shader_Value

   function Get_Mouse_Ray(Mouse_Position : Vector2; Camera : RayLib.Camera) return Ray;
   function Get_Camera_Matrix(Camera : RayLib.Camera) return Matrix;
   function Get_Camera_Matrix_2D(Camera : Camera_2D) return Matrix;
   function Get_World_To_Screen(Position : Vector3; Camera : RayLib.Camera) return Vector2;
   function Get_World_To_Screen(Position : Vector3; Camera : RayLib.Camera; Width, Height : Integer) return Vector2;
   function Get_World_To_Screen_2D(Position : Vector2; Camera : Camera2D) return Vector2;
   function Get_Screen_To_World_2D(Position : Vector2; Camera : Camera2D) return Vector2;

   procedure Set_Target_FPS (FPS : Integer);
   function Get_FPS return Integer;
   function Get_Frame_Time return Float;
   function Get_Time return Long_Float;

   function Get_Random_Value(Min, Max : Integer) return Integer;
   procedure Set_Random_Seed(Seed : Natural);
   procedure Take_Screenshot(Name : String);
   procedure Set_Config_Flags(Flags : Config_Flag);

   --  TODO: Trace_Log
   procedure Set_Trace_Log_Level(Log_Level : Trace_Log_Level);
   --  TODO: memalloc??

   procedure Set_Trace_Log_Callback(Cb : Trace_Log_Callback);
   procedure Set_Load_File_Data_Callback(Cb : Load_File_Data_Callback);
   procedure Set_Save_File_Data_Callback(Cb : Save_File_Data_Callback);
   procedure Set_Load_File_Text_Callback(Cb : Load_File_Text_Callback);
   procedure Set_Save_File_Text_Callback(Cb : Save_File_Text_Callback);

   function Load_File_Data(Name : String) return Stream_Element_Array;
   --  no need for unload*
   function Save_File_Data(Name : String; Data : Stream_Element_Array) return Boolean;
   --  TODO: make a procedure?
   function Load_File_Text(Name : String) return String;
   function Save_File_Text(Name : String; Text : String);
   function File_Exists(Name : String) return Boolean;
   function Directory_Exists(Dir_Path : String) return Boolean;
   function Is_File_Extension(Name, Extension : String) return Boolean;
   function Get_File_Extension(Name : String) return String;
   function Get_Name(File_Path : String) return String;
   function Get_Name_Without_Extension(File_Path : String) return String;
   function Get_Directory_Path(File_Path : String) return String;
   function Get_Previous_Directory_Path(Dir_Path : String) return String;
   function Get_Working_Directory return String;
   --  TODO: Get_Directory_Files?
   function Change_Directory(Dir : String) return Boolean;
   --  TODO: Make a procedure?
   function Is_File_Dropped return Boolean;
   --  TODO: Get_Dropped_Files
   function Get_File_Modification_Time(Name : String) return Time;

   function Compress_Data(Data : Stream_Element_Array) return Stream_Element_Array;
   function Decompress_Data(Data : Stream_Element_Array) return Stream_Element_Array;
   function Encode_Data_Base64(Data : Stream_Element_Array) return String;
   function Decode_Data_Base64(Data : String) return Stream_Element_Array;

   function Save_Storage_Value(Position : Natural; Value : Integer) return Boolean;
   --  TODO: make procedure?
   function Load_Storage_Value(Position : Natural) return Integer;

   procedure Open_URL(URL : String);

   --  TODO: input handling, gestures


   procedure Set_Camera_Mode(Camera : RayLib.Camera; Mode : Camera_Mode);
   procedure Update_Camera(Camera : in out RayLib.Camera);

   --  TODO: Set_Camera_**_Control

   procedure Set_Shapes_Texture(Texture : Texture_2D; Source : Rectangle);

   procedure Draw_Pixel(Position_X, Position_Y : Integer; Color : RayLib.Color);
   procedure Draw_Pixel(Position : Vector2; Color : RayLib.Color);
   procedure Draw_Line(Start_Position_X, Start_Position_Y, End_Position_X, End_Position_T : Integer; Color : RayLib.Color);
   procedure Draw_Line(Start_Position, End_Position : Vector2; Color : RayLib.Color);
   procedure Draw_Line(Start_Position, End_Position : Vector2; Thickness : Float; Color : RayLib.Color);
   procedure Draw_Line_Bezier(Start_Position, End_Position : Vector2; Thickness : Float; Color : RayLib.Color);
   procedure Draw_Line_Bezier(Start_Position, End_Position, Control_Position : Vector2; Thickness : Float; Color : RayLib.Color);
   procedure Draw_Line_Bezier(Start_Position, End_Position, Start_Control_Position, End_Control_Position : Vector2; Thickness : Float; Color : RayLib.Color);
   --  TODO: Draw_Line_Strip
   procedure Draw_Circle(Center_X, Center_Y : Integer; Radius : Float; Color : RayLib.Color);
   procedure Draw_Circle_Sector(Center : Vector2; Radius, Start_Angle, End_Angle : Float; Segments : Integer; Color : RayLib.Color);
   procedure Draw_Circle_Sector_Lines(Center : Vector2; Radius, Start_Angle, End_Angle : Float; Segments : Integer; Color : RayLib.Color);
   procedure Draw_Circle_Gradient(Center_X, Center_Y : Integer; Radius : Float; Color_1, Color_2 : RayLib.Color);
   procedure Draw_Circle(Center : Vector2; Radius : Float; Color : RayLib.Color);
   procedure Draw_Circle_Lines(Center_X, Center_Y : Integer; Radius : Float; Color : RayLib.Color);
   procedure Draw_Ellipse(Center_X, Center_Y : Integer; Radius_H, Radius_V : Float; Color : RayLib.Color);
   procedure Draw_Ellipse_Lines(Center_X, Center_Y : Integer; Radius_H, Radius_V : Float; Color : RayLib.Color);
   procedure Draw_Ring(Center : Vector2; Inner_Radius, Outer_Radius, Start_Angle, End_Angle : Float; Segments : Integer; Color : RayLib.Color);
   procedure Draw_Ring_Lines(Center : Vector2; Inner_Radius, Outer_Radius, Start_Angle, End_Angle : Float; Segments : Integer; Color : RayLib.Color);
   procedure Draw_Rectangle(Position_X, Position_Y, Width, Height : Integer; Color : RayLib.Color);
   procedure Draw_Rectangle(Position, Size : Vector2; Color : RayLib.Color);
   procedure Draw_Rectangle(Rectangle : RayLib.Rectangle; Color : RayLib.Color);
   procedure Draw_Rectangle(Rectangle : RayLib.Rectangle; Origin : Vector2; Rotation : Float; Color : RayLib.Color);
   procedure Draw_Rectangle_Gradient_Vertical(Position_X, Position_Y, Width, Height : Integer; Color_1, Color_2 : RayLib.Color);
   procedure Draw_Rectangle_Gradient_Horizontal(Position_X, Position_Y, Width, Height : Integer; Color_1, Color_2 : RayLib.Color);
   procedure Draw_Rectangle_Gradient(Rectangle : RayLib.Rectangle;  Color_1, Color_2, Color_3, Color_4 : RayLib.Color);
   procedure Draw_Rectangle_Lines(Position_X, Position_Y, Width, Height : Integer; Color : RayLib.Color);
   procedure Draw_Rectangle_Lines(Rectangle : RayLib.Rectangle; Line_Thickness : Float; Color : RayLib.Color);
   procedure Draw_Rectangle_Rounded(Rectangle : RayLib.Rectangle; Roundness : Float; Segments : Integer; Color : RayLib.Color);
   procedure Draw_Rectangle_Rounded_Lines(Rectangle : RayLib.Rectangle; Roundness : Float; Segments : Integer; Line_Thickness : Float; Color : RayLib.Color);
   procedure Draw_Triangle(V1, V2, V3 : Vector2; Color : RayLib.Color);
   procedure Draw_Triangle_Lines(V1, V2, V3 : Vector2; Color : RayLib.Color);
   --  TODO: Draw_Triangle_Fan, Strip
   procedure Draw_Polygon(Center : Vector2; Sides : Integer; Radius, Rotation : Float; Color : RayLib.Color);
   procedure Draw_Polygon_Lines(Center : Vector2; Sides : Integer; Radius, Rotation : Float; Color : RayLib.Color);
   procedure Draw_Polygon_Lines(Center : Vector2; Sides : Integer; Radius, Rotation, Line_Thickness : Float; Color : RayLib.Color);
   

   function Check_Collision_Rectangles(Rectangle_1, Rectangle_2 : RayLib.Rectangle) return Boolean;
   function Check_Collision_Circles(Center_1 : Vector2; Radius_1 : Float; Center_2: Vector2; Radius_2 : Float) return Boolean;
   function Check_Collision_Circle_Rectangle(Center : Vector2; Radius : Float; Rectangle : RayLib.Rectangle) return Boolean;
   function Check_Collision_Point_Rectangle(Point : Vector2; Rectangle : RayLib.Rectangle) return Boolean;
   function Check_Collision_Point_Circle(Point, Center : Vector2; Radius : Float) return Boolean;
   function Check_Collision_Point_Triangle(Point, P1, P2, P3 : Vector2) return Boolean;
   function Check_Collision_Lines(Start_Position_1, End_Position_1, Start_Position_2, End_Position_2 : Vector2; Collision_Point : out Vector2) return Boolean;
   function Check_Collision_Point_Line(Point, P1, P2 : Vector2; Threshold : Integer) return Boolean;
   function Get_Collision_Rectangle(Rectangle_1, Rectangle_2 : RayLib.Rectangle) return Rectangle;


   function Load_Image(Name : String) return Image;
   function Load_Image(Name : String; Width, Height : Integer; Format : Pixel_Format; Header_Size : Integer) return Image;
   --  TODO: Load_Image_Anim
   function Load_Image_From_Memory(File_Type : String; Data : Stream_Element_Array) return Image;
   function Load_Image_From_Texture(Texture : Texture_2D) return Image;
   
   
   
   

   procedure Draw_Text
     (Text  : String; Position_X, Position_Y : Integer; Font_Size : Integer;
      Color : RayLib.Color);
end RayLib;

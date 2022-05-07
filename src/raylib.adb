pragma Ada_2012;

with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with System.Address_Image;

with raylib_h;

package body RayLib is
   function Data (Self : Image'Class) return Stream_Element_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Data unimplemented");
      return raise Program_Error with "Unimplemented function Data";
   end Data;

   function Width (Self : Image'Class) return Natural is
     (Natural (Self.Payload.all.Data.width));

   function Height (Self : Image'Class) return Natural is
     (Natural (Self.Payload.all.Data.height));

   function Mipmaps (Self : Image'Class) return Natural is
     (Natural (Self.Payload.all.Data.mipmaps));

   function Format (Self : Image'Class) return Pixel_Format is
     (Pixel_Format (Self.Payload.all.Data.format));

   function Id (Self : Texture'Class) return OpenGL_Id is
     (OpenGL_Id (Self.Payload.all.Data.id));

   function Width (Self : Texture'Class) return Natural is
     (Natural (Self.Payload.all.Data.width));

   function Height (Self : Texture'Class) return Natural is
     (Natural (Self.Payload.all.Data.height));

   function Mipmaps (Self : Texture'Class) return Natural is
     (Natural (Self.Payload.all.Data.mipmaps));

   function Format (Self : Texture'Class) return Pixel_Format is
     (Pixel_Format (Self.Payload.all.Data.format));

   function Id (Self : Render_Texture'Class) return OpenGL_Id is
     (OpenGL_Id (Self.Payload.all.Data.id));

   function Get_Texture (Self : Render_Texture'Class) return RayLib.Texture is
     (Ada.Finalization.Controlled with
      Payload =>
        new Texture_Payload'
          (Data   => Self.Payload.all.Data.the_texture, Parent_Unloads => True,
           others => <>));

   function Depth (Self : Render_Texture'Class) return RayLib.Texture is
     (Ada.Finalization.Controlled with
      Payload =>
        new Texture_Payload'
          (Data   => Self.Payload.all.Data.depth, Parent_Unloads => True,
           others => <>));

   function Glyph_Count (Self : Font'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Glyph_Count unimplemented");
      return raise Program_Error with "Unimplemented function Glyph_Count";
   end Glyph_Count;

   function Base_Size (Self : Font'Class) return Integer is
   begin
      pragma Compile_Time_Warning (Standard.True, "Base_Size unimplemented");
      return raise Program_Error with "Unimplemented function Base_Size";
   end Base_Size;

   function Glyph_Padding (Self : Font'Class) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Glyph_Padding unimplemented");
      return raise Program_Error with "Unimplemented function Glyph_Padding";
   end Glyph_Padding;

   function Get_Texture (Self : Font'Class) return RayLib.Texture2D'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Texture unimplemented");
      return raise Program_Error with "Unimplemented function Get_Texture";
   end Get_Texture;

   function Recs (Self : Font'Class) return Rectangle_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Recs unimplemented");
      return raise Program_Error with "Unimplemented function Recs";
   end Recs;

   function Glyphs (Self : Font'Class) return Glyph_Info_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Glyphs unimplemented");
      return raise Program_Error with "Unimplemented function Glyphs";
   end Glyphs;

   function Vertex_Count (Self : Mesh'Class) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Vertex_Count unimplemented");
      return raise Program_Error with "Unimplemented function Vertex_Count";
   end Vertex_Count;

   function Triangle_Count (Self : Mesh'Class) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Triangle_Count unimplemented");
      return raise Program_Error with "Unimplemented function Triangle_Count";
   end Triangle_Count;

   function Vertices (Self : Mesh'Class) return Vector3_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Vertices unimplemented");
      return raise Program_Error with "Unimplemented function Vertices";
   end Vertices;

   function Texcoords (Self : Mesh'Class) return Vector2_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Texcoords unimplemented");
      return raise Program_Error with "Unimplemented function Texcoords";
   end Texcoords;

   function Texcoords2 (Self : Mesh'Class) return Vector2_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Texcoords2 unimplemented");
      return raise Program_Error with "Unimplemented function Texcoords2";
   end Texcoords2;

   function Normals (Self : Mesh'Class) return Vector3_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Normals unimplemented");
      return raise Program_Error with "Unimplemented function Normals";
   end Normals;

   function Tangents (Self : Mesh'Class) return Vector4_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Tangents unimplemented");
      return raise Program_Error with "Unimplemented function Tangents";
   end Tangents;

   function Colors (Self : Mesh'Class) return Color_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Colors unimplemented");
      return raise Program_Error with "Unimplemented function Colors";
   end Colors;

   function Indices (Self : Mesh'Class) return Integer_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Indices unimplemented");
      return raise Program_Error with "Unimplemented function Indices";
   end Indices;

   function Animated_Vertices (Self : Mesh'Class) return Vector3_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Animated_Vertices unimplemented");
      return
        raise Program_Error with "Unimplemented function Animated_Vertices";
   end Animated_Vertices;

   function Animated_Normals (Self : Mesh'Class) return Vector3_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Animated_Normals unimplemented");
      return
        raise Program_Error with "Unimplemented function Animated_Normals";
   end Animated_Normals;

   function Bone_Ids (Self : Mesh'Class) return Stream_Element_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bone_Ids unimplemented");
      return raise Program_Error with "Unimplemented function Bone_Ids";
   end Bone_Ids;

   function Bone_Weights (Self : Mesh'Class) return Float_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Bone_Weights unimplemented");
      return raise Program_Error with "Unimplemented function Bone_Weights";
   end Bone_Weights;

   function VAO_Id (Self : Mesh'Class) return OpenGL_Id is
   begin
      pragma Compile_Time_Warning (Standard.True, "VAO_Id unimplemented");
      return raise Program_Error with "Unimplemented function VAO_Id";
   end VAO_Id;

   function VBO_Id (Self : Mesh'Class) return OpenGL_Id_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "VBO_Id unimplemented");
      return raise Program_Error with "Unimplemented function VBO_Id";
   end VBO_Id;

   function Id (Self : Shader'Class) return OpenGL_Id is
   begin
      pragma Compile_Time_Warning (Standard.True, "Id unimplemented");
      return raise Program_Error with "Unimplemented function Id";
   end Id;

   function Locations (Self : Shader'Class) return Integer_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Locations unimplemented");
      return raise Program_Error with "Unimplemented function Locations";
   end Locations;

   function Get_Shader (Self : Material'Class) return RayLib.Shader'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Shader unimplemented");
      return raise Program_Error with "Unimplemented function Get_Shader";
   end Get_Shader;

   function Maps (Self : Material'Class) return Material_Map_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Maps unimplemented");
      return raise Program_Error with "Unimplemented function Maps";
   end Maps;

   function Parameters (Self : Material'Class) return Float_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Parameters unimplemented");
      return raise Program_Error with "Unimplemented function Parameters";
   end Parameters;

   function Mesh_Count (Self : Model'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Mesh_Count unimplemented");
      return raise Program_Error with "Unimplemented function Mesh_Count";
   end Mesh_Count;

   function Material_Count (Self : Model'Class) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Material_Count unimplemented");
      return raise Program_Error with "Unimplemented function Material_Count";
   end Material_Count;

   function Bone_Count (Self : Model'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bone_Count unimplemented");
      return raise Program_Error with "Unimplemented function Bone_Count";
   end Bone_Count;

   function Get_Transform (Self : Model'Class) return RayLib.Matrix is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Transform unimplemented");
      return raise Program_Error with "Unimplemented function Get_Transform";
   end Get_Transform;

   function Meshes (Self : Model'Class) return Mesh_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Meshes unimplemented");
      return raise Program_Error with "Unimplemented function Meshes";
   end Meshes;

   function Materials (Self : Model'Class) return Material_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Materials unimplemented");
      return raise Program_Error with "Unimplemented function Materials";
   end Materials;

   function Mesh_Material (Self : Model'Class) return Integer_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Mesh_Material unimplemented");
      return raise Program_Error with "Unimplemented function Mesh_Material";
   end Mesh_Material;

   function Bones (Self : Model'Class) return Bone_Info_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bones unimplemented");
      return raise Program_Error with "Unimplemented function Bones";
   end Bones;

   function Bind_Pose (Self : Model'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bind_Pose unimplemented");
      return raise Program_Error with "Unimplemented function Bind_Pose";
   end Bind_Pose;

   function Bone_Count (Self : Model_Animation'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bone_Count unimplemented");
      return raise Program_Error with "Unimplemented function Bone_Count";
   end Bone_Count;

   function Frame_Count (Self : Model_Animation'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Frame_Count unimplemented");
      return raise Program_Error with "Unimplemented function Frame_Count";
   end Frame_Count;

   function Bones (Self : Model_Animation'Class) return Bone_Info_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Bones unimplemented");
      return raise Program_Error with "Unimplemented function Bones";
   end Bones;

   function Frame_Poses (Self : Model_Animation'Class) return Transform_Array
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Frame_Poses unimplemented");
      return raise Program_Error with "Unimplemented function Frame_Poses";
   end Frame_Poses;

   function Frame_Count (Self : Wave'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Frame_Count unimplemented");
      return raise Program_Error with "Unimplemented function Frame_Count";
   end Frame_Count;

   function Sample_Rate (Self : Wave'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Sample_Rate unimplemented");
      return raise Program_Error with "Unimplemented function Sample_Rate";
   end Sample_Rate;

   function Sample_Size (Self : Wave'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Sample_Size unimplemented");
      return raise Program_Error with "Unimplemented function Sample_Size";
   end Sample_Size;

   function Channels (Self : Wave'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Channels unimplemented");
      return raise Program_Error with "Unimplemented function Channels";
   end Channels;

   function Data (Self : Wave'Class) return Stream_Element_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Data unimplemented");
      return raise Program_Error with "Unimplemented function Data";
   end Data;

   function Buffer (Self : Audio_Stream'Class) return Stream_Element_Array is
   begin
      pragma Compile_Time_Warning (Standard.True, "Buffer unimplemented");
      return raise Program_Error with "Unimplemented function Buffer";
   end Buffer;

   function Sample_Rate (Self : Audio_Stream'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Sample_Rate unimplemented");
      return raise Program_Error with "Unimplemented function Sample_Rate";
   end Sample_Rate;

   function Sample_Size (Self : Audio_Stream'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Sample_Size unimplemented");
      return raise Program_Error with "Unimplemented function Sample_Size";
   end Sample_Size;

   function Channels (Self : Audio_Stream'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Channels unimplemented");
      return raise Program_Error with "Unimplemented function Channels";
   end Channels;

   function Stream (Self : Sound'Class) return Audio_Stream is
   begin
      pragma Compile_Time_Warning (Standard.True, "Stream unimplemented");
      return raise Program_Error with "Unimplemented function Stream";
   end Stream;

   function Frame_Count (Self : Sound'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Frame_Count unimplemented");
      return raise Program_Error with "Unimplemented function Frame_Count";
   end Frame_Count;

   function Stream (Self : Music'Class) return Audio_Stream is
   begin
      pragma Compile_Time_Warning (Standard.True, "Stream unimplemented");
      return raise Program_Error with "Unimplemented function Stream";
   end Stream;

   function Frame_Count (Self : Music'Class) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Frame_Count unimplemented");
      return raise Program_Error with "Unimplemented function Frame_Count";
   end Frame_Count;

   function Looping (Self : Music'Class) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Looping unimplemented");
      return raise Program_Error with "Unimplemented function Looping";
   end Looping;

   procedure Init_Window (Width : Natural; Height : Natural; Title : String) is
      Title_Copy : chars_ptr := New_String (Title);
   begin
      raylib_h.InitWindow (int (Width), int (Height), Title_Copy);
      Free (Title_Copy);
   end Init_Window;

   function Window_Should_Close return Boolean is
     (Boolean (raylib_h.WindowShouldClose));

   procedure Close_Window is
   begin
      raylib_h.CloseWindow;
   end Close_Window;

   function Is_Window_Ready return Boolean is
     (Boolean (raylib_h.IsWindowReady));

   function Is_Window_Fullscreen return Boolean is
     (Boolean (raylib_h.IsWindowFullscreen));

   function Is_Window_Hidden return Boolean is
     (Boolean (raylib_h.IsWindowHidden));

   function Is_Window_Minimized return Boolean is
     (Boolean (raylib_h.IsWindowMinimized));

   function Is_Window_Maximized return Boolean is
     (Boolean (raylib_h.IsWindowMaximized));

   function Is_Window_Focused return Boolean is
     (Boolean (raylib_h.IsWindowFocused));

   function Is_Window_Resized return Boolean is
     (Boolean (raylib_h.IsWindowResized));

   function Is_Window_State (Flag : Config_Flag) return Boolean is
     (Boolean (raylib_h.IsWindowState (unsigned (Flag))));

   procedure Set_Window_State (Flags : Config_Flag) is
   begin
      raylib_h.SetWindowState (unsigned (Flags));
   end Set_Window_State;

   procedure Clear_Window_State (Flags : Config_Flag) is
   begin
      raylib_h.ClearWindowState (unsigned (Flags));
   end Clear_Window_State;

   procedure Toggle_Fullscreen is
   begin
      raylib_h.ToggleFullscreen;
   end Toggle_Fullscreen;

   procedure Maximize_Window is
   begin
      raylib_h.MaximizeWindow;
   end Maximize_Window;

   procedure Minimize_Window is
   begin
      raylib_h.MinimizeWindow;
   end Minimize_Window;

   procedure Restore_Window is
   begin
      raylib_h.RestoreWindow;
   end Restore_Window;

   procedure Set_Window_Icon (Image : RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Window_Icon unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Window_Icon";
   end Set_Window_Icon;

   procedure Set_Window_Title (Title : String) is
      Title_Copy : chars_ptr := New_String (Title);
   begin
      raylib_h.SetWindowTitle (Title_Copy);
      Free (Title_Copy);
   end Set_Window_Title;

   procedure Set_Window_Position (X : Natural; Y : Natural) is
   begin
      raylib_h.SetWindowPosition (int (X), int (Y));
   end Set_Window_Position;

   procedure Set_Window_Monitor (Monitor : Monitor_Id) is
   begin
      raylib_h.SetWindowMonitor (int (Monitor));
   end Set_Window_Monitor;

   procedure Set_Window_Min_Size (Width : Natural; Height : Natural) is
   begin
      raylib_h.SetWindowMinSize (int (Width), int (Height));
   end Set_Window_Min_Size;

   procedure Set_Window_Size (Width : Natural; Height : Natural) is
   begin
      raylib_h.SetWindowSize (int (Width), int (Height));
   end Set_Window_Size;

   function Get_Window_Handle return System.Address is
     (raylib_h.GetWindowHandle);

   function Get_Screen_Width return Natural is
     (Natural (raylib_h.GetScreenWidth));

   function Get_Screen_Height return Natural is
     (Natural (raylib_h.GetScreenHeight));

   function Get_Monitor_Count return Natural is
     (Natural (raylib_h.GetMonitorCount));

   function Get_Current_Monitor return Monitor_Id is
     (Monitor_Id (raylib_h.GetCurrentMonitor));

   function Get_Monitor_Position
     (Monitor : Monitor_Id) return RayLib.Vector2 is
     (+raylib_h.GetMonitorPosition (int (Monitor)));

   function Get_Monitor_Width (Monitor : Monitor_Id) return Natural is
     (Natural (raylib_h.GetMonitorWidth (int (Monitor))));

   function Get_Monitor_Height (Monitor : Monitor_Id) return Natural is
     (Natural (raylib_h.GetMonitorHeight (int (Monitor))));

   function Get_Monitor_Physical_Width (Monitor : Monitor_Id) return Natural is
     (Natural (raylib_h.GetMonitorPhysicalWidth (int (Monitor))));

   function Get_Monitor_Physical_Height
     (Monitor : Monitor_Id) return Natural is
     (Natural (raylib_h.GetMonitorPhysicalHeight (int (Monitor))));

   function Get_Monitor_Refresh_Rate (Monitor : Monitor_Id) return Natural is
     (Natural (raylib_h.GetMonitorRefreshRate (int (Monitor))));

   function Get_Window_Position return RayLib.Vector2 is
     (+raylib_h.GetWindowPosition);

   function Get_Window_Scale_DPI return RayLib.Vector2 is
     (+raylib_h.GetWindowScaleDPI);

   function Get_Monitor_Name (Monitor : Monitor_Id) return String is
     (Value (raylib_h.GetMonitorName (int (Monitor))));

   procedure Set_Clipboard_Text (Text : String) is
      Text_Copy : chars_ptr := New_String (Text);
   begin
      raylib_h.SetClipboardText (Text_Copy);
      Free (Text_Copy);
   end Set_Clipboard_Text;

   function Get_Clipboard_Text return String is
     (Value (raylib_h.GetClipboardText));

   procedure Swap_Screen_Buffer is
   begin
      raylib_h.SwapScreenBuffer;
   end Swap_Screen_Buffer;

   procedure Poll_Input_Events is
   begin
      raylib_h.PollInputEvents;
   end Poll_Input_Events;

   procedure Wait_Time (Ms : Float) is
   begin
      raylib_h.WaitTime (Ms);
   end Wait_Time;

   procedure Show_Cursor is
   begin
      raylib_h.ShowCursor;
   end Show_Cursor;

   procedure Hide_Cursor is
   begin
      raylib_h.HideCursor;
   end Hide_Cursor;

   function Is_Cursor_Hidden return Boolean is
     (Boolean (raylib_h.IsCursorHidden));

   procedure Enable_Cursor is
   begin
      raylib_h.EnableCursor;
   end Enable_Cursor;

   procedure Disable_Cursor is
   begin
      raylib_h.DisableCursor;
   end Disable_Cursor;

   function Is_Cursor_On_Screen return Boolean is
     (Boolean (raylib_h.IsCursorOnScreen));

   procedure Clear_Background (Color : RayLib.Color) is
   begin
      raylib_h.ClearBackground (+Color);
   end Clear_Background;

   procedure Begin_Drawing is
   begin
      raylib_h.BeginDrawing;
   end Begin_Drawing;

   procedure End_Drawing is
   begin
      raylib_h.EndDrawing;
   end End_Drawing;

   procedure Begin_Mode2D (Camera : RayLib.Camera2D) is
   begin
      raylib_h.BeginMode2D (+Camera);
   end Begin_Mode2D;

   procedure End_Mode2D is
   begin
      raylib_h.EndMode2D;
   end End_Mode2D;

   procedure Begin_Mode3D (Camera : RayLib.Camera3D) is
   begin
      raylib_h.BeginMode3D (+Camera);
   end Begin_Mode3D;

   procedure End_Mode3D is
   begin
      raylib_h.EndMode3D;
   end End_Mode3D;

   procedure Begin_Texture_Mode (Target : RayLib.Render_Texture2D) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Begin_Texture_Mode unimplemented");
      raise Program_Error with "Unimplemented procedure Begin_Texture_Mode";
   end Begin_Texture_Mode;

   procedure End_Texture_Mode is
   begin
      raylib_h.EndTextureMode;
   end End_Texture_Mode;

   procedure Begin_Shader_Mode (Shader : RayLib.Shader) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Begin_Shader_Mode unimplemented");
      raise Program_Error with "Unimplemented procedure Begin_Shader_Mode";
   end Begin_Shader_Mode;

   procedure End_Shader_Mode is
   begin
      raylib_h.EndShaderMode;
   end End_Shader_Mode;

   procedure Begin_Blend_Mode (Mode : Blend_Mode) is
   begin
      raylib_h.BeginBlendMode (int (Mode));
   end Begin_Blend_Mode;

   procedure End_Blend_Mode is
   begin
      raylib_h.EndBlendMode;
   end End_Blend_Mode;

   procedure Begin_Scissor_Mode (X, Y, Width, Height : Natural) is
   begin
      raylib_h.BeginScissorMode (int (X), int (Y), int (Width), int (Height));
   end Begin_Scissor_Mode;

   procedure End_Scissor_Mode is
   begin
      raylib_h.EndScissorMode;
   end End_Scissor_Mode;

   procedure Begin_VR_Stereo_Mode (Config : RayLib.VR_Stereo_Config) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Begin_VR_Stereo_Mode unimplemented");
      raise Program_Error with "Unimplemented procedure Begin_VR_Stereo_Mode";
   end Begin_VR_Stereo_Mode;

   procedure End_VR_Stereo_Mode is
   begin
      raylib_h.EndVrStereoMode;
   end End_VR_Stereo_Mode;

   function Load_VR_Stereo_Config
     (Device : RayLib.VR_Device_Info) return RayLib.VR_Stereo_Config
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_VR_Stereo_Config unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_VR_Stereo_Config";
   end Load_VR_Stereo_Config;

   function Load_Shader
     (Vs_File_Name : String; Fs_File_Name : String) return RayLib.Shader
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Load_Shader unimplemented");
      return raise Program_Error with "Unimplemented function Load_Shader";
   end Load_Shader;

   function Load_Shader_From_Memory
     (Vs_Code : String; Fs_Code : String) return RayLib.Shader
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Shader_From_Memory unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_Shader_From_Memory";
   end Load_Shader_From_Memory;

   function Get_Shader_Location
     (Shader : RayLib.Shader; Uniform_Name : String) return Integer
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Shader_Location unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Shader_Location";
   end Get_Shader_Location;

   function Get_Shader_Location_Attrib
     (Shader : RayLib.Shader; Attrib_Name : String) return Integer
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Shader_Location_Attrib unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_Shader_Location_Attrib";
   end Get_Shader_Location_Attrib;

   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Value : System.Address;
      Uniform_Type : Integer)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Shader_Value unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Shader_Value";
   end Set_Shader_Value;

   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Value : System.Address;
      Uniform_Type : Integer; Count : Integer)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Shader_Value unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Shader_Value";
   end Set_Shader_Value;

   procedure Set_Shader_Value
     (Shader : RayLib.Shader; Loc_Index : Integer; Mat : RayLib.Matrix)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Shader_Value unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Shader_Value";
   end Set_Shader_Value;

   procedure Set_Shader_Value_Texture
     (Shader  : RayLib.Shader'Class; Loc_Index : Integer;
      Texture : RayLib.Texture2D'Class)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Shader_Value_Texture unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Shader_Value_Texture";
   end Set_Shader_Value_Texture;

   function Get_Mouse_Ray
     (Mouse_Position : RayLib.Vector2; Camera : RayLib.Camera)
      return RayLib.Ray
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Mouse_Ray unimplemented");
      return raise Program_Error with "Unimplemented function Get_Mouse_Ray";
   end Get_Mouse_Ray;

   function Get_Camera_Matrix (Camera : RayLib.Camera) return RayLib.Matrix is
     (+raylib_h.GetCameraMatrix (+Camera));

   function Get_Camera_Matrix2D
     (Camera : RayLib.Camera2D) return RayLib.Matrix is
     (+raylib_h.GetCameraMatrix2D (+Camera));

   function Get_World_To_Screen
     (Position : RayLib.Vector3; Camera : RayLib.Camera)
      return RayLib.Vector2 is
     (+raylib_h.GetWorldToScreen (+Position, +Camera));

   function Get_World_To_Screen
     (Position : RayLib.Vector3; Camera : RayLib.Camera; Width : Natural;
      Height   : Natural) return RayLib.Vector2 is
     (+raylib_h.GetWorldToScreenEx
        (+Position, +Camera, int (Width), int (Height)));

   function Get_World_To_Screen2D
     (Position : RayLib.Vector2; Camera : RayLib.Camera2D)
      return RayLib.Vector2 is
     (+raylib_h.GetWorldToScreen2D (+Position, +Camera));

   function Get_Screen_To_World2D
     (Position : RayLib.Vector2; Camera : RayLib.Camera2D)
      return RayLib.Vector2 is
     (+raylib_h.GetScreenToWorld2D (+Position, +Camera));

   procedure Set_Target_FPS (Fps : Natural) is
   begin
      raylib_h.SetTargetFPS (int (Fps));
   end Set_Target_FPS;

   function Get_FPS return Natural is (Natural (raylib_h.GetFPS));

   function Get_Frame_Time return Float is (raylib_h.GetFrameTime);

   function Get_Time return Long_Float is (Long_Float (raylib_h.GetTime));

   function Get_Random_Value (Min : Integer; Max : Integer) return Integer is
     (Integer (raylib_h.GetRandomValue (int (Min), int (Max))));

   procedure Set_Random_Seed (Seed : Natural) is
   begin
      raylib_h.SetRandomSeed (unsigned (Seed));
   end Set_Random_Seed;

   procedure Take_Screenshot (File_Name : String) is
      File_Name_Copy : chars_ptr := New_String (File_Name);
   begin
      raylib_h.TakeScreenshot (File_Name_Copy);
      Free (File_Name_Copy);
   end Take_Screenshot;

   procedure Set_Config_Flags (Flags : Config_Flag) is
   begin
      raylib_h.SetConfigFlags (unsigned (Flags));
   end Set_Config_Flags;

   procedure Trace_Log (Log_Level : Trace_Log_Level; Text : String) is
      procedure Internal (logLevel : int; text : chars_ptr) with
         Import,
         Convention    => C,
         External_Name => "Ada_Trace_Log";

      Text_Copy : chars_ptr := New_String (Text);
   begin
      Internal (int (Log_Level), Text_Copy);
      Free (Text_Copy);
   end Trace_Log;

   procedure Set_Trace_Log_Level (Log_Level : Trace_Log_Level) is
   begin
      raylib_h.SetTraceLogLevel (int (Log_Level));
   end Set_Trace_Log_Level;

   procedure Set_Trace_Log_Callback (Callback : RayLib.Trace_Log_Callback) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Trace_Log_Callback unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Trace_Log_Callback";
   end Set_Trace_Log_Callback;

   procedure Set_Load_File_Data_Callback
     (Callback : RayLib.Load_File_Data_Callback)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Load_File_Data_Callback unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Load_File_Data_Callback";
   end Set_Load_File_Data_Callback;

   procedure Set_Save_File_Data_Callback
     (Callback : RayLib.Save_File_Data_Callback)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Save_File_Data_Callback unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Save_File_Data_Callback";
   end Set_Save_File_Data_Callback;

   procedure Set_Load_File_Text_Callback
     (Callback : RayLib.Load_File_Text_Callback)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Load_File_Text_Callback unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Load_File_Text_Callback";
   end Set_Load_File_Text_Callback;

   procedure Set_Save_File_Text_Callback
     (Callback : RayLib.Save_File_Text_Callback)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Save_File_Text_Callback unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Save_File_Text_Callback";
   end Set_Save_File_Text_Callback;

   function Load_File_Data (File_Name : String) return Stream_Element_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_File_Data unimplemented");
      return raise Program_Error with "Unimplemented function Load_File_Data";
   end Load_File_Data;

   function Save_File_Data
     (File_Name : String; Data : Stream_Element_Array) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Save_File_Data unimplemented");
      return raise Program_Error with "Unimplemented function Save_File_Data";
   end Save_File_Data;

   function Load_File_Text (File_Name : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_File_Text unimplemented");
      return raise Program_Error with "Unimplemented function Load_File_Text";
   end Load_File_Text;

   function Save_File_Text (File_Name : String; Text : String) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Save_File_Text unimplemented");
      return raise Program_Error with "Unimplemented function Save_File_Text";
   end Save_File_Text;

   function File_Exists (File_Name : String) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "File_Exists unimplemented");
      return raise Program_Error with "Unimplemented function File_Exists";
   end File_Exists;

   function Directory_Exists (Dir_Path : String) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Directory_Exists unimplemented");
      return
        raise Program_Error with "Unimplemented function Directory_Exists";
   end Directory_Exists;

   function Is_File_Extension (File_Name : String; Ext : String) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_File_Extension unimplemented");
      return
        raise Program_Error with "Unimplemented function Is_File_Extension";
   end Is_File_Extension;

   function Get_File_Length (File_Name : String) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_File_Length unimplemented");
      return raise Program_Error with "Unimplemented function Get_File_Length";
   end Get_File_Length;

   function Get_File_Extension (File_Name : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_File_Extension unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_File_Extension";
   end Get_File_Extension;

   function Get_File_Name (File_Path : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_File_Name unimplemented");
      return raise Program_Error with "Unimplemented function Get_File_Name";
   end Get_File_Name;

   function Get_File_Name_Without_Ext (File_Path : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_File_Name_Without_Ext unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_File_Name_Without_Ext";
   end Get_File_Name_Without_Ext;

   function Get_Directory_Path (File_Path : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Directory_Path unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Directory_Path";
   end Get_Directory_Path;

   function Get_Prev_Directory_Path (Dir_Path : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Prev_Directory_Path unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_Prev_Directory_Path";
   end Get_Prev_Directory_Path;

   function Get_Working_Directory return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Working_Directory unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_Working_Directory";
   end Get_Working_Directory;

   function Get_Application_Directory return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Application_Directory unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_Application_Directory";
   end Get_Application_Directory;

   function Get_Directory_Files
     (Dir_Path : String) return Unbounded_String_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Directory_Files unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Directory_Files";
   end Get_Directory_Files;

   function Change_Directory (Dir : String) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Change_Directory unimplemented");
      return
        raise Program_Error with "Unimplemented function Change_Directory";
   end Change_Directory;

   function Is_File_Dropped return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Is_File_Dropped unimplemented");
      return raise Program_Error with "Unimplemented function Is_File_Dropped";
   end Is_File_Dropped;

   function Get_Dropped_Files return Unbounded_String_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Dropped_Files unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Dropped_Files";
   end Get_Dropped_Files;

   function Get_File_Mod_Time (File_Name : String) return Time is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_File_Mod_Time unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_File_Mod_Time";
   end Get_File_Mod_Time;

   function Compress_Data
     (Data : Stream_Element_Array) return Stream_Element_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Compress_Data unimplemented");
      return raise Program_Error with "Unimplemented function Compress_Data";
   end Compress_Data;

   function Decompress_Data
     (Comp_Data : Stream_Element_Array) return Stream_Element_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Decompress_Data unimplemented");
      return raise Program_Error with "Unimplemented function Decompress_Data";
   end Decompress_Data;

   function Encode_Data_Base64 (Data : Stream_Element_Array) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Encode_Data_Base64 unimplemented");
      return
        raise Program_Error with "Unimplemented function Encode_Data_Base64";
   end Encode_Data_Base64;

   function Decode_Data_Base64 (Data : String) return Stream_Element_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Decode_Data_Base64 unimplemented");
      return
        raise Program_Error with "Unimplemented function Decode_Data_Base64";
   end Decode_Data_Base64;

   function Save_Storage_Value
     (Position : Natural; Value : Integer) return Boolean is
     (Boolean (raylib_h.SaveStorageValue (unsigned (Position), int (Value))));

   function Load_Storage_Value (Position : Natural) return Integer is
     (Integer (raylib_h.LoadStorageValue (unsigned (Position))));

   procedure Open_URL (Url : String) is
      Url_Copy : chars_ptr := New_String (Url);
   begin
      raylib_h.OpenURL (Url_Copy);
      Free (Url_Copy);
   end Open_URL;

   function Is_Key_Pressed (Key : Keyboard_Key) return Boolean is
     (Boolean (raylib_h.IsKeyPressed (int (Key))));

   function Is_Key_Down (Key : Keyboard_Key) return Boolean is
     (Boolean (raylib_h.IsKeyDown (int (Key))));

   function Is_Key_Released (Key : Keyboard_Key) return Boolean is
     (Boolean (raylib_h.IsKeyReleased (int (Key))));

   function Is_Key_Up (Key : Keyboard_Key) return Boolean is
     (Boolean (raylib_h.IsKeyUp (int (Key))));

   procedure Set_Exit_Key (Key : Keyboard_Key) is
   begin
      raylib_h.SetExitKey (int (Key));
   end Set_Exit_Key;

   function Get_Key_Pressed return Keyboard_Key is
     (Keyboard_Key (raylib_h.GetKeyPressed));

   function Get_Char_Pressed return Character is
     (Character'Val (raylib_h.GetCharPressed));

   function Is_Gamepad_Available (Gamepad : Gamepad_Id) return Boolean is
     (Boolean (raylib_h.IsGamepadAvailable (int (Gamepad))));

   function Get_Gamepad_Name (Gamepad : Gamepad_Id) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Gamepad_Name unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Gamepad_Name";
   end Get_Gamepad_Name;

   function Is_Gamepad_Button_Pressed
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean is
     (Boolean (raylib_h.IsGamepadButtonPressed (int (Gamepad), int (Button))));

   function Is_Gamepad_Button_Down
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean is
     (Boolean (raylib_h.IsGamepadButtonDown (int (Gamepad), int (Button))));

   function Is_Gamepad_Button_Released
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean is
     (Boolean
        (raylib_h.IsGamepadButtonReleased (int (Gamepad), int (Button))));

   function Is_Gamepad_Button_Up
     (Gamepad : Gamepad_Id; Button : Gamepad_Button) return Boolean is
     (Boolean (raylib_h.IsGamepadButtonUp (int (Gamepad), int (Button))));

   function Get_Gamepad_Button_Pressed return Gamepad_Button is
     (Gamepad_Button (raylib_h.GetGamepadButtonPressed));

   function Get_Gamepad_Axis_Count (Gamepad : Gamepad_Id) return Natural is
     (Natural (raylib_h.GetGamepadAxisCount (int (Gamepad))));

   function Get_Gamepad_Axis_Movement
     (Gamepad : Gamepad_Id; Axis : Gamepad_Axis) return Float is
     (raylib_h.GetGamepadAxisMovement (int (Gamepad), int (Axis)));

   function Set_Gamepad_Mappings (Mappings : String) return Integer is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Gamepad_Mappings unimplemented");
      return
        raise Program_Error with "Unimplemented function Set_Gamepad_Mappings";
   end Set_Gamepad_Mappings;

   function Is_Mouse_Button_Pressed (Button : Mouse_Button) return Boolean is
     (Boolean (raylib_h.IsMouseButtonPressed (int (Button))));

   function Is_Mouse_Button_Down (Button : Mouse_Button) return Boolean is
     (Boolean (raylib_h.IsMouseButtonDown (int (Button))));

   function Is_Mouse_Button_Released (Button : Mouse_Button) return Boolean is
     (Boolean (raylib_h.IsMouseButtonReleased (int (Button))));

   function Is_Mouse_Button_Up (Button : Mouse_Button) return Boolean is
     (Boolean (raylib_h.IsMouseButtonUp (int (Button))));

   function Get_Mouse_X return Natural is (Natural (raylib_h.GetMouseX));

   function Get_Mouse_Y return Natural is (Natural (raylib_h.GetMouseY));

   function Get_Mouse_Position return RayLib.Vector2 is
     (+raylib_h.GetMousePosition);

   function Get_Mouse_Delta return RayLib.Vector2 is (+raylib_h.GetMouseDelta);

   procedure Set_Mouse_Position (X : Natural; Y : Natural) is
   begin
      raylib_h.SetMousePosition (int (X), int (Y));
   end Set_Mouse_Position;

   procedure Set_Mouse_Offset (Offset_X : Integer; Offset_Y : Integer) is
   begin
      raylib_h.SetMouseOffset (int (Offset_X), int (Offset_Y));
   end Set_Mouse_Offset;

   procedure Set_Mouse_Scale (Scale_X : Float; Scale_Y : Float) is
   begin
      raylib_h.SetMouseScale (Scale_X, Scale_Y);
   end Set_Mouse_Scale;

   function Get_Mouse_Wheel_Move return Float is (raylib_h.GetMouseWheelMove);

   procedure Set_Mouse_Cursor (Cursor : Mouse_Cursor) is
   begin
      raylib_h.SetMouseCursor (int (Cursor));
   end Set_Mouse_Cursor;

   function Get_Touch_X return Natural is (Natural (raylib_h.GetTouchX));

   function Get_Touch_Y return Natural is (Natural (raylib_h.GetTouchX));

   function Get_Touch_Position (Index : Natural) return RayLib.Vector2 is
     (+raylib_h.GetTouchPosition (int (Index)));

   function Get_Touch_Point_Id (Index : Natural) return Natural is
     (Natural (raylib_h.GetTouchPointId (int (Index))));

   function Get_Touch_Point_Count return Natural is
     (Natural (raylib_h.GetTouchPointCount));

   procedure Set_Gestures_Enabled (Flags : RayLib.Gesture) is
   begin
      raylib_h.SetGesturesEnabled (unsigned (Flags));
   end Set_Gestures_Enabled;

   function Is_Gesture_Detected (Gesture : RayLib.Gesture) return Boolean is
     (Boolean (raylib_h.IsGestureDetected (int (Gesture))));

   function Get_Gesture_Detected return RayLib.Gesture is
     (Gesture (raylib_h.GetGestureDetected));

   function Get_Gesture_Hold_Duration return Float is
     (raylib_h.GetGestureHoldDuration);

   function Get_Gesture_Drag_Vector return RayLib.Vector2 is
     (+raylib_h.GetGestureDragVector);

   function Get_Gesture_Drag_Angle return Float is
     (raylib_h.GetGestureDragAngle);

   function Get_Gesture_Pinch_Vector return RayLib.Vector2 is
     (+raylib_h.GetGesturePinchVector);

   function Get_Gesture_Pinch_Angle return Float is
     (raylib_h.GetGesturePinchAngle);

   procedure Set_Camera_Mode (Camera : RayLib.Camera; Mode : Camera_Mode) is
   begin
      raylib_h.SetCameraMode (+Camera, int (Mode));
   end Set_Camera_Mode;

   procedure Update_Camera (Camera : in out RayLib.Camera) is
      Camera_Copy : aliased raylib_h.Camera := +Camera;
   begin
      raylib_h.UpdateCamera (Camera_Copy'Access);
      Camera := +Camera_Copy;
   end Update_Camera;

   procedure Set_Camera_Pan_Control (Key_Pan : Keyboard_Key) is
   begin
      raylib_h.SetCameraPanControl (int (Key_Pan));
   end Set_Camera_Pan_Control;

   procedure Set_Camera_Alt_Control (Key_Alt : Keyboard_Key) is
   begin
      raylib_h.SetCameraAltControl (int (Key_Alt));
   end Set_Camera_Alt_Control;

   procedure Set_Camera_Smooth_Zoom_Control (Key_Smooth_Zoom : Keyboard_Key) is
   begin
      raylib_h.SetCameraSmoothZoomControl (int (Key_Smooth_Zoom));
   end Set_Camera_Smooth_Zoom_Control;

   procedure Set_Camera_Move_Controls
     (Key_Front : Keyboard_Key; Key_Back : Keyboard_Key;
      Key_Right : Keyboard_Key; Key_Left : Keyboard_Key; Key_Up : Keyboard_Key;
      Key_Down  : Keyboard_Key)
   is
   begin
      raylib_h.SetCameraMoveControls
        (int (Key_Front), int (Key_Back), int (Key_Right), int (Key_Left),
         int (Key_Up), int (Key_Down));
   end Set_Camera_Move_Controls;

   procedure Set_Shapes_Texture
     (Texture : RayLib.Texture2D'Class; Source : RayLib.Rectangle)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Shapes_Texture unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Shapes_Texture";
   end Set_Shapes_Texture;

   procedure Draw_Pixel
     (Pos_X : Natural; Pos_Y : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawPixel (int (Pos_X), int (Pos_Y), +Color);
   end Draw_Pixel;

   procedure Draw_Pixel (Position : RayLib.Vector2; Color : RayLib.Color) is
   begin
      raylib_h.DrawPixelV (+Position, +Color);
   end Draw_Pixel;

   procedure Draw_Line
     (Start_Pos_X : Natural; Start_Pos_Y : Natural; End_Pos_X : Natural;
      End_Pos_Y   : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawLine
        (int (Start_Pos_X), int (Start_Pos_Y), int (End_Pos_X),
         int (End_Pos_Y), +Color);
   end Draw_Line;

   procedure Draw_Line
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Color     : RayLib.Color)
   is
   begin
      raylib_h.DrawLineV (+Start_Pos, +End_Pos, +Color);
   end Draw_Line;

   procedure Draw_Line
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2; Thick : Float;
      Color     : RayLib.Color)
   is
   begin
      raylib_h.DrawLineEx (+Start_Pos, +End_Pos, Thick, +Color);
   end Draw_Line;

   procedure Draw_Line_Bezier
     (Start_Pos : RayLib.Vector2; End_Pos : RayLib.Vector2; Thick : Float;
      Color     : RayLib.Color)
   is
   begin
      raylib_h.DrawLineBezier (+Start_Pos, +End_Pos, Thick, +Color);
   end Draw_Line_Bezier;

   procedure Draw_Line_Bezier_Quad
     (Start_Pos   : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Control_Pos : RayLib.Vector2; Thick : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawLineBezierQuad
        (+Start_Pos, +End_Pos, +Control_Pos, Thick, +Color);
   end Draw_Line_Bezier_Quad;

   procedure Draw_Line_Bezier_Cubic
     (Start_Pos         : RayLib.Vector2; End_Pos : RayLib.Vector2;
      Start_Control_Pos : RayLib.Vector2; End_Control_Pos : RayLib.Vector2;
      Thick             : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawLineBezierCubic
        (+Start_Pos, +End_Pos, +Start_Control_Pos, +End_Control_Pos, Thick,
         +Color);
   end Draw_Line_Bezier_Cubic;

   procedure Draw_Line_Strip
     (Points : RayLib.Vector2_Array; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Line_Strip unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Line_Strip";
   end Draw_Line_Strip;

   procedure Draw_Circle
     (Center_X : Natural; Center_Y : Natural; Radius : Float;
      Color    : RayLib.Color)
   is
   begin
      raylib_h.DrawCircle (int (Center_X), int (Center_Y), Radius, +Color);
   end Draw_Circle;

   procedure Draw_Circle_Sector
     (Center    : RayLib.Vector2; Radius : Float; Start_Angle : Float;
      End_Angle : Float; Segments : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCircleSector
        (+Center, Radius, Start_Angle, End_Angle, int (Segments), +Color);
   end Draw_Circle_Sector;

   procedure Draw_Circle_Sector_Lines
     (Center    : RayLib.Vector2; Radius : Float; Start_Angle : Float;
      End_Angle : Float; Segments : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCircleSectorLines
        (+Center, Radius, Start_Angle, End_Angle, int (Segments), +Color);
   end Draw_Circle_Sector_Lines;

   procedure Draw_Circle_Gradient
     (Center_X : Natural; Center_Y : Natural; Radius : Float;
      Color1   : RayLib.Color; Color2 : RayLib.Color)
   is
   begin
      raylib_h.DrawCircleGradient
        (int (Center_X), int (Center_Y), Radius, +Color1, +Color2);
   end Draw_Circle_Gradient;

   procedure Draw_Circle
     (Center : RayLib.Vector2; Radius : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCircleV (+Center, Radius, +Color);
   end Draw_Circle;

   procedure Draw_Circle_Lines
     (Center_X : Natural; Center_Y : Natural; Radius : Float;
      Color    : RayLib.Color)
   is
   begin
      raylib_h.DrawCircleLines
        (int (Center_X), int (Center_Y), Radius, +Color);
   end Draw_Circle_Lines;

   procedure Draw_Ellipse
     (Center_X : Natural; Center_Y : Natural; Radius_H : Float;
      Radius_V : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawEllipse
        (int (Center_X), int (Center_Y), Radius_H, Radius_V, +Color);
   end Draw_Ellipse;

   procedure Draw_Ellipse_Lines
     (Center_X : Natural; Center_Y : Natural; Radius_H : Float;
      Radius_V : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawEllipseLines
        (int (Center_X), int (Center_Y), Radius_H, Radius_V, +Color);
   end Draw_Ellipse_Lines;

   procedure Draw_Ring
     (Center      : RayLib.Vector2; Inner_Radius : Float; Outer_Radius : Float;
      Start_Angle : Float; End_Angle : Float; Segments : Natural;
      Color       : RayLib.Color)
   is
   begin
      raylib_h.DrawRing
        (+Center, Inner_Radius, Outer_Radius, Start_Angle, End_Angle,
         int (Segments), +Color);
   end Draw_Ring;

   procedure Draw_Ring_Lines
     (Center      : RayLib.Vector2; Inner_Radius : Float; Outer_Radius : Float;
      Start_Angle : Float; End_Angle : Float; Segments : Natural;
      Color       : RayLib.Color)
   is
   begin
      raylib_h.DrawRingLines
        (+Center, Inner_Radius, Outer_Radius, Start_Angle, End_Angle,
         int (Segments), +Color);
   end Draw_Ring_Lines;

   procedure Draw_Rectangle
     (Pos_X : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangle
        (int (Pos_X), int (Pos_Y), int (Width), int (Height), +Color);
   end Draw_Rectangle;

   procedure Draw_Rectangle
     (Position : RayLib.Vector2; Size : RayLib.Vector2; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleV (+Position, +Size, +Color);
   end Draw_Rectangle;

   procedure Draw_Rectangle (Rec : RayLib.Rectangle; Color : RayLib.Color) is
   begin
      raylib_h.DrawRectangleRec (+Rec, +Color);
   end Draw_Rectangle;

   procedure Draw_Rectangle
     (Rec   : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectanglePro (+Rec, +Origin, Rotation, +Color);
   end Draw_Rectangle;

   procedure Draw_Rectangle_Gradient_Vertical
     (Pos_X  : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color1 : RayLib.Color; Color2 : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleGradientV
        (int (Pos_X), int (Pos_Y), int (Width), int (Height), +Color1,
         +Color2);
   end Draw_Rectangle_Gradient_Vertical;

   procedure Draw_Rectangle_Gradient_Horizontal
     (Pos_X  : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color1 : RayLib.Color; Color2 : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleGradientH
        (int (Pos_X), int (Pos_Y), int (Width), int (Height), +Color1,
         +Color2);
   end Draw_Rectangle_Gradient_Horizontal;

   procedure Draw_Rectangle_Gradient
     (Rec  : RayLib.Rectangle; Col1 : RayLib.Color; Col2 : RayLib.Color;
      Col3 : RayLib.Color; Col4 : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleGradientEx (+Rec, +Col1, +Col2, +Col3, +Col4);
   end Draw_Rectangle_Gradient;

   procedure Draw_Rectangle_Lines
     (Pos_X : Natural; Pos_Y : Natural; Width : Natural; Height : Natural;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleLines
        (int (Pos_X), int (Pos_Y), int (Width), int (Height), +Color);
   end Draw_Rectangle_Lines;

   procedure Draw_Rectangle_Lines
     (Rec : RayLib.Rectangle; Line_Thick : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleLinesEx (+Rec, Line_Thick, +Color);
   end Draw_Rectangle_Lines;

   procedure Draw_Rectangle_Rounded
     (Rec   : RayLib.Rectangle; Roundness : Float; Segments : Natural;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleRounded (+Rec, Roundness, int (Segments), +Color);
   end Draw_Rectangle_Rounded;

   procedure Draw_Rectangle_Rounded_Lines
     (Rec        : RayLib.Rectangle; Roundness : Float; Segments : Natural;
      Line_Thick : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawRectangleRoundedLines
        (+Rec, Roundness, int (Segments), Line_Thick, +Color);
   end Draw_Rectangle_Rounded_Lines;

   procedure Draw_Triangle
     (V1    : RayLib.Vector2; V2 : RayLib.Vector2; V3 : RayLib.Vector2;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawTriangle (+V1, +V2, +V3, +Color);
   end Draw_Triangle;

   procedure Draw_Triangle_Lines
     (V1    : RayLib.Vector2; V2 : RayLib.Vector2; V3 : RayLib.Vector2;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawTriangleLines (+V1, +V2, +V3, +Color);
   end Draw_Triangle_Lines;

   procedure Draw_Triangle_Fan
     (Points : RayLib.Vector2_Array; Color : RayLib.Color)
   is
      type Vec2_Array is array (Natural range <>) of aliased raylib_h.Vector2;
      Points_Copy : aliased Vec2_Array (Points'First .. Points'Last);
   begin
      for I in Points'Range loop
         Points_Copy (I) := +Points (I);
      end loop;
      raylib_h.DrawTriangleFan
        (Points_Copy (Points_Copy'First)'Access, int (Points_Copy'Length),
         +Color);
   end Draw_Triangle_Fan;

   procedure Draw_Triangle_Strip
     (Points : RayLib.Vector2_Array; Color : RayLib.Color)
   is
      type Vec2_Array is array (Natural range <>) of aliased raylib_h.Vector2;
      Points_Copy : aliased Vec2_Array (Points'First .. Points'Last);
   begin
      for I in Points'Range loop
         Points_Copy (I) := +Points (I);
      end loop;
      raylib_h.DrawTriangleStrip
        (Points_Copy (Points_Copy'First)'Access, int (Points_Copy'Length),
         +Color);
   end Draw_Triangle_Strip;

   procedure Draw_Poly
     (Center   : RayLib.Vector2; Sides : Natural; Radius : Float;
      Rotation : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawPoly (+Center, int (Sides), Radius, Rotation, +Color);
   end Draw_Poly;

   procedure Draw_Poly_Lines
     (Center   : RayLib.Vector2; Sides : Natural; Radius : Float;
      Rotation : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawPolyLines (+Center, int (Sides), Radius, Rotation, +Color);
   end Draw_Poly_Lines;

   procedure Draw_Poly_Lines
     (Center   : RayLib.Vector2; Sides : Natural; Radius : Float;
      Rotation : Float; Line_Thick : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawPolyLinesEx
        (+Center, int (Sides), Radius, Rotation, Line_Thick, +Color);
   end Draw_Poly_Lines;

   function Check_Collision_Recs
     (Rec1 : RayLib.Rectangle; Rec2 : RayLib.Rectangle) return Boolean is
     (Boolean (raylib_h.CheckCollisionRecs (+Rec1, +Rec2)));

   function Check_Collision_Circles
     (Center1 : RayLib.Vector2; Radius1 : Float; Center2 : RayLib.Vector2;
      Radius2 : Float) return Boolean is
     (Boolean
        (raylib_h.CheckCollisionCircles
           (+Center1, Radius1, +Center2, Radius2)));

   function Check_Collision_Circle_Rec
     (Center : RayLib.Vector2; Radius : Float; Rec : RayLib.Rectangle)
      return Boolean is
     (Boolean (raylib_h.CheckCollisionCircleRec (+Center, Radius, +Rec)));

   function Check_Collision_Point_Rec
     (Point : RayLib.Vector2; Rec : RayLib.Rectangle) return Boolean is
     (Boolean (raylib_h.CheckCollisionPointRec (+Point, +Rec)));

   function Check_Collision_Point_Circle
     (Point : RayLib.Vector2; Center : RayLib.Vector2; Radius : Float)
      return Boolean is
     (Boolean (raylib_h.CheckCollisionPointCircle (+Point, +Center, Radius)));

   function Check_Collision_Point_Triangle
     (Point : RayLib.Vector2; P1 : RayLib.Vector2; P2 : RayLib.Vector2;
      P3    : RayLib.Vector2) return Boolean is
     (Boolean (raylib_h.CheckCollisionPointTriangle (+Point, +P1, +P2, +P3)));

   function Check_Collision_Lines
     (Start_Pos1      :     RayLib.Vector2; End_Pos1 : RayLib.Vector2;
      Start_Pos2      :     RayLib.Vector2; End_Pos2 : RayLib.Vector2;
      Collision_Point : out RayLib.Vector2) return Boolean
   is
      Collision_Point_Copy : aliased raylib_h.Vector2;
      Result               : Boolean;
   begin
      Result :=
        Boolean
          (raylib_h.CheckCollisionLines
             (+Start_Pos1, +End_Pos1, +Start_Pos2, +End_Pos2,
              Collision_Point_Copy'Access));
      Collision_Point := +Collision_Point_Copy;
      return Result;
   end Check_Collision_Lines;

   function Check_Collision_Point_Line
     (Point     : RayLib.Vector2; P1 : RayLib.Vector2; P2 : RayLib.Vector2;
      Threshold : Natural) return Boolean is
     (Boolean
        (raylib_h.CheckCollisionPointLine
           (+Point, +P1, +P2, int (Threshold))));

   function Get_Collision_Rec
     (Rec1 : RayLib.Rectangle; Rec2 : RayLib.Rectangle)
      return RayLib.Rectangle is
     (+raylib_h.GetCollisionRec (+Rec1, +Rec2));

   function Load_Image (File_Name : String) return RayLib.Image is
      File_Name_Copy : chars_ptr    := New_String (File_Name);
      Result         : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data := raylib_h.LoadImage (File_Name_Copy);
      Free (File_Name_Copy);
      return Result;
   end Load_Image;

   function Load_Image_Raw
     (File_Name : String; Width : Natural; Height : Natural;
      Format    : Pixel_Format; Header_Size : Natural) return RayLib.Image
   is
      File_Name_Copy : chars_ptr    := New_String (File_Name);
      Result         : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadImageRaw
          (File_Name_Copy, int (Width), int (Height), int (Format),
           int (Header_Size));
      Free (File_Name_Copy);
      return Result;
   end Load_Image_Raw;

   function Load_Image_Anim
     (File_Name : String; Frames : out Natural) return RayLib.Image
   is
      File_Name_Copy : chars_ptr    := New_String (File_Name);
      Frames_Copy    : aliased int;
      Result         : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadImageAnim (File_Name_Copy, Frames_Copy'Access);
      Frames := Natural (Frames_Copy);
      Free (File_Name_Copy);
      return Result;
   end Load_Image_Anim;

   function Load_Image_From_Memory
     (File_Type : String; File_Data : Stream_Element_Array) return RayLib.Image
   is
      type File_Data_Type is
        array
          (int range int (File_Data'First) ..
               int (File_Data'Last)) of aliased unsigned_char;
      function Convert is new Ada.Unchecked_Conversion
        (Stream_Element_Array, File_Data_Type);

      File_Type_Copy : chars_ptr              := New_String (File_Type);
      File_Data_Copy : aliased File_Data_Type := Convert (File_Data);
      Result         : RayLib.Image           :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadImageFromMemory
          (File_Type_Copy, File_Data_Copy (File_Data_Copy'First)'Access,
           File_Data_Copy'Length);
      Free (File_Type_Copy);
      return Result;
   end Load_Image_From_Memory;

   function Load_Image_From_Texture
     (Texture : RayLib.Texture2D'Class) return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadImageFromTexture (Texture.Payload.all.Data);
      return Result;
   end Load_Image_From_Texture;

   function Load_Image_From_Screen return RayLib.Image is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data := raylib_h.LoadImageFromScreen;
      return Result;
   end Load_Image_From_Screen;

   function Export_Image
     (Image : RayLib.Image'Class; File_Name : String) return Boolean
   is
      File_Name_Copy : chars_ptr := New_String (File_Name);
      Result         : Boolean;
   begin
      Result :=
        Boolean
          (raylib_h.ExportImage (Image.Payload.all.Data, File_Name_Copy));
      Free (File_Name_Copy);
      return Result;
   end Export_Image;

   function Export_Image_As_Code
     (Image : RayLib.Image'Class; File_Name : String) return Boolean
   is
      File_Name_Copy : chars_ptr := New_String (File_Name);
      Result         : Boolean;
   begin
      Result :=
        Boolean
          (raylib_h.ExportImageAsCode
             (Image.Payload.all.Data, File_Name_Copy));
      Free (File_Name_Copy);
      return Result;
   end Export_Image_As_Code;

   function Gen_Image_Color
     (Width : Natural; Height : Natural; Color : RayLib.Color)
      return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageColor (int (Width), int (Height), +Color);
      return Result;
   end Gen_Image_Color;

   function Gen_Image_Gradient_Vertical
     (Width  : Natural; Height : Natural; Top : RayLib.Color;
      Bottom : RayLib.Color) return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageGradientV (int (Width), int (Height), +Top, +Bottom);
      return Result;
   end Gen_Image_Gradient_Vertical;

   function Gen_Image_Gradient_Horizontal
     (Width : Natural; Height : Natural; Left : RayLib.Color;
      Right : RayLib.Color) return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageGradientH (int (Width), int (Height), +Left, +Right);
      return Result;
   end Gen_Image_Gradient_Horizontal;

   function Gen_Image_Gradient_Radial
     (Width : Natural; Height : Natural; Density : Float; Inner : RayLib.Color;
      Outer : RayLib.Color) return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageGradientRadial
          (int (Width), int (Height), Density, +Inner, +Outer);
      return Result;
   end Gen_Image_Gradient_Radial;

   function Gen_Image_Checked
     (Width    : Natural; Height : Natural; Checks_X : Natural;
      Checks_Y : Natural; Col1 : RayLib.Color; Col2 : RayLib.Color)
      return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageChecked
          (int (Width), int (Height), int (Checks_X), int (Checks_Y), +Col1,
           +Col2);
      return Result;
   end Gen_Image_Checked;

   function Gen_Image_White_Noise
     (Width : Natural; Height : Natural; Factor : Float) return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageWhiteNoise (int (Width), int (Height), Factor);
      return Result;
   end Gen_Image_White_Noise;

   function Gen_Image_Cellular
     (Width : Natural; Height : Natural; Tile_Size : Natural)
      return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.GenImageCellular (int (Width), int (Height), int (Tile_Size));
      return Result;
   end Gen_Image_Cellular;

   function Image_Copy (Image : RayLib.Image'Class) return RayLib.Image is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data := raylib_h.ImageCopy (Image.Payload.all.Data);
      return Result;
   end Image_Copy;

   function Image_From_Image
     (Image : RayLib.Image'Class; Rec : RayLib.Rectangle) return RayLib.Image
   is
      Result : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.ImageFromImage (Image.Payload.all.Data, +Rec);
      return Result;
   end Image_From_Image;

   function Image_Text
     (Text : String; Font_Size : Natural; Color : RayLib.Color)
      return RayLib.Image
   is
      Text_Copy : chars_ptr    := New_String (Text);
      Result    : RayLib.Image :=
        (Ada.Finalization.Controlled with Payload => new Image_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.ImageText (Text_Copy, int (Font_Size), +Color);
      Free (Text_Copy);
      return Result;
   end Image_Text;

   function Image_Text
     (Font    : RayLib.Font'Class; Text : String; Font_Size : Float;
      Spacing : Float; Tint : RayLib.Color) return RayLib.Image
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Image_Text unimplemented");
      return raise Program_Error with "Unimplemented function Image_Text";
   end Image_Text;

   procedure Image_Format
     (Image : in out RayLib.Image'Class; New_Format : Pixel_Format)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Format unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Format";
   end Image_Format;

   procedure Image_To_POT
     (Image : in out RayLib.Image'Class; Fill : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_To_POT unimplemented");
      raise Program_Error with "Unimplemented procedure Image_To_POT";
   end Image_To_POT;

   procedure Image_Crop
     (Image : in out RayLib.Image'Class; Crop : RayLib.Rectangle)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Image_Crop unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Crop";
   end Image_Crop;

   procedure Image_Alpha_Crop
     (Image : in out RayLib.Image'Class; Threshold : Float)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Alpha_Crop unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Alpha_Crop";
   end Image_Alpha_Crop;

   procedure Image_Alpha_Clear
     (Image     : in out RayLib.Image'Class; Color : RayLib.Color;
      Threshold :        Float)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Alpha_Clear unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Alpha_Clear";
   end Image_Alpha_Clear;

   procedure Image_Alpha_Mask
     (Image : in out RayLib.Image'Class; Alpha_Mask : RayLib.Image'Class)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Alpha_Mask unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Alpha_Mask";
   end Image_Alpha_Mask;

   procedure Image_Alpha_Premultiply (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Alpha_Premultiply unimplemented");
      raise Program_Error
        with "Unimplemented procedure Image_Alpha_Premultiply";
   end Image_Alpha_Premultiply;

   procedure Image_Resize
     (Image      : in out RayLib.Image'Class; New_Width : Natural;
      New_Height :        Natural)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Resize unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Resize";
   end Image_Resize;

   procedure Image_Resize_NN
     (Image      : in out RayLib.Image'Class; New_Width : Natural;
      New_Height :        Natural)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Resize_NN unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Resize_NN";
   end Image_Resize_NN;

   procedure Image_Resize_Canvas
     (Image      : in out RayLib.Image'Class; New_Width : Natural;
      New_Height :        Natural; Offset_X : Natural; Offset_Y : Natural;
      Fill       :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Resize_Canvas unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Resize_Canvas";
   end Image_Resize_Canvas;

   procedure Image_Mipmaps (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Mipmaps unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Mipmaps";
   end Image_Mipmaps;

   procedure Image_Dither
     (Image : in out RayLib.Image'Class; R_Bpp : Natural; G_Bpp : Natural;
      B_Bpp :        Natural; A_Bpp : Natural)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Dither unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Dither";
   end Image_Dither;

   procedure Image_Flip_Vertical (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Flip_Vertical unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Flip_Vertical";
   end Image_Flip_Vertical;

   procedure Image_Flip_Horizontal (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Flip_Horizontal unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Flip_Horizontal";
   end Image_Flip_Horizontal;

   procedure Image_Rotate_CW (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Rotate_CW unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Rotate_CW";
   end Image_Rotate_CW;

   procedure Image_Rotate_CCW (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Rotate_CCW unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Rotate_CCW";
   end Image_Rotate_CCW;

   procedure Image_Color_Tint
     (Image : in out RayLib.Image'Class; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Color_Tint unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Color_Tint";
   end Image_Color_Tint;

   procedure Image_Color_Invert (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Color_Invert unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Color_Invert";
   end Image_Color_Invert;

   procedure Image_Color_Grayscale (Image : in out RayLib.Image'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Color_Grayscale unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Color_Grayscale";
   end Image_Color_Grayscale;

   procedure Image_Color_Contrast
     (Image : in out RayLib.Image'Class; Contrast : Float)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Color_Contrast unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Color_Contrast";
   end Image_Color_Contrast;

   procedure Image_Color_Brightness
     (Image : in out RayLib.Image'Class; Brightness : Integer)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Color_Brightness unimplemented");
      raise Program_Error
        with "Unimplemented procedure Image_Color_Brightness";
   end Image_Color_Brightness;

   procedure Image_Color_Replace
     (Image   : in out RayLib.Image'Class; Color : RayLib.Color;
      Replace :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Color_Replace unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Color_Replace";
   end Image_Color_Replace;

   function Load_Image_Colors
     (Image : RayLib.Image'Class) return RayLib.Color_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Image_Colors unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Image_Colors";
   end Load_Image_Colors;

   function Load_Image_Palette
     (Image : RayLib.Image'Class; Max_Palette_Size : Natural)
      return RayLib.Color_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Image_Palette unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Image_Palette";
   end Load_Image_Palette;

   function Get_Image_Alpha_Border
     (Image : RayLib.Image'Class; Threshold : Float) return RayLib.Rectangle
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Image_Alpha_Border unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_Image_Alpha_Border";
   end Get_Image_Alpha_Border;

   function Get_Image_Color
     (Image : RayLib.Image'Class; X : Natural; Y : Natural) return RayLib.Color
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Image_Color unimplemented");
      return raise Program_Error with "Unimplemented function Get_Image_Color";
   end Get_Image_Color;

   procedure Image_Clear_Background
     (Dst : in out RayLib.Image'Class; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Clear_Background unimplemented");
      raise Program_Error
        with "Unimplemented procedure Image_Clear_Background";
   end Image_Clear_Background;

   procedure Image_Draw_Pixel
     (Dst   : in out RayLib.Image'Class; Pos_X : Natural; Pos_Y : Natural;
      Color :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Pixel unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Pixel";
   end Image_Draw_Pixel;

   procedure Image_Draw_Pixel
     (Dst   : in out RayLib.Image'Class; Position : RayLib.Vector2;
      Color :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Pixel unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Pixel";
   end Image_Draw_Pixel;

   procedure Image_Draw_Line
     (Dst         : in out RayLib.Image'Class; Start_Pos_X : Natural;
      Start_Pos_Y :        Natural; End_Pos_X : Natural; End_Pos_Y : Natural;
      Color       :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Line";
   end Image_Draw_Line;

   procedure Image_Draw_Line
     (Dst    : in out RayLib.Image'Class; Start : RayLib.Vector2;
      Finish :        RayLib.Vector2; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Line unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Line";
   end Image_Draw_Line;

   procedure Image_Draw_Circle
     (Dst : in out RayLib.Image'Class; Center_X : Natural; Center_Y : Natural;
      Radius :        Natural; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Circle unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Circle";
   end Image_Draw_Circle;

   procedure Image_Draw_Circle
     (Dst    : in out RayLib.Image'Class; Center : RayLib.Vector2;
      Radius :        Integer; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Circle unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Circle";
   end Image_Draw_Circle;

   procedure Image_Draw_Rectangle
     (Dst   : in out RayLib.Image'Class; Pos_X : Natural; Pos_Y : Natural;
      Width :        Natural; Height : Natural; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Rectangle unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Rectangle";
   end Image_Draw_Rectangle;

   procedure Image_Draw_Rectangle
     (Dst  : in out RayLib.Image'Class; Position : RayLib.Vector2;
      Size :        RayLib.Vector2; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Rectangle unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Rectangle";
   end Image_Draw_Rectangle;

   procedure Image_Draw_Rectangle
     (Dst   : in out RayLib.Image'Class; Rec : RayLib.Rectangle;
      Color :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Rectangle unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Rectangle";
   end Image_Draw_Rectangle;

   procedure Image_Draw_Rectangle_Lines
     (Dst : in out RayLib.Image'Class; Rec : RayLib.Rectangle; Thick : Natural;
      Color :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Rectangle_Lines unimplemented");
      raise Program_Error
        with "Unimplemented procedure Image_Draw_Rectangle_Lines";
   end Image_Draw_Rectangle_Lines;

   procedure Image_Draw
     (Dst     : in out RayLib.Image'Class; Src : RayLib.Image;
      Src_Rec :        RayLib.Rectangle; Dst_Rec : RayLib.Rectangle;
      Tint    :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Image_Draw unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw";
   end Image_Draw;

   procedure Image_Draw_Text
     (Dst   : in out RayLib.Image'Class; Text : String; Pos_X : Natural;
      Pos_Y :        Natural; Font_Size : Natural; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Text unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Text";
   end Image_Draw_Text;

   procedure Image_Draw_Text
     (Dst : in out RayLib.Image'Class; Font : RayLib.Font'Class; Text : String;
      Position :        RayLib.Vector2; Font_Size : Float; Spacing : Float;
      Tint     :        RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Image_Draw_Text unimplemented");
      raise Program_Error with "Unimplemented procedure Image_Draw_Text";
   end Image_Draw_Text;

   function Load_Texture (File_Name : String) return RayLib.Texture2D is
      File_Name_Copy : chars_ptr        := New_String (File_Name);
      Result         : RayLib.Texture2D :=
        (Ada.Finalization.Controlled with Payload => new Texture_Payload);
   begin
      Result.Payload.all.Data := raylib_h.LoadTexture (File_Name_Copy);
      Free (File_Name_Copy);
      return Result;
   end Load_Texture;

   function Load_Texture_From_Image
     (Image : RayLib.Image'Class) return RayLib.Texture2D
   is
      Result : RayLib.Texture :=
        (Ada.Finalization.Controlled with Payload => new Texture_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadTextureFromImage (Image.Payload.all.Data);
      return Result;
   end Load_Texture_From_Image;

   function Load_Texture_Cubemap
     (Image : RayLib.Image'Class; Layout : Cubemap_Layout)
      return RayLib.Texture_Cubemap
   is
      Result : RayLib.Texture_Cubemap :=
        (Ada.Finalization.Controlled with Payload => new Texture_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadTextureCubemap (Image.Payload.all.Data, int (Layout));
      return Result;
   end Load_Texture_Cubemap;

   function Load_Render_Texture
     (Width : Natural; Height : Natural) return RayLib.Render_Texture2D
   is
      Result : RayLib.Render_Texture2D :=
        (Ada.Finalization.Controlled with
         Payload => new Render_Texture_Payload);
   begin
      Result.Payload.all.Data :=
        raylib_h.LoadRenderTexture (int (Width), int (Height));
      return Result;
   end Load_Render_Texture;

   procedure Update_Texture
     (Texture : RayLib.Texture2D'Class; Pixels : Stream_Element_Array)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Update_Texture unimplemented");
      raise Program_Error with "Unimplemented procedure Update_Texture";
   end Update_Texture;

   procedure Update_Texture
     (Texture : RayLib.Texture2D'Class; Rec : RayLib.Rectangle;
      Pixels  : Stream_Element_Array)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Update_Texture unimplemented");
      raise Program_Error with "Unimplemented procedure Update_Texture";
   end Update_Texture;

   procedure Gen_Texture_Mipmaps (Texture : in out RayLib.Texture2D'Class) is
   begin
      raylib_h.GenTextureMipmaps (Texture.Payload.all.Data'Access);
   end Gen_Texture_Mipmaps;

   procedure Set_Texture_Filter
     (Texture : RayLib.Texture2D'Class; Filter : Texture_Filter)
   is
   begin
      raylib_h.SetTextureFilter (Texture.Payload.all.Data, int (Filter));
   end Set_Texture_Filter;

   procedure Set_Texture_Wrap
     (Texture : RayLib.Texture2D'Class; Wrap : Texture_Wrap)
   is
   begin
      raylib_h.SetTextureWrap (Texture.Payload.all.Data, int (Wrap));
   end Set_Texture_Wrap;

   procedure Draw_Texture
     (Texture : RayLib.Texture2D'Class; Pos_X : Natural; Pos_Y : Natural;
      Tint    : RayLib.Color)
   is
   begin
      raylib_h.DrawTexture
        (Texture.Payload.all.Data, int (Pos_X), int (Pos_Y), +Tint);
   end Draw_Texture;

   procedure Draw_Texture
     (Texture : RayLib.Texture2D'Class; Position : RayLib.Vector2;
      Tint    : RayLib.Color)
   is
   begin
      raylib_h.DrawTextureV (Texture.Payload.all.Data, +Position, +Tint);
   end Draw_Texture;

   procedure Draw_Texture
     (Texture  : RayLib.Texture2D'Class; Position : RayLib.Vector2;
      Rotation : Float; Scale : Float; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawTextureEx
        (Texture.Payload.all.Data, +Position, Rotation, Scale, +Tint);
   end Draw_Texture;

   procedure Draw_Texture
     (Texture  : RayLib.Texture2D'Class; Source : RayLib.Rectangle;
      Position : RayLib.Vector2; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawTextureRec
        (Texture.Payload.all.Data, +Source, +Position, +Tint);
   end Draw_Texture;

   procedure Draw_Texture_Quad
     (Texture : RayLib.Texture2D'Class; Tiling : RayLib.Vector2;
      Offset  : RayLib.Vector2; Quad : RayLib.Rectangle; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawTextureQuad
        (Texture.Payload.all.Data, +Tiling, +Offset, +Quad, +Tint);
   end Draw_Texture_Quad;

   procedure Draw_Texture_Tiled
     (Texture : RayLib.Texture2D'Class; Source : RayLib.Rectangle;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Scale   : Float; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawTextureTiled
        (Texture.Payload.all.Data, +Source, +Dest, +Origin, Rotation, Scale,
         +Tint);
   end Draw_Texture_Tiled;

   procedure Draw_Texture
     (Texture : RayLib.Texture2D'Class; Source : RayLib.Rectangle;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Tint    : RayLib.Color)
   is
   begin
      raylib_h.DrawTexturePro
        (Texture.Payload.all.Data, +Source, +Dest, +Origin, Rotation, +Tint);
   end Draw_Texture;

   procedure Draw_Texture_N_Patch
     (Texture : RayLib.Texture2D'Class; N_Patch_Info : RayLib.N_Patch_Info;
      Dest    : RayLib.Rectangle; Origin : RayLib.Vector2; Rotation : Float;
      Tint    : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Texture_N_Patch unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Texture_N_Patch";
   end Draw_Texture_N_Patch;

   procedure Draw_Texture_Poly
     (Texture : RayLib.Texture2D'Class; Center : RayLib.Vector2;
      Points  : RayLib.Vector2_Array; Texcoords : RayLib.Vector2_Array;
      Tint    : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Texture_Poly unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Texture_Poly";
   end Draw_Texture_Poly;

   function Fade (Color : RayLib.Color; Alpha : Float) return RayLib.Color is
     (+raylib_h.Fade (+Color, Alpha));

   function Color_To_Int (Color : RayLib.Color) return Natural is
     (Natural (raylib_h.ColorToInt (+Color)));

   function Color_Normalize (Color : RayLib.Color) return RayLib.Vector4 is
     (+raylib_h.ColorNormalize (+Color));

   function Color_From_Normalized
     (Normalized : RayLib.Vector4) return RayLib.Color is
     (+raylib_h.ColorFromNormalized (+Normalized));

   function Color_To_HSV (Color : RayLib.Color) return RayLib.Vector3 is
     (+raylib_h.ColorToHSV (+Color));

   function Color_From_HSV
     (Hue : Float; Saturation : Float; Value : Float) return RayLib.Color is
     (+raylib_h.ColorFromHSV (Hue, Saturation, Value));

   function Color_Alpha
     (Color : RayLib.Color; Alpha : Float) return RayLib.Color is
     (+raylib_h.ColorAlpha (+Color, Alpha));

   function Color_Alpha_Blend
     (Dst : RayLib.Color; Src : RayLib.Color; Tint : RayLib.Color)
      return RayLib.Color is
     (+raylib_h.ColorAlphaBlend (+Dst, +Src, +Tint));

   function Get_Color (Hex_Value : Natural) return RayLib.Color is
     (+raylib_h.GetColor (unsigned (Hex_Value)));

   function Get_Pixel_Color
     (Src_Ptr : System.Address; Format : Pixel_Format) return RayLib.Color is
     (+raylib_h.GetPixelColor (Src_Ptr, int (Format)));

   procedure Set_Pixel_Color
     (Dst_Ptr : System.Address; Color : RayLib.Color; Format : Pixel_Format)
   is
   begin
      raylib_h.SetPixelColor (Dst_Ptr, +Color, int (Format));
   end Set_Pixel_Color;

   function Get_Pixel_Data_Size
     (Width : Natural; Height : Natural; Format : Pixel_Format)
      return Natural is
     (Natural
        (raylib_h.GetPixelDataSize (int (Width), int (Height), int (Format))));

   function Get_Font_Default return RayLib.Font'Class is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Font_Default unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Font_Default";
   end Get_Font_Default;

   function Load_Font (File_Name : String) return RayLib.Font'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Load_Font unimplemented");
      return raise Program_Error with "Unimplemented function Load_Font";
   end Load_Font;

   function Load_Font
     (File_Name   : String; Font_Size : Natural; Font_Chars : Wide_Wide_String;
      Glyph_Count : Natural) return RayLib.Font'Class
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Load_Font unimplemented");
      return raise Program_Error with "Unimplemented function Load_Font";
   end Load_Font;

   function Load_Font_From_Image
     (Image      : RayLib.Image'Class; Key : RayLib.Color;
      First_Char : Wide_Wide_Character) return RayLib.Font'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Font_From_Image unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Font_From_Image";
   end Load_Font_From_Image;

   function Load_Font_From_Memory
     (File_Type   : String; File_Data : Stream_Element_Array;
      Font_Size   : Natural; Font_Chars : Wide_Wide_String;
      Glyph_Count : Natural) return RayLib.Font'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Font_From_Memory unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_Font_From_Memory";
   end Load_Font_From_Memory;

   function Load_Font_Data
     (File_Data  : Stream_Element_Array; Font_Size : Natural;
      Font_Chars : Wide_Wide_String; Glyph_Count : Natural;
      Font_Type  : RayLib.Font_Type) return RayLib.Glyph_Info_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Font_Data unimplemented");
      return raise Program_Error with "Unimplemented function Load_Font_Data";
   end Load_Font_Data;

   function Gen_Image_Font_Atlas
     (Chars : Glyph_Info_Array; Recs : Rectangle_Array; Glyph_Count : Natural;
      Font_Size : Natural; Padding : Natural; Pack_Method : Natural)
      return RayLib.Image'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Image_Font_Atlas unimplemented");
      return
        raise Program_Error with "Unimplemented function Gen_Image_Font_Atlas";
   end Gen_Image_Font_Atlas;

   function Export_Font_As_Code
     (Font : RayLib.Font'Class; File_Name : String) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Export_Font_As_Code unimplemented");
      return
        raise Program_Error with "Unimplemented function Export_Font_As_Code";
   end Export_Font_As_Code;

   procedure Draw_FPS (Pos_X : Natural; Pos_Y : Natural) is
   begin
      raylib_h.DrawFPS (int (Pos_X), int (Pos_Y));
   end Draw_FPS;

   procedure Draw_Text
     (Text  : String; Pos_X : Natural; Pos_Y : Natural; Font_Size : Natural;
      Color : RayLib.Color)
   is
      Text_Copy : chars_ptr := New_String (Text);
   begin
      raylib_h.DrawText
        (Text_Copy, int (Pos_X), int (Pos_Y), int (Font_Size), +Color);
      Free (Text_Copy);
   end Draw_Text;

   procedure Draw_Text
     (Font      : RayLib.Font'Class; Text : String; Position : RayLib.Vector2;
      Font_Size : Float; Spacing : Float; Tint : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Draw_Text unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Text";
   end Draw_Text;

   procedure Draw_Text
     (Font    : RayLib.Font'Class; Text : String; Position : RayLib.Vector2;
      Origin  : RayLib.Vector2; Rotation : Float; Font_Size : Float;
      Spacing : Float; Tint : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Draw_Text unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Text";
   end Draw_Text;

   procedure Draw_Text_Codepoint
     (Font     : RayLib.Font'Class; Codepoint : Wide_Wide_Character;
      Position : RayLib.Vector2; Font_Size : Float; Tint : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Text_Codepoint unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Text_Codepoint";
   end Draw_Text_Codepoint;

   procedure Draw_Text_Codepoints
     (Font     : RayLib.Font'Class; Codepoints : Wide_Wide_String;
      Position : RayLib.Vector2; Font_Size : Float; Spacing : Float;
      Tint     : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Text_Codepoints unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Text_Codepoints";
   end Draw_Text_Codepoints;

   function Measure_Text (Text : String; Font_Size : Natural) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Measure_Text unimplemented");
      return raise Program_Error with "Unimplemented function Measure_Text";
   end Measure_Text;

   function Measure_Text
     (Font    : RayLib.Font'Class; Text : String; Font_Size : Float;
      Spacing : Float) return RayLib.Vector2
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Measure_Text unimplemented");
      return raise Program_Error with "Unimplemented function Measure_Text";
   end Measure_Text;

   function Get_Glyph_Index
     (Font : RayLib.Font'Class; Codepoint : Wide_Wide_Character) return Natural
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Glyph_Index unimplemented");
      return raise Program_Error with "Unimplemented function Get_Glyph_Index";
   end Get_Glyph_Index;

   function Get_Glyph_Info
     (Font : RayLib.Font'Class; Codepoint : Wide_Wide_Character)
      return RayLib.Glyph_Info
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Glyph_Info unimplemented");
      return raise Program_Error with "Unimplemented function Get_Glyph_Info";
   end Get_Glyph_Info;

   function Get_Glyph_Atlas
     (Font : RayLib.Font'Class; Codepoint : Wide_Wide_Character)
      return RayLib.Rectangle
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Glyph_Atlas unimplemented");
      return raise Program_Error with "Unimplemented function Get_Glyph_Atlas";
   end Get_Glyph_Atlas;

   function Load_Codepoints (Text : String) return Wide_Wide_String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Codepoints unimplemented");
      return raise Program_Error with "Unimplemented function Load_Codepoints";
   end Load_Codepoints;

   function Get_Codepoint_Count (Text : String) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Codepoint_Count unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Codepoint_Count";
   end Get_Codepoint_Count;

   function Get_Codepoint
     (Text : String; Bytes_Processed : out Natural) return Wide_Wide_Character
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Codepoint unimplemented");
      return raise Program_Error with "Unimplemented function Get_Codepoint";
   end Get_Codepoint;

   function Codepoint_To_UTF8 (Codepoint : Wide_Wide_Character) return String
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Codepoint_To_UTF8 unimplemented");
      return
        raise Program_Error with "Unimplemented function Codepoint_To_UTF8";
   end Codepoint_To_UTF8;

   function Text_Codepoints_To_UTF8
     (Codepoints : Wide_Wide_String) return String
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_Codepoints_To_UTF8 unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Text_Codepoints_To_UTF8";
   end Text_Codepoints_To_UTF8;

   procedure Text_Copy (Dst : out String; Src : String) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Text_Copy unimplemented");
      raise Program_Error with "Unimplemented procedure Text_Copy";
   end Text_Copy;

   function Text_Is_Equal (Text1 : String; Text2 : String) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_Is_Equal unimplemented");
      return raise Program_Error with "Unimplemented function Text_Is_Equal";
   end Text_Is_Equal;

   function Text_Length (Text : String) return Natural is
   begin
      pragma Compile_Time_Warning (Standard.True, "Text_Length unimplemented");
      return raise Program_Error with "Unimplemented function Text_Length";
   end Text_Length;

   function Text_Subtext
     (Text : String; Position : Positive; Length : Natural) return String
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_Subtext unimplemented");
      return raise Program_Error with "Unimplemented function Text_Subtext";
   end Text_Subtext;

   function Text_Replace
     (Text : String; Replace : String; By : String) return String
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_Replace unimplemented");
      return raise Program_Error with "Unimplemented function Text_Replace";
   end Text_Replace;

   function Text_Insert
     (Text : String; Insert : String; Position : Positive) return String
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Text_Insert unimplemented");
      return raise Program_Error with "Unimplemented function Text_Insert";
   end Text_Insert;

   function Text_Join
     (Text_List : Unbounded_String_Array; Delimiter : String) return String
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Text_Join unimplemented");
      return raise Program_Error with "Unimplemented function Text_Join";
   end Text_Join;

   function Text_Split
     (Text : String; Delimiter : Character) return Unbounded_String_Array
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Text_Split unimplemented");
      return raise Program_Error with "Unimplemented function Text_Split";
   end Text_Split;

   procedure Text_Append
     (Text : Unbounded_String; Append : String; Position : out Positive)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Text_Append unimplemented");
      raise Program_Error with "Unimplemented procedure Text_Append";
   end Text_Append;

   function Text_Find_Index (Text : String; Find : String) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_Find_Index unimplemented");
      return raise Program_Error with "Unimplemented function Text_Find_Index";
   end Text_Find_Index;

   function Text_To_Upper (Text : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_To_Upper unimplemented");
      return raise Program_Error with "Unimplemented function Text_To_Upper";
   end Text_To_Upper;

   function Text_To_Lower (Text : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_To_Lower unimplemented");
      return raise Program_Error with "Unimplemented function Text_To_Lower";
   end Text_To_Lower;

   function Text_To_Pascal (Text : String) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_To_Pascal unimplemented");
      return raise Program_Error with "Unimplemented function Text_To_Pascal";
   end Text_To_Pascal;

   function Text_To_Integer (Text : String) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Text_To_Integer unimplemented");
      return raise Program_Error with "Unimplemented function Text_To_Integer";
   end Text_To_Integer;

   procedure Draw_Line3D
     (Start_Pos : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Color     : RayLib.Color)
   is
   begin
      raylib_h.DrawLine3D (+Start_Pos, +End_Pos, +Color);
   end Draw_Line3D;

   procedure Draw_Point3D (Position : RayLib.Vector3; Color : RayLib.Color) is
   begin
      raylib_h.DrawPoint3D (+Position, +Color);
   end Draw_Point3D;

   procedure Draw_Circle3D
     (Center : RayLib.Vector3; Radius : Float; Rotation_Axis : RayLib.Vector3;
      Rotation_Angle : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCircle3D
        (+Center, Radius, +Rotation_Axis, Rotation_Angle, +Color);
   end Draw_Circle3D;

   procedure Draw_Triangle3D
     (V1    : RayLib.Vector3; V2 : RayLib.Vector3; V3 : RayLib.Vector3;
      Color : RayLib.Color)
   is
   begin
      raylib_h.DrawTriangle3D (+V1, +V2, +V3, +Color);
   end Draw_Triangle3D;

   procedure Draw_Triangle_Strip3D
     (Points : Vector3_Array; Color : RayLib.Color)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Triangle_Strip3D unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Triangle_Strip3D";
   end Draw_Triangle_Strip3D;

   procedure Draw_Cube
     (Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color)
   is
   begin
      raylib_h.DrawCube (+Position, Width, Height, Length, +Color);
   end Draw_Cube;

   procedure Draw_Cube
     (Position : RayLib.Vector3; Size : RayLib.Vector3; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCubeV (+Position, +Size, +Color);
   end Draw_Cube;

   procedure Draw_Cube_Wires
     (Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color)
   is
   begin
      raylib_h.DrawCubeWires (+Position, Width, Height, Length, +Color);
   end Draw_Cube_Wires;

   procedure Draw_Cube_Wires
     (Position : RayLib.Vector3; Size : RayLib.Vector3; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCubeWiresV (+Position, +Size, +Color);
   end Draw_Cube_Wires;

   procedure Draw_Cube_Texture
     (Texture : RayLib.Texture2D'Class; Position : RayLib.Vector3;
      Width   : Float; Height : Float; Length : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCubeTexture
        (Texture.Payload.all.Data, +Position, Width, Height, Length, +Color);
   end Draw_Cube_Texture;

   procedure Draw_Cube_Texture
     (Texture  : RayLib.Texture2D'Class; Source : RayLib.Rectangle;
      Position : RayLib.Vector3; Width : Float; Height : Float; Length : Float;
      Color    : RayLib.Color)
   is
   begin
      raylib_h.DrawCubeTextureRec
        (Texture.Payload.all.Data, +Source, +Position, Width, Height, Length,
         +Color);
   end Draw_Cube_Texture;

   procedure Draw_Sphere
     (Center_Pos : RayLib.Vector3; Radius : Float; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawSphere (+Center_Pos, Radius, +Color);
   end Draw_Sphere;

   procedure Draw_Sphere
     (Center_Pos : RayLib.Vector3; Radius : Float; Rings : Natural;
      Slices     : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawSphereEx
        (+Center_Pos, Radius, int (Rings), int (Slices), +Color);
   end Draw_Sphere;

   procedure Draw_Sphere_Wires
     (Center_Pos : RayLib.Vector3; Radius : Float; Rings : Natural;
      Slices     : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawSphereWires
        (+Center_Pos, Radius, int (Rings), int (Slices), +Color);
   end Draw_Sphere_Wires;

   procedure Draw_Cylinder
     (Position : RayLib.Vector3; Radius_Top : Float; Radius_Bottom : Float;
      Height   : Float; Slices : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCylinder
        (+Position, Radius_Top, Radius_Bottom, Height, int (Slices), +Color);
   end Draw_Cylinder;

   procedure Draw_Cylinder
     (Start_Pos    : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Start_Radius : Float; End_Radius : Float; Sides : Natural;
      Color        : RayLib.Color)
   is
   begin
      raylib_h.DrawCylinderEx
        (+Start_Pos, +End_Pos, Start_Radius, End_Radius, int (Sides), +Color);
   end Draw_Cylinder;

   procedure Draw_Cylinder_Wires
     (Position : RayLib.Vector3; Radius_Top : Float; Radius_Bottom : Float;
      Height   : Float; Slices : Natural; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawCylinderWires
        (+Position, Radius_Top, Radius_Bottom, Height, int (Slices), +Color);
   end Draw_Cylinder_Wires;

   procedure Draw_Cylinder_Wires
     (Start_Pos    : RayLib.Vector3; End_Pos : RayLib.Vector3;
      Start_Radius : Float; End_Radius : Float; Sides : Natural;
      Color        : RayLib.Color)
   is
   begin
      raylib_h.DrawCylinderWiresEx
        (+Start_Pos, +End_Pos, Start_Radius, End_Radius, int (Sides), +Color);
   end Draw_Cylinder_Wires;

   procedure Draw_Plane
     (Center_Pos : RayLib.Vector3; Size : RayLib.Vector2; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawPlane (+Center_Pos, +Size, +Color);
   end Draw_Plane;

   procedure Draw_Ray (Ray : RayLib.Ray; Color : RayLib.Color) is
   begin
      raylib_h.DrawRay (+Ray, +Color);
   end Draw_Ray;

   procedure Draw_Grid (Slices : Natural; Spacing : Float) is
   begin
      raylib_h.DrawGrid (int (Slices), Spacing);
   end Draw_Grid;

   function Load_Model (File_Name : String) return RayLib.Model is
      File_Name_Copy : chars_ptr    := New_String (File_Name);
      Result         : RayLib.Model :=
        (Ada.Finalization.Controlled with Payload => new Model_Payload);
   begin
      Result.Payload.all.Data := raylib_h.LoadModel (File_Name_Copy);
      Free (File_Name_Copy);
      return Result;
   end Load_Model;

   function Load_Model_From_Mesh (Mesh : RayLib.Mesh'Class) return RayLib.Model
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Model_From_Mesh unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Model_From_Mesh";
   end Load_Model_From_Mesh;

   function Get_Model_Bounding_Box
     (Model : RayLib.Model'Class) return RayLib.Bounding_Box is
     (+raylib_h.GetModelBoundingBox (Model.Payload.all.Data));

   procedure Draw_Model
     (Model : RayLib.Model'Class; Position : RayLib.Vector3; Scale : Float;
      Tint  : RayLib.Color)
   is
   begin
      raylib_h.DrawModel (Model.Payload.all.Data, +Position, Scale, +Tint);
   end Draw_Model;

   procedure Draw_Model
     (Model         : RayLib.Model'Class; Position : RayLib.Vector3;
      Rotation_Axis : RayLib.Vector3; Rotation_Angle : Float;
      Scale         : RayLib.Vector3; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawModelEx
        (Model.Payload.all.Data, +Position, +Rotation_Axis, Rotation_Angle,
         +Scale, +Tint);
   end Draw_Model;

   procedure Draw_Model_Wires
     (Model : RayLib.Model'Class; Position : RayLib.Vector3; Scale : Float;
      Tint  : RayLib.Color)
   is
   begin
      raylib_h.DrawModelWires
        (Model.Payload.all.Data, +Position, Scale, +Tint);
   end Draw_Model_Wires;

   procedure Draw_Model_Wires
     (Model         : RayLib.Model'Class; Position : RayLib.Vector3;
      Rotation_Axis : RayLib.Vector3; Rotation_Angle : Float;
      Scale         : RayLib.Vector3; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawModelWiresEx
        (Model.Payload.all.Data, +Position, +Rotation_Axis, Rotation_Angle,
         +Scale, +Tint);
   end Draw_Model_Wires;

   procedure Draw_Bounding_Box
     (Box : RayLib.Bounding_Box; Color : RayLib.Color)
   is
   begin
      raylib_h.DrawBoundingBox (+Box, +Color);
   end Draw_Bounding_Box;

   procedure Draw_Billboard
     (Camera   : RayLib.Camera; Texture : RayLib.Texture2D'Class;
      Position : RayLib.Vector3; Size : Float; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawBillboard
        (+Camera, Texture.Payload.all.Data, +Position, Size, +Tint);
   end Draw_Billboard;

   procedure Draw_Billboard
     (Camera : RayLib.Camera; Texture : RayLib.Texture2D'Class;
      Source : RayLib.Rectangle; Position : RayLib.Vector3;
      Size   : RayLib.Vector2; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawBillboardRec
        (+Camera, Texture.Payload.all.Data, +Source, +Position, +Size, +Tint);
   end Draw_Billboard;

   procedure Draw_Billboard
     (Camera   : RayLib.Camera; Texture : RayLib.Texture2D'Class;
      Source   : RayLib.Rectangle; Position : RayLib.Vector3;
      Up : RayLib.Vector3; Size : RayLib.Vector2; Origin : RayLib.Vector2;
      Rotation : Float; Tint : RayLib.Color)
   is
   begin
      raylib_h.DrawBillboardPro
        (+Camera, Texture.Payload.all.Data, +Source, +Position, +Up, +Size,
         +Origin, Rotation, +Tint);
   end Draw_Billboard;

   procedure Upload_Mesh (Mesh : in out RayLib.Mesh'Class; Dynamic : Boolean)
   is
   begin
      raylib_h.UploadMesh (Mesh.Payload.all.Data'Access, bool (Dynamic));
   end Upload_Mesh;

   procedure Update_Mesh_Buffer
     (Mesh : RayLib.Mesh'Class; Index : Integer; Data : Stream_Element_Array)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Update_Mesh_Buffer unimplemented");
      raise Program_Error with "Unimplemented procedure Update_Mesh_Buffer";
   end Update_Mesh_Buffer;

   procedure Draw_Mesh
     (Mesh      : RayLib.Mesh'Class; Material : RayLib.Material'Class;
      Transform : RayLib.Matrix)
   is
   begin
      raylib_h.DrawMesh
        (Mesh.Payload.all.Data, Material.Payload.all.Data, +Transform);
   end Draw_Mesh;

   procedure Draw_Mesh_Instanced
     (Mesh       : RayLib.Mesh'Class; Material : RayLib.Material'Class;
      Transforms : Transform_Array; Instances : Natural)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Draw_Mesh_Instanced unimplemented");
      raise Program_Error with "Unimplemented procedure Draw_Mesh_Instanced";
   end Draw_Mesh_Instanced;

   function Export_Mesh
     (Mesh : RayLib.Mesh'Class; File_Name : String) return Boolean
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Export_Mesh unimplemented");
      return raise Program_Error with "Unimplemented function Export_Mesh";
   end Export_Mesh;

   function Get_Mesh_Bounding_Box
     (Mesh : RayLib.Mesh'Class) return RayLib.Bounding_Box is
     (+raylib_h.GetMeshBoundingBox (Mesh.Payload.all.Data));

   procedure Gen_Mesh_Tangents (Mesh : in out RayLib.Mesh'Class) is
   begin
      raylib_h.GenMeshTangents (Mesh.Payload.all.Data'Access);
   end Gen_Mesh_Tangents;

   procedure Gen_Mesh_Binormals (Mesh : in out RayLib.Mesh'Class) is
   begin
      raylib_h.GenMeshBinormals (Mesh.Payload.all.Data'Access);
   end Gen_Mesh_Binormals;

   function Gen_Mesh_Poly (Sides : Natural; Radius : Float) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Poly unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Poly";
   end Gen_Mesh_Poly;

   function Gen_Mesh_Plane
     (Width : Float; Length : Float; Res_X : Integer; Res_Z : Natural)
      return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Plane unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Plane";
   end Gen_Mesh_Plane;

   function Gen_Mesh_Cube
     (Width : Float; Height : Float; Length : Float) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Cube unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Cube";
   end Gen_Mesh_Cube;

   function Gen_Mesh_Sphere
     (Radius : Float; Rings : Natural; Slices : Natural) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Sphere unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Sphere";
   end Gen_Mesh_Sphere;

   function Gen_Mesh_Hemi_Sphere
     (Radius : Float; Rings : Natural; Slices : Natural) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Hemi_Sphere unimplemented");
      return
        raise Program_Error with "Unimplemented function Gen_Mesh_Hemi_Sphere";
   end Gen_Mesh_Hemi_Sphere;

   function Gen_Mesh_Cylinder
     (Radius : Float; Height : Float; Slices : Natural) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Cylinder unimplemented");
      return
        raise Program_Error with "Unimplemented function Gen_Mesh_Cylinder";
   end Gen_Mesh_Cylinder;

   function Gen_Mesh_Cone
     (Radius : Float; Height : Float; Slices : Natural) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Cone unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Cone";
   end Gen_Mesh_Cone;

   function Gen_Mesh_Torus
     (Radius : Float; Size : Float; Rad_Seg : Natural; Sides : Natural)
      return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Torus unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Torus";
   end Gen_Mesh_Torus;

   function Gen_Mesh_Knot
     (Radius : Float; Size : Float; Rad_Seg : Natural; Sides : Natural)
      return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Knot unimplemented");
      return raise Program_Error with "Unimplemented function Gen_Mesh_Knot";
   end Gen_Mesh_Knot;

   function Gen_Mesh_Heightmap
     (Heightmap : RayLib.Image'Class; Size : RayLib.Vector3) return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Heightmap unimplemented");
      return
        raise Program_Error with "Unimplemented function Gen_Mesh_Heightmap";
   end Gen_Mesh_Heightmap;

   function Gen_Mesh_Cubicmap
     (Cubicmap : RayLib.Image'Class; Cube_Size : RayLib.Vector3)
      return RayLib.Mesh
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Gen_Mesh_Cubicmap unimplemented");
      return
        raise Program_Error with "Unimplemented function Gen_Mesh_Cubicmap";
   end Gen_Mesh_Cubicmap;

   function Load_Materials (File_Name : String) return RayLib.Material_Array is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Materials unimplemented");
      return raise Program_Error with "Unimplemented function Load_Materials";
   end Load_Materials;

   function Load_Material_Default return RayLib.Material is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Material_Default unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_Material_Default";
   end Load_Material_Default;

   procedure Set_Material_Texture
     (Material : in out RayLib.Material'Class; Map_Type : Material_Map_Index;
      Texture  :        RayLib.Texture2D'Class)
   is
   begin
      raylib_h.SetMaterialTexture
        (Material.Payload.all.Data'Access, int (Map_Type),
         Texture.Payload.all.Data);
   end Set_Material_Texture;

   procedure Set_Model_Mesh_Material
     (Model       : in out RayLib.Model'Class; Mesh_Id : Natural;
      Material_Id :        Natural)
   is
   begin
      raylib_h.SetModelMeshMaterial
        (Model.Payload.all.Data'Access, int (Mesh_Id), int (Material_Id));
   end Set_Model_Mesh_Material;

   function Load_Model_Animations
     (File_Name : String) return RayLib.Model_Animation_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Model_Animations unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_Model_Animations";
   end Load_Model_Animations;

   procedure Update_Model_Animation
     (Model : RayLib.Model'Class; Anim : RayLib.Model_Animation'Class;
      Frame : Natural)
   is
   begin
      raylib_h.UpdateModelAnimation
        (Model.Payload.all.Data, Anim.Payload.all.Data, int (Frame));
   end Update_Model_Animation;

   function Is_Model_Animation_Valid
     (Model : RayLib.Model'Class; Anim : RayLib.Model_Animation'Class)
      return Boolean is
     (Boolean
        (raylib_h.IsModelAnimationValid
           (Model.Payload.all.Data, Anim.Payload.all.Data)));

   function Check_Collision_Spheres
     (Center1 : RayLib.Vector3; Radius1 : Float; Center2 : RayLib.Vector3;
      Radius2 : Float) return Boolean is
     (Boolean
        (raylib_h.CheckCollisionSpheres
           (+Center1, Radius1, +Center2, Radius2)));

   function Check_Collision_Boxes
     (Box1 : RayLib.Bounding_Box; Box2 : RayLib.Bounding_Box) return Boolean is
     (Boolean (raylib_h.CheckCollisionBoxes (+Box1, +Box2)));

   function Get_Ray_Collision_Sphere
     (Ray : RayLib.Ray; Center : RayLib.Vector3; Radius : Float)
      return RayLib.Ray_Collision is
     (+raylib_h.GetRayCollisionSphere (+Ray, +Center, Radius));

   function Get_Ray_Collision_Box
     (Ray : RayLib.Ray; Box : RayLib.Bounding_Box)
      return RayLib.Ray_Collision is
     (+raylib_h.GetRayCollisionBox (+Ray, +Box));

   function Get_Ray_Collision_Mesh
     (Ray : RayLib.Ray; Mesh : RayLib.Mesh'Class; Transform : RayLib.Matrix)
      return RayLib.Ray_Collision is
     (+raylib_h.GetRayCollisionMesh (+Ray, Mesh.Payload.all.Data, +Transform));

   function Get_Ray_Collision_Triangle
     (Ray : RayLib.Ray; P1 : RayLib.Vector3; P2 : RayLib.Vector3;
      P3  : RayLib.Vector3) return RayLib.Ray_Collision is
     (+raylib_h.GetRayCollisionTriangle (+Ray, +P1, +P2, +P3));

   function Get_Ray_Collision_Quad
     (Ray : RayLib.Ray; P1 : RayLib.Vector3; P2 : RayLib.Vector3;
      P3  : RayLib.Vector3; P4 : RayLib.Vector3) return RayLib.Ray_Collision is
     (+raylib_h.GetRayCollisionQuad (+Ray, +P1, +P2, +P3, +P4));

   procedure Init_Audio_Device is
   begin
      raylib_h.InitAudioDevice;
   end Init_Audio_Device;

   procedure Close_Audio_Device is
   begin
      raylib_h.CloseAudioDevice;
   end Close_Audio_Device;

   function Is_Audio_Device_Ready return Boolean is
     (Boolean (raylib_h.IsAudioDeviceReady));

   procedure Set_Master_Volume (Volume : Float) is
   begin
      raylib_h.SetMasterVolume (Volume);
   end Set_Master_Volume;

   function Load_Wave (File_Name : String) return RayLib.Wave'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Load_Wave unimplemented");
      return raise Program_Error with "Unimplemented function Load_Wave";
   end Load_Wave;

   function Load_Wave_From_Memory
     (File_Type : String; File_Data : Stream_Element_Array)
      return RayLib.Wave'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Wave_From_Memory unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_Wave_From_Memory";
   end Load_Wave_From_Memory;

   function Load_Sound (File_Name : String) return RayLib.Sound'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Load_Sound unimplemented");
      return raise Program_Error with "Unimplemented function Load_Sound";
   end Load_Sound;

   function Load_Sound_From_Wave
     (Wave : RayLib.Wave'Class) return RayLib.Sound'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Sound_From_Wave unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Sound_From_Wave";
   end Load_Sound_From_Wave;

   procedure Update_Sound
     (Sound        : RayLib.Sound'Class; Data : Stream_Element_Array;
      Sample_Count : Natural)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Update_Sound unimplemented");
      raise Program_Error with "Unimplemented procedure Update_Sound";
   end Update_Sound;

   function Export_Wave
     (Wave : RayLib.Wave'Class; File_Name : String) return Boolean
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Export_Wave unimplemented");
      return raise Program_Error with "Unimplemented function Export_Wave";
   end Export_Wave;

   function Export_Wave_As_Code
     (Wave : RayLib.Wave'Class; File_Name : String) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Export_Wave_As_Code unimplemented");
      return
        raise Program_Error with "Unimplemented function Export_Wave_As_Code";
   end Export_Wave_As_Code;

   procedure Play_Sound (Sound : RayLib.Sound'Class) is
   begin
      raylib_h.PlaySound (Sound.Payload.all.Data);
   end Play_Sound;

   procedure Stop_Sound (Sound : RayLib.Sound'Class) is
   begin
      raylib_h.StopSound (Sound.Payload.all.Data);
   end Stop_Sound;

   procedure Pause_Sound (Sound : RayLib.Sound'Class) is
   begin
      raylib_h.PauseSound (Sound.Payload.all.Data);
   end Pause_Sound;

   procedure Resume_Sound (Sound : RayLib.Sound'Class) is
   begin
      raylib_h.ResumeSound (Sound.Payload.all.Data);
   end Resume_Sound;

   procedure Play_Sound_Multi (Sound : RayLib.Sound'Class) is
   begin
      raylib_h.PlaySoundMulti (Sound.Payload.all.Data);
   end Play_Sound_Multi;

   procedure Stop_Sound_Multi is
   begin
      raylib_h.StopSoundMulti;
   end Stop_Sound_Multi;

   function Get_Sounds_Playing return Natural is
     (Natural (raylib_h.GetSoundsPlaying));

   function Is_Sound_Playing (Sound : RayLib.Sound'Class) return Boolean is
     (Boolean (raylib_h.IsSoundPlaying (Sound.Payload.all.Data)));

   procedure Set_Sound_Volume (Sound : RayLib.Sound'Class; Volume : Float) is
   begin
      raylib_h.SetSoundVolume (Sound.Payload.all.Data, Volume);
   end Set_Sound_Volume;

   procedure Set_Sound_Pitch (Sound : RayLib.Sound'Class; Pitch : Float) is
   begin
      raylib_h.SetSoundPitch (Sound.Payload.all.Data, Pitch);
   end Set_Sound_Pitch;

   function Wave_Copy (Wave : RayLib.Wave'Class) return RayLib.Wave'Class is
   begin
      pragma Compile_Time_Warning (Standard.True, "Wave_Copy unimplemented");
      return raise Program_Error with "Unimplemented function Wave_Copy";
   end Wave_Copy;

   procedure Wave_Crop
     (Wave         : in out RayLib.Wave'Class; Init_Sample : Natural;
      Final_Sample :        Natural)
   is
   begin
      raylib_h.WaveCrop
        (Wave.Payload.all.Data'Access, int (Init_Sample), int (Final_Sample));
   end Wave_Crop;

   procedure Wave_Format
     (Wave        : in out RayLib.Wave'Class; Sample_Rate : Natural;
      Sample_Size :        Natural; Channels : Natural)
   is
   begin
      raylib_h.WaveFormat
        (Wave.Payload.all.Data'Access, int (Sample_Rate), int (Sample_Size),
         int (Channels));
   end Wave_Format;

   function Load_Wave_Samples
     (Wave : RayLib.Wave'Class) return RayLib.Float_Array
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Wave_Samples unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Wave_Samples";
   end Load_Wave_Samples;

   function Load_Music_Stream (File_Name : String) return RayLib.Music'Class is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Music_Stream unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Music_Stream";
   end Load_Music_Stream;

   function Load_Music_Stream_From_Memory
     (File_Type : String; Data : Stream_Element_Array)
      return RayLib.Music'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Music_Stream_From_Memory unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Load_Music_Stream_From_Memory";
   end Load_Music_Stream_From_Memory;

   procedure Play_Music_Stream (Music : RayLib.Music'Class) is
   begin
      raylib_h.PlayMusicStream (Music.Payload.all.Data);
   end Play_Music_Stream;

   function Is_Music_Stream_Playing
     (Music : RayLib.Music'Class) return Boolean is
     (Boolean (raylib_h.IsMusicStreamPlaying (Music.Payload.all.Data)));

   procedure Update_Music_Stream (Music : RayLib.Music'Class) is
   begin
      raylib_h.UpdateMusicStream (Music.Payload.all.Data);
   end Update_Music_Stream;

   procedure Stop_Music_Stream (Music : RayLib.Music'Class) is
   begin
      raylib_h.StopMusicStream (Music.Payload.all.Data);
   end Stop_Music_Stream;

   procedure Pause_Music_Stream (Music : RayLib.Music'Class) is
   begin
      raylib_h.PauseMusicStream (Music.Payload.all.Data);
   end Pause_Music_Stream;

   procedure Resume_Music_Stream (Music : RayLib.Music'Class) is
   begin
      raylib_h.ResumeMusicStream (Music.Payload.all.Data);
   end Resume_Music_Stream;

   procedure Seek_Music_Stream (Music : RayLib.Music'Class; Position : Float)
   is
   begin
      raylib_h.SeekMusicStream (Music.Payload.all.Data, Position);
   end Seek_Music_Stream;

   procedure Set_Music_Volume (Music : RayLib.Music'Class; Volume : Float) is
   begin
      raylib_h.SetMusicVolume (Music.Payload.all.Data, Volume);
   end Set_Music_Volume;

   procedure Set_Music_Pitch (Music : RayLib.Music'Class; Pitch : Float) is
   begin
      raylib_h.SetMusicPitch (Music.Payload.all.Data, Pitch);
   end Set_Music_Pitch;

   function Get_Music_Time_Length (Music : RayLib.Music'Class) return Float is
     (raylib_h.GetMusicTimeLength (Music.Payload.all.Data));

   function Get_Music_Time_Played (Music : RayLib.Music'Class) return Float is
     (raylib_h.GetMusicTimePlayed (Music.Payload.all.Data));

   function Load_Audio_Stream
     (Sample_Rate : Natural; Sample_Size : Natural; Channels : Natural)
      return RayLib.Audio_Stream'Class
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Audio_Stream unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Audio_Stream";
   end Load_Audio_Stream;

   procedure Update_Audio_Stream
     (Stream      : RayLib.Audio_Stream'Class; Data : Stream_Element_Array;
      Frame_Count : Natural)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Update_Audio_Stream unimplemented");
      raise Program_Error with "Unimplemented procedure Update_Audio_Stream";
   end Update_Audio_Stream;

   function Is_Audio_Stream_Processed
     (Stream : RayLib.Audio_Stream'Class) return Boolean is
     (Boolean (raylib_h.IsAudioStreamProcessed (Stream.Payload.all.Data)));

   procedure Play_Audio_Stream (Stream : RayLib.Audio_Stream'Class) is
   begin
      raylib_h.PlayAudioStream (Stream.Payload.all.Data);
   end Play_Audio_Stream;

   procedure Pause_Audio_Stream (Stream : RayLib.Audio_Stream'Class) is
   begin
      raylib_h.PauseAudioStream (Stream.Payload.all.Data);
   end Pause_Audio_Stream;

   procedure Resume_Audio_Stream (Stream : RayLib.Audio_Stream'Class) is
   begin
      raylib_h.ResumeAudioStream (Stream.Payload.all.Data);
   end Resume_Audio_Stream;

   function Is_Audio_Stream_Playing
     (Stream : RayLib.Audio_Stream'Class) return Boolean is
     (Boolean (raylib_h.IsAudioStreamPlaying (Stream.Payload.all.Data)));

   procedure Stop_Audio_Stream (Stream : RayLib.Audio_Stream'Class) is
   begin
      raylib_h.StopAudioStream (Stream.Payload.all.Data);
   end Stop_Audio_Stream;

   procedure Set_Audio_Stream_Volume
     (Stream : RayLib.Audio_Stream'Class; Volume : Float)
   is
   begin
      raylib_h.SetAudioStreamVolume (Stream.Payload.all.Data, Volume);
   end Set_Audio_Stream_Volume;

   procedure Set_Audio_Stream_Pitch
     (Stream : RayLib.Audio_Stream'Class; Pitch : Float)
   is
   begin
      raylib_h.SetAudioStreamPitch (Stream.Payload.all.Data, Pitch);
   end Set_Audio_Stream_Pitch;

   procedure Set_Audio_Stream_Buffer_Size_Default (Size : Natural) is
   begin
      raylib_h.SetAudioStreamBufferSizeDefault (int (Size));
   end Set_Audio_Stream_Buffer_Size_Default;

   use System.Atomic_Counters;

   overriding procedure Adjust (Self : in out Image) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Image) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Image_Payload, Image_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the image from " &
                  System.Address_Image (Self.Payload.all.Data.data));
               raylib_h.UnloadImage (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Texture) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Texture) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Texture_Payload, Texture_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the texture id " &
                  Self.Payload.all.Data.id'Image);
               raylib_h.UnloadTexture (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Render_Texture) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Render_Texture) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Render_Texture_Payload, Render_Texture_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the render texture id " &
                  Self.Payload.all.Data.id'Image);
               raylib_h.UnloadRenderTexture (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Font) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Font) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Font_Payload, Font_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the font");
               raylib_h.UnloadFont (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Shader) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Shader) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Shader_Payload, Shader_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the shader id " &
                  Self.Payload.all.Data.id'Image);
               raylib_h.UnloadShader (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Material) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Material) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Material_Payload, Material_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the material");
               raylib_h.UnloadMaterial (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Model) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Model) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Model_Payload, Model_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the model");
               raylib_h.UnloadModel (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Model_Animation) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Model_Animation) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Model_Animation_Payload, Model_Animation_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the model animation");
               raylib_h.UnloadModelAnimation (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Wave) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Wave) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Wave_Payload, Wave_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the wave");
               raylib_h.UnloadWave (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Audio_Stream) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Audio_Stream) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Audio_Stream_Payload, Audio_Stream_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the audio stream");
               raylib_h.UnloadAudioStream (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Sound) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Sound) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Sound_Payload, Sound_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the sound");
               raylib_h.UnloadSound (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   overriding procedure Adjust (Self : in out Music) is
   begin
      if Self.Payload /= null then
         Increment (Self.Payload.all.Counter);
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Music) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Music_Payload, Music_Payload_Access);
   begin
      if Self.Payload /= null then
         if Decrement (Self.Payload.all.Counter) then
            if not Self.Payload.all.Parent_Unloads then
               Trace_Log
                 (Log_Debug,
                  "ADA: Reference count reached 0, unloading the music");
               raylib_h.UnloadMusicStream (Self.Payload.all.Data);
            end if;
            Free (Self.Payload);
            Self.Payload := null;
         end if;
      end if;
   end Finalize;

   function "+" (V : RayLib.Vector2) return raylib_h.Vector2 is
     (x => V.X, y => V.Y);
   function "+" (V : raylib_h.Vector2) return RayLib.Vector2 is
     (X => V.x, Y => V.y);
   function "+" (V : RayLib.Vector3) return raylib_h.Vector3 is
     (x => V.X, y => V.Y, z => V.Z);
   function "+" (V : raylib_h.Vector3) return RayLib.Vector3 is
     (X => V.x, Y => V.y, Z => V.z);
   function "+" (V : RayLib.Vector4) return raylib_h.Vector4 is
     (x => V.X, y => V.Y, z => V.Z, w => V.W);
   function "+" (V : raylib_h.Vector4) return RayLib.Vector4 is
     (X => V.x, Y => V.y, Z => V.z, W => V.w);
   function "+" (V : RayLib.Matrix) return raylib_h.Matrix is
     (m0  => V.M0, m1 => V.M1, m2 => V.M2, m3 => V.M3, m4 => V.M4, m5 => V.M5,
      m6  => V.M6, m7 => V.M7, m8 => V.M8, m9 => V.M9, m10 => V.M10,
      m11 => V.M11, m12 => V.M12, m13 => V.M13, m14 => V.M14, m15 => V.M15);
   function "+" (V : raylib_h.Matrix) return RayLib.Matrix is
     (M0  => V.m0, M1 => V.m1, M2 => V.m2, M3 => V.m3, M4 => V.m4, M5 => V.m5,
      M6  => V.m6, M7 => V.m7, M8 => V.m8, M9 => V.m9, M10 => V.m10,
      M11 => V.m11, M12 => V.m12, M13 => V.m13, M14 => V.m14, M15 => V.m15);
   function "+" (v : RayLib.Color) return raylib_h.Color is
     (r => unsigned_char (v.R), g => unsigned_char (v.G),
      b => unsigned_char (v.B), a => unsigned_char (v.A));
   function "+" (v : raylib_h.Color) return RayLib.Color is
     (R => Color_Component (v.r), G => Color_Component (v.g),
      B => Color_Component (v.b), A => Color_Component (v.a));
   function "+" (V : RayLib.Rectangle) return raylib_h.Rectangle is
     (x => V.X, y => V.Y, width => V.Width, height => V.Height);
   function "+" (V : raylib_h.Rectangle) return RayLib.Rectangle is
     (X => V.x, Y => V.y, Width => V.width, Height => V.height);
   function "+" (V : RayLib.Camera3D) return raylib_h.Camera3D is
     (position => +V.Position, target => +V.Target, up => +V.Up,
      fovy     => V.Fov_Y, projection => int (V.Projection));
   function "+" (V : raylib_h.Camera3D) return RayLib.Camera3D is
     (Position => +V.position, Target => +V.target, Up => +V.up,
      Fov_Y    => V.fovy, Projection => Camera_Projection (V.projection));
   function "+" (V : RayLib.Camera2D) return raylib_h.Camera2D is
     (offset => +V.Offset, target => +V.Target, rotation => V.Rotation,
      zoom   => +V.Zoom);
   function "+" (V : raylib_h.Camera2D) return RayLib.Camera2D is
     (Offset => +V.offset, Target => +V.target, Rotation => V.rotation,
      Zoom   => +V.zoom);
   function "+" (V : RayLib.Ray) return raylib_h.Ray is
     (position => +V.Position, direction => +V.Direction);
   function "+" (V : raylib_h.Ray) return RayLib.Ray is
     (Position => +V.position, Direction => +V.direction);
   function "+" (V : RayLib.Ray_Collision) return raylib_h.RayCollision is
     (hit    => bool (V.Hit), distance => V.Distance, point => +V.Point,
      normal => +V.Normal);
   function "+" (V : raylib_h.RayCollision) return RayLib.Ray_Collision is
     (Hit    => Boolean (V.hit), Distance => V.distance, Point => +V.point,
      Normal => +V.normal);
   function "+" (V : RayLib.Bounding_Box) return raylib_h.BoundingBox is
     (min => +V.Min, max => +V.Max);
   function "+" (V : raylib_h.BoundingBox) return RayLib.Bounding_Box is
     (Min => +V.min, Max => +V.max);
end RayLib;

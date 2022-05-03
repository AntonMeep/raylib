with Interfaces.C; use Interfaces.C;

package RayLib is
   Version : constant String := "4.0";

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

   type Color is record
      R, G, B, A : unsigned_char;
   end record with
      Convention => C_Pass_By_Copy;

   type Rectangle is record
      X, Y, Width, Height : Float;
   end record with
      Convention => C_Pass_By_Copy;

      --  TODO: rest of the definitons

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

   procedure Init_Window (Width, Height : Natural; Title : String);
   function Window_Should_Close return Boolean;
   procedure Close_Window with
      Import        => True,
      Convention    => C,
      External_Name => "CloseWindow";

   procedure Clear_Background (Color : RayLib.Color) with
      Import        => True,
      Convention    => C,
      External_Name => "ClearBackground";
   procedure Begin_Drawing with
      Import        => True,
      Convention    => C,
      External_Name => "BeginDrawing";
   procedure End_Drawing with
      Import        => True,
      Convention    => C,
      External_Name => "EndDrawing";

   procedure Set_Target_FPS (FPS : int) with
      Import        => True,
      Convention    => C,
      External_Name => "SetTargetFPS";

   procedure Draw_Text
     (Text  : String; Position_X, Position_Y : int; Font_Size : int;
      Color : RayLib.Color);
end RayLib;

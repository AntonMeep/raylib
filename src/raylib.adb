pragma Ada_2012;

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings;    use Interfaces.C.Strings;

package body RayLib is
   procedure Init_Window (Width, Height : Natural; Title : String) is
      procedure Internal (width : int; height : int; title : chars_ptr) with
         Import        => True,
         Convention    => C,
         External_Name => "InitWindow";

      Title_Copy : chars_ptr := New_String (Title);
   begin
      Internal (int (Width), int (Height), Title_Copy);
      Free (Title_Copy);
   end Init_Window;

   function Window_Should_Close return Boolean is
      function Internal return bool with
         Import        => True,
         Convention    => C,
         External_Name => "WindowShouldClose";
   begin
      return Boolean (Internal);
   end Window_Should_Close;

   procedure Draw_Text
     (Text  : String; Position_X, Position_Y : Natural; Font_Size : Natural;
      Color : RayLib.Color)
   is
      procedure Internal
        (text  : chars_ptr; posX : int; posY : int; fontSize : int;
         color : RayLib.Color) with
         Import        => True,
         Convention    => C,
         External_Name => "DrawText";

      Text_Copy : chars_ptr := New_String (Text);
   begin
      Internal
        (Text_Copy, int (Position_X), int (Position_Y), int (Font_Size),
         Color);
      Free (Text_Copy);
   end Draw_Text;
end RayLib;

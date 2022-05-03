with RayLib; use RayLib;

procedure Core_Basic_Window is
   Screen_Width  : Integer := 800;
   Screen_Height : Integer := 450;
begin
   Init_Window
     (Screen_Width, Screen_Height, "raylib [core] example - basic window");

   Set_Target_FPS (60);

   while not Window_Should_Close loop
      Begin_Drawing;

      Clear_Background (Ray_White);

      Draw_Text
        ("Congrats! You created your first window!", 190, 200, 20, Light_Gray);

      End_Drawing;
   end loop;

   Close_Window;
end Core_Basic_Window;

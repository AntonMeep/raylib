with RayLib; use RayLib;

procedure Textures_Image_Loading is
   Screen_Width  : constant Integer := 800;
   Screen_Height : constant Integer := 450;
begin
   Init_Window
     (Screen_Width, Screen_Height,
      "raylib [textures] example - image loading");

   --  Declare block is used so that controlled Image and Texture objects can
   --  be automatically freed at the end of the scope. No need for explicit
   --  Unload* call
   declare
      Image : constant RayLib.Image :=
        Load_Image
          ("../deps/raylib/examples/textures/resources/raylib_logo.png");
      Texture : constant Texture2D := Load_Texture_From_Image (Image);
   begin
      Set_Target_FPS (60);

      while not Window_Should_Close loop
         Begin_Drawing;

         Clear_Background (Ray_White);

         Draw_Texture
           (Texture, Screen_Width / 2 - Texture.Width / 2,
            Screen_Height / 2 - Texture.Height / 2, White);
         Draw_Text
           ("this IS a texture loaded from an image!", 300, 370, 10, Gray);

         End_Drawing;
      end loop;
   end;
   --  At this point Image and Texture are automatically unloaded

   Close_Window;
end Textures_Image_Loading;

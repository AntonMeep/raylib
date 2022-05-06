with RayLib; use RayLib;

procedure Core_Basic_Screen_Manager is
   type Game_Screen is (Logo, Title, Gameplay, Ending);

   Screen_Width  : constant Integer := 800;
   Screen_Height : constant Integer := 450;

   Current_Screen : Game_Screen := Logo;

   Frame_Counter : Natural := 0;
begin
   Init_Window
     (Screen_Width, Screen_Height,
      "raylib [core] example - basic screen manager");

   Set_Target_FPS (60);

   while not Window_Should_Close loop
      case Current_Screen is
         when Logo =>
            Frame_Counter := Frame_Counter + 1;

            if Frame_Counter > 120 then
               Current_Screen := Title;
            end if;
         when Title =>
            if Is_Key_Pressed (Key_Enter) or Is_Gesture_Detected (Gesture_Tap)
            then
               Current_Screen := Gameplay;
            end if;
         when Gameplay =>
            if Is_Key_Pressed (Key_Enter) or Is_Gesture_Detected (Gesture_Tap)
            then
               Current_Screen := Ending;
            end if;
         when Ending =>
            if Is_Key_Pressed (Key_Enter) or Is_Gesture_Detected (Gesture_Tap)
            then
               Current_Screen := Title;
            end if;
      end case;

      Begin_Drawing;

      Clear_Background (Ray_White);

      case Current_Screen is
         when Logo =>
            Draw_Text ("LOGO SCREEN", 20, 20, 40, Light_Gray);
            Draw_Text ("WAIT for 2 SECONDS...", 290, 220, 20, Gray);
         when Title =>
            Draw_Rectangle (0, 0, Screen_Width, Screen_Height, Green);
            Draw_Text ("TITLE SCREEN", 20, 20, 40, Dark_Green);
            Draw_Text
              ("PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN", 120, 220, 20,
               Dark_Green);
         when Gameplay =>
            Draw_Rectangle (0, 0, Screen_Width, Screen_Height, Purple);
            Draw_Text ("GAMEPLAY SCREEN", 20, 20, 40, Maroon);
            Draw_Text
              ("PRESS ENTER or TAP to JUMP to ENDING SCREEN", 120, 220, 20,
               Maroon);
         when Ending =>
            Draw_Rectangle (0, 0, Screen_Width, Screen_Height, Blue);
            Draw_Text ("ENDING SCREEN", 20, 20, 40, Dark_Blue);
            Draw_Text
              ("PRESS ENTER or TAP to RETURN to TITLE SCREEN", 120, 220, 20,
               Dark_Blue);
      end case;

      End_Drawing;
   end loop;

   Close_Window;
end Core_Basic_Screen_Manager;

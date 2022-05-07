with RayLib; use RayLib;

procedure Core_Window_Flags is
   Screen_Width  : constant Integer := 800;
   Screen_Height : constant Integer := 450;

   Ball_Position, Ball_Speed : Vector2;
   Ball_Radius               : Float;

   Frame_Counter : Natural := 0;
begin
   Init_Window
     (Screen_Width, Screen_Height, "raylib [core] example - window flags");

   Ball_Position :=
     (Float (Get_Screen_Width) / 2.0, Float (Get_Screen_Height) / 2.0);
   Ball_Speed  := (5.0, 4.0);
   Ball_Radius := 20.0;

   --  Set_Target_FPS (60);

   while not Window_Should_Close loop
      if Is_Key_Pressed (Key_F) then
         Toggle_Fullscreen;
      end if;

      if Is_Key_Pressed (Key_R) then
         if Is_Window_State (Flag_Window_Resizable) then
            Clear_Window_State (Flag_Window_Resizable);
         else
            Set_Window_State (Flag_Window_Resizable);
         end if;
      end if;

      if Is_Key_Pressed (Key_D) then
         if Is_Window_State (Flag_Window_Undecorated) then
            Clear_Window_State (Flag_Window_Undecorated);
         else
            Set_Window_State (Flag_Window_Undecorated);
         end if;
      end if;

      if Is_Key_Pressed (Key_H) then
         if not Is_Window_State (Flag_Window_Hidden) then
            Set_Window_State (Flag_Window_Hidden);
         end if;

         Frame_Counter := 0;
      end if;

      if Is_Window_State (Flag_Window_Hidden) then
         Frame_Counter := Frame_Counter + 1;
         if Frame_Counter >= 240 then
            Clear_Window_State (Flag_Window_Hidden);
         end if;
      end if;

      if Is_Key_Pressed (Key_N) then
         if not Is_Window_State (Flag_Window_Minimized) then
            Minimize_Window;
         end if;

         Frame_Counter := 0;
      end if;

      if Is_Window_State (Flag_Window_Minimized) then
         Frame_Counter := Frame_Counter + 1;
         if Frame_Counter >= 240 then
            Restore_Window;
         end if;
      end if;

      if Is_Key_Pressed (Key_M) then
         if Is_Window_State (Flag_Window_Maximized) then
            Restore_Window;
         else
            Maximize_Window;
         end if;
      end if;

      if Is_Key_Pressed (Key_U) then
         if Is_Window_State (Flag_Window_Unfocused) then
            Clear_Window_State (Flag_Window_Unfocused);
         else
            Set_Window_State (Flag_Window_Unfocused);
         end if;
      end if;

      if Is_Key_Pressed (Key_T) then
         if Is_Window_State (Flag_Window_Topmost) then
            Clear_Window_State (Flag_Window_Topmost);
         else
            Set_Window_State (Flag_Window_Topmost);
         end if;
      end if;

      if Is_Key_Pressed (Key_A) then
         if Is_Window_State (Flag_Window_Always_Run) then
            Clear_Window_State (Flag_Window_Always_Run);
         else
            Set_Window_State (Flag_Window_Always_Run);
         end if;
      end if;

      if Is_Key_Pressed (Key_V) then
         if Is_Window_State (Flag_Vsync_Hint) then
            Clear_Window_State (Flag_Vsync_Hint);
         else
            Set_Window_State (Flag_Vsync_Hint);
         end if;
      end if;

      Ball_Position.X := Ball_Position.X + Ball_Speed.X;
      Ball_Position.Y := Ball_Position.Y + Ball_Speed.Y;

      if Ball_Position.X >= (Float (Get_Screen_Width) - Ball_Radius)
        or else Ball_Position.X <= Ball_Radius
      then
         Ball_Speed.X := -Ball_Speed.X;
      end if;

      if Ball_Position.Y >= (Float (Get_Screen_Height) - Ball_Radius)
        or else Ball_Position.Y <= Ball_Radius
      then
         Ball_Speed.Y := -Ball_Speed.Y;
      end if;

      Begin_Drawing;

      if Is_Window_State (Flag_Window_Transparent) then
         Clear_Background (Blank);
      else
         Clear_Background (Ray_White);
      end if;

      Draw_Circle (Ball_Position, Ball_Radius, Maroon);
      Draw_Rectangle_Lines
        ((X      => 0.0, Y => 0.0, Width => Float (Get_Screen_Width),
          Height => Float (Get_Screen_Height)),
         4.0, Ray_White);

      Draw_Circle (Get_Mouse_Position, 10.0, Dark_Blue);

      Draw_FPS (10, 10);

      Draw_Text
        ("Screen Size: [" & Get_Screen_Width'Image & "," &
         Get_Screen_Height'Image & "]",
         10, 40, 10, Green);

      Draw_Text
        ("Following flags can be set after window creation:", 10, 60, 10,
         Gray);

      if Is_Window_State (Flag_Fullscreen_Mode) then
         Draw_Text ("[F] FLAG_FULLSCREEN_MODE: on", 10, 80, 10, Lime);
      else
         Draw_Text ("[F] FLAG_FULLSCREEN_MODE: off", 10, 80, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Resizable) then
         Draw_Text ("[R] FLAG_WINDOW_RESIZABLE: on", 10, 100, 10, Lime);
      else
         Draw_Text ("[R] FLAG_WINDOW_RESIZABLE: off", 10, 100, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Undecorated) then
         Draw_Text ("[D] FLAG_WINDOW_UNDECORATED: on", 10, 120, 10, Lime);
      else
         Draw_Text ("[D] FLAG_WINDOW_UNDECORATED: off", 10, 120, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Hidden) then
         Draw_Text ("[H] FLAG_WINDOW_HIDDEN: on", 10, 140, 10, Lime);
      else
         Draw_Text ("[H] FLAG_WINDOW_HIDDEN: off", 10, 140, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Minimized) then
         Draw_Text ("[N] FLAG_WINDOW_MINIMIZED: on", 10, 160, 10, Lime);
      else
         Draw_Text ("[N] FLAG_WINDOW_MINIMIZED: off", 10, 160, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Maximized) then
         Draw_Text ("[M] FLAG_WINDOW_MAXIMIZED: on", 10, 180, 10, Lime);
      else
         Draw_Text ("[M] FLAG_WINDOW_MAXIMIZED: off", 10, 180, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Unfocused) then
         Draw_Text ("[G] FLAG_WINDOW_UNFOCUSED: on", 10, 200, 10, Lime);
      else
         Draw_Text ("[U] FLAG_WINDOW_UNFOCUSED: off", 10, 200, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Topmost) then
         Draw_Text ("[T] FLAG_WINDOW_TOPMOST: on", 10, 220, 10, Lime);
      else
         Draw_Text ("[T] FLAG_WINDOW_TOPMOST: off", 10, 220, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Always_Run) then
         Draw_Text ("[A] FLAG_WINDOW_ALWAYS_RUN: on", 10, 240, 10, Lime);
      else
         Draw_Text ("[A] FLAG_WINDOW_ALWAYS_RUN: off", 10, 240, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Vsync_Hint) then
         Draw_Text ("[V] FLAG_VSYNC_HINT: on", 10, 260, 10, Lime);
      else
         Draw_Text ("[V] FLAG_VSYNC_HINT: off", 10, 260, 10, Maroon);
      end if;

      Draw_Text
        ("Following flags can only be set before window creation:", 10, 300,
         10, Gray);

      if Is_Window_State (Flag_Window_HighDPI) then
         Draw_Text ("FLAG_WINDOW_HIGHDPI: on", 10, 320, 10, Lime);
      else
         Draw_Text ("FLAG_WINDOW_HIGHDPI: off", 10, 320, 10, Maroon);
      end if;

      if Is_Window_State (Flag_Window_Transparent) then
         Draw_Text ("FLAG_WINDOW_TRANSPARENT: on", 10, 340, 10, Lime);
      else
         Draw_Text ("FLAG_WINDOW_TRANSPARENT: off", 10, 340, 10, Maroon);
      end if;

      if Is_Window_State (Flag_MSAA_4x_Hint) then
         Draw_Text ("FLAG_MSAA_4X_HINT: on", 10, 360, 10, Lime);
      else
         Draw_Text ("FLAG_MSAA_4X_HINT: off", 10, 360, 10, Maroon);
      end if;

      End_Drawing;
   end loop;

   Close_Window;
end Core_Window_Flags;

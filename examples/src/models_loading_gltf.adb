with RayLib; use RayLib;

procedure Models_Loading_GLTF is
   Screen_Width  : constant Integer := 800;
   Screen_Height : constant Integer := 450;
begin
   Init_Window
     (Screen_Width, Screen_Height, "raylib [models] example - model");

   declare
      Camera : RayLib.Camera :=
        (Position   => (10.0, 10.0, 10.0), Target => (0.0, 0.0, 0.0),
         Up         => (0.0, 1.0, 0.0), Fov_Y => 45.0,
         Projection => Camera_Perspective);

      Path_Base : constant String :=
        "../deps/raylib/examples/models/resources/models/gltf/";

      Models : constant Model_Array (1 .. 7) :=
        (Load_Model (Path_Base & "raylib_32x32.glb"),
         Load_Model (Path_Base & "rigged_figure.glb"),
         Load_Model (Path_Base & "BoxAnimated.glb"),
         Load_Model (Path_Base & "AnimatedTriangle.gltf"),
         Load_Model (Path_Base & "/AnimatedMorphCube.glb"),
         Load_Model (Path_Base & "vertex_colored_object.glb"),
         Load_Model (Path_Base & "girl.glb"));

      Current_Model : Natural          := Models'First;
      Position      : constant Vector3 := (0.0, 0.0, 0.0);
   begin
      Set_Camera_Mode (Camera, Camera_Free);

      Set_Target_FPS (60);

      while not Window_Should_Close loop
         Update_Camera (Camera);

         if Is_Key_Released (Key_Right) then
            Current_Model := Current_Model + 1;
            if Current_Model > Models'Last then
               Current_Model := Models'First;
            end if;
         end if;

         if Is_Key_Released (Key_Left) then
            Current_Model := Current_Model - 1;
            if Current_Model < Models'First then
               Current_Model := Models'Last;
            end if;
         end if;

         Begin_Drawing;

         Clear_Background (Sky_Blue);

         Begin_Mode3D (Camera);

         Draw_Model (Models (Current_Model), Position, 1.0, White);
         Draw_Grid (10, 1.0);

         End_Mode3D;

         End_Drawing;
      end loop;
   end;
   --  At this point all models are automatically unloaded

   Close_Window;
end Models_Loading_GLTF;

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Parse_Args; use Parse_Args;

with JSON.Parsers;
with JSON.Types;

procedure RayGen is
   INDENT : constant String := "   ";

   package JSON_Types is new JSON.Types (Integer, Float);
   package JSON_Parser is new JSON.Parsers (JSON_Types);

   use JSON_Types;

   Args        : Argument_Parser;
   Output_Spec : access File_Type;

   function To_Train_Case (Value : String) return String is
      Result : String (1 .. Value'Length * 2);
      Index  : Positive := Result'First;
   begin
      for I in Value'Range loop
         if Index = Result'First then
            --  First letter is always capital
            Result (Index) := To_Upper (Value (I));
            Index          := Index + 1;
         elsif Is_Upper (Value (I)) and then I - 1 >= Value'First
           and then not Is_Digit (Value (I - 1))
         then
            --  Capital letters are prepended with an underscore
            --  unless its a part of an abbreviation,
            --  for example, "2D"
            Result (Index)     := '_';
            Result (Index + 1) := Value (I);
            Index              := Index + 2;
         else
            --  Other letters are preserved
            Result (Index) := Value (I);
            Index          := Index + 1;
         end if;
      end loop;
      return Result (Result'First .. Index - 1);
   end To_Train_Case;

   function Adaify_Name (Value_Original : String) return String is
      Value : String  := To_Train_Case (Value_Original);
      Index : Natural := 0;
   begin
      Index := Ada.Strings.Fixed.Index (Value, "Vr_");
      if Index /= 0 then
         Replace_Slice (Value, Index, Index + 2, "VR_");
      end if;
      return Value;
   end Adaify_Name;

   function Adaify_Type (Value : String) return String is
      use Ada.Strings;
   begin
      if Equal_Case_Insensitive (Value, "int") then
         return "Integer";
      elsif Equal_Case_Insensitive (Value, "unsigned int") then
         return "Natural";
      else
         return To_Train_Case (Value);
      end if;
   end Adaify_Type;

   function To_Ada_Name (Value : JSON_Value) return String is
     (Adaify_Name (Value.Value));
   function To_Ada_Type (Value : JSON_Value) return String is
     (Adaify_Type (Value.Value));
begin
   Args.Add_Option
     (Make_Boolean_Option, "help", 'h', Usage => "Display program help");
   Args.Add_Option
     (Make_String_Option ("-"), "output-spec", 's',
      Usage =>
        "Output package specification file." & " `-` - output to stdout");
   Args.Append_Positional (Make_String_Option, "INFILE");
   Args.Set_Prologue
     ("Program to generate Ada wrapper to raylib based" & ASCII.LF &
      "on json description provided by raylib parser");

   Args.Parse_Command_Line;

   if not Args.Parse_Success then
      Put_Line
        ("Could not parse command line arguments: " & Args.Parse_Message);
      New_Line;
      Args.Usage;
      Set_Exit_Status (-1);
      return;
   end if;

   if Args.Boolean_Value ("help") then
      Args.Usage;
      return;
   end if;

   if Args.String_Value ("INFILE") = "" then
      Put_Line ("INFILE was not provided. See --help for more information");
      Set_Exit_Status (-1);
      return;
   end if;

   if Args.String_Value ("output-spec") = "-" then
      Output_Spec := new File_Type'(Standard_Output);
   else
      Output_Spec := new File_Type;
      Create (Output_Spec.all, Out_File, Args.String_Value ("output-spec"));
   end if;

   declare
      Parser : JSON_Parser.Parser :=
        JSON_Parser.Create_From_File (Args.String_Value ("INFILE"));
      Value : constant JSON_Value := Parser.Parse;
   begin
      Put_Line ("Successfully opened " & Args.String_Value ("INFILE"));

      Put_Line (Output_Spec.all, "package RayLib is");

      Put_Line ("> Processing struct definitions");

      for Struct of Value ("structs") loop
         Put_Line (">> " & Struct ("name").Value);

         if Struct ("description").Value /= "" then
            Put_Line
              (Output_Spec.all,
               INDENT & "--  " & Struct ("description").Value);
         end if;
         Put_Line
           (Output_Spec.all,
            INDENT & "type " & To_Ada_Name (Struct ("name")) & " is record");

         for Field of Struct ("fields") loop
            Put_Line
              (Output_Spec.all,
               2 * INDENT & To_Ada_Name (Field ("name")) & " : " &
               To_Ada_Type (Field ("type")) & ";");
            if Field ("description").Value /= "" then
               Put_Line
                 (Output_Spec.all,
                  2 * INDENT & "--  " & Field ("description").Value);
            end if;
         end loop;

         Put_Line (Output_Spec.all, "   end record;");
         New_Line (Output_Spec.all);
      end loop;

      Put_Line (Output_Spec.all, "end RayLib;");
   end;
end RayGen;

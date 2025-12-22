with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Directories;

with Functions;
with Core;

procedure DFM is
   use Ada.Text_IO;
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Directories;

   use Functions;
   use Core;

   I : Integer := 1;
   Folder : Unbounded_String := Null_Unbounded_String;
   Ignored : Unbounded_String := Null_Unbounded_String;
   Sample_Path : Unbounded_String := Null_Unbounded_String;
   Home : constant String := Ada.Environment_Variables.Value("HOME");
   Verbose_Mode : Boolean := False;
begin
   Folder := To_Unbounded_String(Home);
   while I <= Argument_Count loop
      declare Arg : constant String := Argument(I);
      begin
         if Arg = "-f" or Arg = "--folder" then
            if I < Argument_Count then
               I := I+1;
               if not (To_String(To_Unbounded_String(Argument(I))) = "-") then
                  Folder := To_Unbounded_String(Argument(I));
                  if Folder /= Null_Unbounded_String then
                     Put_Line("Selected folder: " & To_String(Folder));
                  else
                     Folder := To_Unbounded_String(Home);
                  end if;
                  if Folder /= Null_Unbounded_String and then not Exists(To_String(Folder)) then
                     Display_Message(Red, "Error: " & Arg & " Wrong path. Folder " & To_String(Folder) & " might not exists.");
                     Display_Help;
                     Set_Exit_Status(Failure);
                     return;
                  end if;
               end if;
            else
               Display_Message(Red, "Error: " & Arg & " No folder specified.");
               Display_Help;
               Set_Exit_Status(Failure);
               return;
            end if;

         elsif Arg = "-i" or Arg = "--ignore" then
            if I < Argument_Count then
               I := I+1;
               Ignored := To_Unbounded_String(Argument(I));
               if Ignored /= Null_Unbounded_String then
                  Put_Line("Selected ignore file: " & To_String(Ignored));
               else
                  Ignored := To_Unbounded_String("");
               end if;
            else
               if not Exists(To_String(Folder)) then
                  Display_Message(Red, "Error: " & Arg & " Wrong path. File " & To_String(Folder) & " might not exists.");
                  Display_Help;
                  Set_Exit_Status(Failure);
                  return;
               end if;
            end if;

         elsif Arg = "-s" or Arg = "--sample" then
            if I < Argument_Count then
               I := I+1;
               if not (To_String(To_Unbounded_String(Argument(I))) = "-") then
                  Sample_Path := To_Unbounded_String(Argument(I));
                  if Sample_Path /= Null_Unbounded_String then
                     Put_Line("Selected sample path: " & To_String(Sample_Path));
                  else
                     Sample_Path := To_Unbounded_String(Home);
                  end if;
                  if Sample_Path /= Null_Unbounded_String and then not Exists(To_String(Sample_Path)) then
                     Display_Message(Red, "Error: " & Arg & " Wrong path. File " & To_String(Sample_Path) & " might not exists.");
                     Display_Help;
                     Set_Exit_Status(Failure);
                     return;
                  end if;
               else
                  Put_Line("No sample path specified. Default is " & Home);
                  Sample_Path := To_Unbounded_String(Home);
               end if;
            else
               Put_Line("No sample path specified. Default is " & Home);
               Sample_Path := To_Unbounded_String(Home);
            end if;
            if Create_Sample(To_String(Sample_Path)) then
               Put_Line("Sample created at " & To_String(Sample_Path) & "/ignorefile");
               Set_Exit_Status(Success);
               return;
            else
               Display_Message(Red, "Error: Failed to create file at location " & To_String(Sample_Path));
               Set_Exit_Status(Failure);
               return;
            end if;

         elsif Arg = "-h" or Arg = "--help" then
            if I = 1 and Argument_Count = 1 then
               Display_Help_Extended;
               return;
            else
               Display_Message(Red, "Error: " & Arg & " must be used alone.");
               Display_Help;
               Set_Exit_Status(Failure);
               return;
            end if;

         elsif Arg = "-v" or Arg = "--verbose" then
            Verbose_Mode := True;

         else
            Display_Message(Red, "Error: unknown option '" & Arg & "'");
            Display_Help;
            Set_Exit_Status (Failure);
            return;
         end if;
         I := I+1;
      end;
   end loop;
   Put_Line("Selected folder: " & To_String(Folder));
   Start_Searching(To_String(Folder), To_String(Ignored), Verbose_Mode);

end DFM;

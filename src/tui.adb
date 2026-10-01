with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Directories;
with Ada.Interrupts;
with Ada.Interrupts.Names;
with GNAT.OS_Lib;

package body TUI is
   type Duplicate_Group is record
      Hash : Unbounded_String;
      Paths : Core.Path_Vector;
   end record;
   
   package Duplicate_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Duplicate_Group);
   
   use Duplicate_Vectors;
   
   Color_Pair_Selected : constant Color_Pair := 1;
   Color_Pair_Normal : constant Color_Pair := 2;
   Color_Pair_Header : constant Color_Pair := 3;
   Color_Pair_Help : constant Color_Pair := 4;
   
   Info_Win : Window;
   Left_Win : Window;
   Right_Win : Window;
   Status_Win : Window;
   
   Info_Height : constant Line_Count := 3;
   Left_Height, Left_Width : Line_Count;
   Right_Height, Right_Width : Line_Count;
   
   Current_Group : Positive := 1;
   Current_File : Positive := 1;
   
   Duplicates : Duplicate_Vectors.Vector;
   Show_Full_Hash : Boolean := False;
   Use_Link : Boolean := False;
   Total_Recovered_Size : Long_Long_Integer := 0;
   Total_Duplicates : Natural := 0;
   Processing_Time_Str : Unbounded_String := Null_Unbounded_String;
   

   procedure Init_Colors is
   begin
      Start_Color;
      Init_Pair(Color_Pair_Selected, Cyan, Black);
      Init_Pair(Color_Pair_Normal, White, Black);
      Init_Pair(Color_Pair_Header, Yellow, Blue);
      Init_Pair(Color_Pair_Help, Green, Black);
   end Init_Colors;
   

   procedure Set_Color_Pair(Win : Window; Pair : Color_Pair) is
   begin
      Set_Color(Win, Pair);
      Set_Character_Attributes(Win, (others => False));
   end Set_Color_Pair;
   

   procedure Init_Windows is
      Lines : Line_Count;
      Cols : Column_Count;
      Vis : Cursor_Visibility := Invisible;
   begin
      Init_Screen;
      Set_Echo_Mode(False);
      Set_Cbreak_Mode(True);
      Set_Cursor_Visibility(Vis);

      if Has_Colors then
         Init_Colors;
      end if;
      
      Get_Size(Win => Standard_Window, Number_Of_Lines => Lines, Number_Of_Columns => Cols);
      
      Left_Height := Lines - Info_Height - 3;
      Left_Width := Line_Count(Column_Count'(Cols / 2));
      Right_Height := Lines - Info_Height - 3;
      Right_Width := Line_Count(Cols - Column_Count(Left_Width) - 3);
      Info_Win := Create(Number_Of_Lines => Info_Height,
                         Number_Of_Columns => Cols,
                         First_Line_Position => 0,
                         First_Column_Position => 0);
      
      Left_Win := Create(Number_Of_Lines => Left_Height, 
                         Number_Of_Columns => Column_Count(Left_Width), 
                         First_Line_Position => Info_Height, 
                         First_Column_Position => 1);
      Right_Win := Create(Number_Of_Lines => Right_Height, 
                          Number_Of_Columns => Column_Count(Right_Width), 
                          First_Line_Position => Info_Height, 
                          First_Column_Position => Column_Position(Left_Width + 2));
      Status_Win := Create(Number_Of_Lines => 2,
                           Number_Of_Columns => Cols,
                           First_Line_Position => Lines - 2,
                           First_Column_Position => 0);
      
      Set_KeyPad_Mode(Left_Win, True);
      Set_KeyPad_Mode(Right_Win, True);
      
      Refresh;
   end Init_Windows;
   

   procedure Cleanup_Windows is
   begin
      Delete(Info_Win);
      Delete(Left_Win);
      Delete(Right_Win);
      Delete(Status_Win);
      End_Windows;
   end Cleanup_Windows;
   
   
   protected Interrupt_Handler is
      procedure Handle;
      pragma Interrupt_Handler(Handle);
   end Interrupt_Handler;
   
   
      protected body Interrupt_Handler is
      procedure Handle is
         use GNAT.OS_Lib;
      begin
         Cleanup_Windows;
         OS_Exit(130); -- killed by SIGINT
      end Handle;
   end Interrupt_Handler;
   
   
   procedure Install_Interrupt_Handler is
      use Ada.Interrupts;
      use Ada.Interrupts.Names;
   begin
      Attach_Handler(Interrupt_Handler.Handle'Access, SIGINT);
   end Install_Interrupt_Handler;

   
   procedure Remove_Interrupt_Handler is
      use Ada.Interrupts;
      use Ada.Interrupts.Names;
   begin
      Detach_Handler(SIGINT);
   end Remove_Interrupt_Handler;


   function Format_Size(Size_In_Bytes : Long_Long_Integer) return String is
      KB : constant Long_Long_Integer := 1024;
      MB : constant Long_Long_Integer := KB * 1024;
      GB : constant Long_Long_Integer := MB * 1024;
      TB : constant Long_Long_Integer := GB * 1024;
      
      function Trim(S : String) return String is
      begin
         if S'Length > 0 and then S(S'First) = ' ' then
            return S(S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Trim;
      
      function To_Fixed(Value : Long_Long_Integer; Divisor : Long_Long_Integer) return String is
         Whole : constant Long_Long_Integer := Value / Divisor;
         Frac : constant Long_Long_Integer := ((Value mod Divisor) * 10) / Divisor;
      begin
         return Trim(Long_Long_Integer'Image(Whole)) & "." & Trim(Long_Long_Integer'Image(Frac));
      end To_Fixed;
   begin
      if Size_In_Bytes >= TB then
         return To_Fixed(Size_In_Bytes, TB) & " TB";
      elsif Size_In_Bytes >= GB then
         return To_Fixed(Size_In_Bytes, GB) & " GB";
      elsif Size_In_Bytes >= MB then
         return To_Fixed(Size_In_Bytes, MB) & " MB";
      elsif Size_In_Bytes >= KB then
         return To_Fixed(Size_In_Bytes, KB) & " KB";
      else
         return Trim(Long_Long_Integer'Image(Size_In_Bytes)) & " bytes";
      end if;
   end Format_Size;
   

   function Compute_Recovered_Size(Hashes : Core.String_Map) return Long_Long_Integer is
      use Ada.Directories;
      use Core.String_Maps;
      use Core.Path_Vectors;
      
      Total_Size : Long_Long_Integer := 0;
      
      procedure Process_Entry(Position : Core.String_Maps.Cursor) is
         Paths : constant Core.Path_Vector := Element(Position);
         Files_Count : constant Natural := Natural(Paths.Length);
         File_Size_Value : Long_Long_Integer;
      begin
         if Files_Count > 1 then
            declare
               First_File : constant String := To_String(Paths.First_Element);
            begin
               File_Size_Value := Long_Long_Integer(Size(First_File));
               Total_Size := Total_Size + File_Size_Value * Long_Long_Integer(Files_Count - 1);
            exception
               when others =>
                  null;
            end;
         end if;
      end Process_Entry;
      
   begin
      Hashes.Iterate(Process_Entry'Access);
      return Total_Size;
   end Compute_Recovered_Size;
   function Trim_Image(N : Integer) return String is
      Img : constant String := Integer'Image(N);
   begin
      if Img(Img'First) = ' ' then
         return Img(Img'First + 1 .. Img'Last);
      else
         return Img;
      end if;
   end Trim_Image;
   
   
   function Format_Duration(D : Duration) return String is
      Total_Seconds : constant Natural := Natural(D);
      Hours : constant Natural := Total_Seconds / 3600;
      Minutes : constant Natural := (Total_Seconds mod 3600) / 60;
      Seconds : constant Natural := Total_Seconds mod 60;
   begin
      if Hours > 0 then
         return Trim_Image(Hours) & "h " & Trim_Image(Minutes) & "m " & Trim_Image(Seconds) & "s";
      elsif Minutes > 0 then
         return Trim_Image(Minutes) & "m " & Trim_Image(Seconds) & "s";
      else
         return Trim_Image(Seconds) & "s";
      end if;
   end Format_Duration;
   

   function Display_Hash(Hash : String; Width : Natural) return String is
   begin
      if Show_Full_Hash then
         if Hash'Length <= Width then
            return Hash;
         else
            return Hash(Hash'First .. Hash'First + Width - 1);
         end if;
      else
         if Hash'Length <= Width then
            return Hash;
         elsif Width < 10 then
            return Hash(Hash'First .. Hash'First + Width - 1);
         else
            declare
               Half : constant Natural := (Width - 3) / 2;
            begin
               return Hash(Hash'First .. Hash'First + Half - 1) & "..." & 
                      Hash(Hash'Last - Half + 1 .. Hash'Last);
            end;
         end if;
      end if;
   end Display_Hash;
   
   
   procedure Display_Info_Panel is
      Lines : Line_Count;
      Cols : Column_Count;
   begin
      Get_Size(Win => Standard_Window, Number_Of_Lines => Lines, Number_Of_Columns => Cols);
      
      Clear(Info_Win);
      Box(Info_Win);
      
      if Has_Colors then
         Set_Color_Pair(Info_Win, Color_Pair_Header);
      end if;
      
      Move_Cursor(Info_Win, 0, 2);
      Add(Info_Win, " Duplicate Files Summary ");
      
      if Has_Colors then
         Set_Color_Pair(Info_Win, Color_Pair_Normal);
      end if;
      
      Move_Cursor(Info_Win, 1, 2);
      Add(Info_Win, "Total duplicate groups: " & Trim_Image(Integer(Duplicates.Length)));
      
      Move_Cursor(Info_Win, 1, Column_Position(Cols) / 2);
      Add(Info_Win, "Total duplicates: " & Trim_Image(Total_Duplicates));
      
      if Total_Recovered_Size > 0 then
         declare
            Size_Str : constant String := Format_Size(Total_Recovered_Size);
            Msg : constant String := "Space recoverable: " & Size_Str;
            Start_Col : constant Column_Position := Column_Position(Cols) - Column_Position(Msg'Length) - 2;
         begin
            Move_Cursor(Info_Win, 1, Start_Col);
            if Has_Colors then
               Set_Color_Pair(Info_Win, Color_Pair_Help);
            end if;
            Add(Info_Win, Msg);
            if Has_Colors then
               Set_Color_Pair(Info_Win, Color_Pair_Normal);
            end if;
         end;
      end if;
      
      Refresh(Info_Win);
   end Display_Info_Panel;
   

   procedure Display_Left_Panel is
      Y : Line_Position := 2;
      Max_Display : constant Natural := Natural(Left_Height) - 3;
      Start_Index : Positive := 1;
   begin
      if Current_Group > Max_Display then
         Start_Index := Current_Group - Max_Display + 1;
      end if;
      
      Clear(Left_Win);
      Box(Left_Win);
      
      if Has_Colors then
         Set_Color_Pair(Left_Win, Color_Pair_Header);
      end if;
      
      Move_Cursor(Left_Win, 0, 2);
      Add(Left_Win, " Duplicates (" & Trim_Image(Integer(Duplicates.Length)) & ") ");
      
      if Has_Colors then
         Set_Color_Pair(Left_Win, Color_Pair_Normal);
      end if;
      
      Move_Cursor(Left_Win, 1, 1);
      Add(Left_Win, "H: Toggle hash view");
      
      for I in Start_Index .. Positive'Min(Start_Index + Max_Display - 1, Positive(Duplicates.Length)) loop
         declare
            Group : constant Duplicate_Group := Duplicates.Element(I);
            Hash_Str : constant String := To_String(Group.Hash);
            Count_Str : constant String := Trim_Image(Integer(Group.Paths.Length));
            Available_Width : constant Natural := Natural(Left_Width) - 4;
         begin
            Move_Cursor(Left_Win, Y, 1);
            
            if I = Current_Group then
               if Has_Colors then
                  Set_Color_Pair(Left_Win, Color_Pair_Selected);
               end if;
               Switch_Character_Attribute(Left_Win, (Reverse_Video => True, others => False));
            end if;
            
            declare
               Prefix : constant String := "#" & Trim_Image(I) & " (" & Count_Str & " files) ";
               Hash_Display : constant String := Display_Hash(Hash_Str, Available_Width - Prefix'Length);
            begin
               Add(Left_Win, Prefix);
               Add(Left_Win, Hash_Display);
               
               for J in (Prefix'Length + Hash_Display'Length) .. Available_Width loop
                  Add(Left_Win, " ");
               end loop;
            end;
            
            if I = Current_Group then
               Switch_Character_Attribute(Left_Win, (Reverse_Video => False, others => False));
               if Has_Colors then
                  Set_Color_Pair(Left_Win, Color_Pair_Normal);
               end if;
            end if;
            
            Y := Y + 1;
         end;
      end loop;
      
      Refresh(Left_Win);
   end Display_Left_Panel;
   

   procedure Display_Right_Panel(Link_Mode : Boolean) is
      Y : Line_Position := 2;
      Max_Display : constant Natural := Natural(Right_Height) - 3;
      Start_Index : Positive := 1;
   begin
      Clear(Right_Win);
      Box(Right_Win);
      
      if Duplicates.Is_Empty then
         Move_Cursor(Right_Win, 0, 2);
         Add(Right_Win, " No duplicates ");
         Refresh(Right_Win);
         return;
      end if;
      
      declare
         Group : constant Duplicate_Group := Duplicates.Element(Current_Group);
         Files_Count : constant Natural := Natural(Group.Paths.Length);
      begin
         if Has_Colors then
            Set_Color_Pair(Right_Win, Color_Pair_Header);
         end if;
         
         Move_Cursor(Right_Win, 0, 2);
         Add(Right_Win, " Files (" & Trim_Image(Files_Count) & ") ");
         
         if Has_Colors then
            Set_Color_Pair(Right_Win, Color_Pair_Normal);
         end if;
         
         Move_Cursor(Right_Win, 1, 1);
         if Link_Mode then
            Add(Right_Win, "S: Select file as symlink source");
         else
            Add(Right_Win, "D: Delete selected file");
         end if;
         
         if Current_File > Max_Display then
            Start_Index := Current_File - Max_Display + 1;
         end if;
         
         for I in Start_Index .. Positive'Min(Start_Index + Max_Display - 1, Files_Count) loop
            declare
               Path : constant String := To_String(Group.Paths.Element(I));
               Display_Path : constant String :=
                 (if Path'Length > Natural(Right_Width) - 4 then
                    "..." & Path(Path'Last - Natural(Right_Width) + 7 .. Path'Last)
                  else
                    Path);
            begin
               Move_Cursor(Right_Win, Y, 1);
               
               if I = Current_File then
                  if Has_Colors then
                     Set_Color_Pair(Right_Win, Color_Pair_Selected);
                  end if;
                  Switch_Character_Attribute(Right_Win, (Reverse_Video => True, others => False));
               end if;
               
               Add(Right_Win, Display_Path);
               
               for J in Display_Path'Length .. Natural(Right_Width) - 4 loop
                  Add(Right_Win, " ");
               end loop;
               
               if I = Current_File then
                  Switch_Character_Attribute(Right_Win, (Reverse_Video => False, others => False));
                  if Has_Colors then
                     Set_Color_Pair(Right_Win, Color_Pair_Normal);
                  end if;
               end if;
               
               Y := Y + 1;
            end;
         end loop;
      end;
      
      Refresh(Right_Win);
   end Display_Right_Panel;
   
   
   procedure Display_Status(Message : String := ""; Link_Mode : Boolean) is
      Lines : Line_Count;
      Cols : Column_Count;
   begin
      Get_Size(Win => Standard_Window, Number_Of_Lines => Lines, Number_Of_Columns => Cols);
      
      Clear(Status_Win);
      
      if Has_Colors then
         Set_Color_Pair(Status_Win, Color_Pair_Help);
      end if;
      
      Move_Cursor(Status_Win, 0, 0);
      if Link_Mode then
         Add(Status_Win, "Navigation: Up/Down (groups) | Left/Right (files) | H: Toggle hash | S: Source | Q: Quit");
      else
         Add(Status_Win, "Navigation: Up/Down (groups) | Left/Right (files) | H: Toggle hash | D: Delete | Q: Quit");
      end if;
      
      if Length(Processing_Time_Str) > 0 then
         declare
            Time_Display : constant String := "Processing time: " & To_String(Processing_Time_Str);
            Start_Col : constant Column_Position := Column_Position(Cols) - Column_Position(Time_Display'Length) - 1;
         begin
            Move_Cursor(Status_Win, 0, Start_Col);
            Add(Status_Win, Time_Display);
         end;
      end if;
      
      if Message /= "" then
         Move_Cursor(Status_Win, 1, 0);
         Add(Status_Win, Message);
      end if;
      
      if Has_Colors then
         Set_Color_Pair(Status_Win, Color_Pair_Normal);
      end if;
      
      Refresh(Status_Win);
   end Display_Status;
   
   
   procedure Redraw_Screen(Message : String := ""; Link_Mode : Boolean) is
   begin
      Display_Info_Panel;
      Display_Left_Panel;
      Display_Right_Panel(Link_Mode);
      Display_Status(Message, Link_Mode);
   end Redraw_Screen;
   
   
   function Confirm_Delete(File_Path : String; Link_Mode : Boolean) return Boolean is
      Confirm_Win : Window;
      Lines : Line_Count;
      Cols : Column_Count;
      Win_Height : constant Line_Count := 7;
      Win_Width : constant Column_Count := 60;
      C : Key_Code;
   begin
      Get_Size(Win => Standard_Window, Number_Of_Lines => Lines, Number_Of_Columns => Cols);
      
      Confirm_Win := Create(
         Number_Of_Lines => Win_Height,
         Number_Of_Columns => Win_Width,
         First_Line_Position => (Lines - Win_Height) / 2,
         First_Column_Position => Column_Position((Cols - Win_Width) / 2)
      );
      
      Set_KeyPad_Mode(Confirm_Win, True);
      
      Box(Confirm_Win);
      
      if Has_Colors then
         Set_Color_Pair(Confirm_Win, Color_Pair_Header);
      end if;
      
      Move_Cursor(Confirm_Win, 0, 2);
      if Link_Mode then
         Add(Confirm_Win, " Confirm Source ");
      else
         Add(Confirm_Win, " Confirm Delete ");
      end if;
      
      if Has_Colors then
         Set_Color_Pair(Confirm_Win, Color_Pair_Normal);
      end if;
      
      Move_Cursor(Confirm_Win, 2, 2);
      if Link_Mode then
         Add(Confirm_Win, "Use this file?");
      else
         Add(Confirm_Win, "Delete this file?");
      end if;
      
      Move_Cursor(Confirm_Win, 3, 2);
      declare
         Display_Path : constant String := 
            (if File_Path'Length > 54 then
               "..." & File_Path(File_Path'Last - 50 .. File_Path'Last)
             else
               File_Path);
      begin
         Add(Confirm_Win, Display_Path);
      end;
      
      Move_Cursor(Confirm_Win, 5, 2);
      Add(Confirm_Win, "Y: Yes  |  N: No");
      
      Refresh(Confirm_Win);
      
      loop
         C := Get_Keystroke(Confirm_Win);
         case C is
            when Character'Pos('y') | Character'Pos('Y') =>
               Delete(Confirm_Win);
               return True;
            when Character'Pos('n') | Character'Pos('N') | 27 =>
               Delete(Confirm_Win);
               return False;
            when others =>
               null;
         end case;
      end loop;
   end Confirm_Delete;
   
   
   function Create_Symlink(Target : String; Link_Name : String) return Boolean is
      use GNAT.OS_Lib;
      Ln_Path : GNAT.OS_Lib.String_Access := Locate_Exec_On_Path("ln");
      Success : Boolean := False;
   begin
      if Ln_Path /= null then
         declare
            Args : Argument_List := (1 => new String'("-s"), 2 => new String'(Target), 3 => new String'(Link_Name));
         begin
            Spawn(Ln_Path.all, Args, Success);
            Free(Args(1));
            Free(Args(2));
            Free(Args(3));
         end;
         Free(Ln_Path);
      end if;
      return Success;
   exception
      when others =>
         return False;
   end Create_Symlink;
   
   
   procedure Delete_File(Link_Mode : Boolean) is
      use Ada.Directories;
      use Core.Path_Vectors;
      
      Group : Duplicate_Group := Duplicates.Element(Current_Group);
      File_Path : constant String := To_String(Group.Paths.Element(Current_File));
      File_Size_Value : Long_Long_Integer;
   begin
      if Confirm_Delete(File_Path, Link_Mode) then
         begin
            File_Size_Value := Long_Long_Integer(Size(File_Path));
            
            Delete_File(File_Path);
            
            Total_Recovered_Size := Total_Recovered_Size - File_Size_Value;
            Total_Duplicates := Total_Duplicates - 1;
            
            Group.Paths.Delete(Current_File);
            
            if Natural(Group.Paths.Length) <= 1 then
               Duplicates.Delete(Current_Group);
               
               if Duplicates.Is_Empty then
                  Redraw_Screen("All duplicates resolved !", Link_Mode);
               elsif Current_Group > Positive(Duplicates.Length) then
                  Current_Group := Positive(Duplicates.Length);
                  Current_File := 1;
                  Redraw_Screen("File deleted successfully !", Link_Mode);
               else
                  Current_File := 1;
                  Redraw_Screen("File deleted successfully !", Link_Mode);
               end if;
            else
               Duplicates.Replace_Element(Current_Group, Group);
               
               if Current_File > Natural(Group.Paths.Length) then
                  Current_File := Natural(Group.Paths.Length);
               end if;
               
               Redraw_Screen("File deleted successfully !", Link_Mode);
            end if;
         exception
            when others =>
               Redraw_Screen("Error: Could not delete file!", Link_Mode);
         end;
      else
         Redraw_Screen("Deletion cancelled.", Link_Mode);
      end if;
   end Delete_File;
   
   
   procedure Make_Symlink_Source(Link_Mode : Boolean) is
      use Ada.Directories;
      use Core.Path_Vectors;
      
      Group : constant Duplicate_Group := Duplicates.Element(Current_Group);
      Source_Path : constant String := To_String(Group.Paths.Element(Current_File));
      Links_Created : Natural := 0;
      Links_Failed : Natural := 0;
   begin
      if Confirm_Delete(Source_Path, Link_Mode) then
         begin
            for I in 1 .. Natural(Group.Paths.Length) loop
               if I /= Current_File then
                  declare
                     Target_Path : constant String := To_String(Group.Paths.Element(I));
                     Target_Size : Long_Long_Integer := 0;
                  begin
                     begin
                        Target_Size := Long_Long_Integer(Size(Target_Path));
                     exception
                        when others =>
                           Target_Size := 0;
                     end;
                     
                     Delete_File(Target_Path);
                     
                     if Create_Symlink(Source_Path, Target_Path) then
                        Links_Created := Links_Created + 1;
                        Total_Recovered_Size := Total_Recovered_Size - Target_Size;
                        Total_Duplicates := Total_Duplicates - 1;
                     else
                        Links_Failed := Links_Failed + 1;
                     end if;
                  exception
                     when others =>
                        Links_Failed := Links_Failed + 1;
                  end;
               end if;
            end loop;
            
            Duplicates.Delete(Current_Group);
            Current_File := 1;
            
            if Duplicates.Is_Empty then
               Current_Group := 1;
               Redraw_Screen("All duplicates resolved !", Link_Mode);
            else
               if Current_Group > Positive(Duplicates.Length) then
                  Current_Group := Positive(Duplicates.Length);
               end if;
               if Links_Failed > 0 then
                  Redraw_Screen(Trim_Image(Links_Created) & " symlink(s) created, " &
                                Trim_Image(Links_Failed) & " failed.", Link_Mode);
               else
                  Redraw_Screen(Trim_Image(Links_Created) & " symlink(s) created successfully !", Link_Mode);
               end if;
            end if;
         exception
            when others =>
               Redraw_Screen("Error: Could not create symlinks!", Link_Mode);
         end;
      else
         Redraw_Screen("Operation cancelled.", Link_Mode);
      end if;
   end Make_Symlink_Source;
   
   
   procedure Populate_Duplicates(Hashes : Core.String_Map) is
      use Core.String_Maps;
      use Core.Path_Vectors;
      
      procedure Process_Entry(Position : Core.String_Maps.Cursor) is
         Hash : constant Unbounded_String := Key(Position);
         Paths : constant Core.Path_Vector := Element(Position);
         New_Group : Duplicate_Group;
         Files_Count : Natural;
      begin
         if Natural(Paths.Length) > 1 then
            New_Group.Hash := Hash;
            New_Group.Paths := Paths;
            Duplicates.Append(New_Group);
            
            Files_Count := Natural(Paths.Length);
            Total_Duplicates := Total_Duplicates + (Files_Count - 1);
         end if;
      end Process_Entry;
      
   begin
      Duplicates.Clear;
      Total_Duplicates := 0;
      Hashes.Iterate(Process_Entry'Access);
      
      Total_Recovered_Size := Compute_Recovered_Size(Hashes);
   end Populate_Duplicates;
   
   
   procedure Handle_Input(Link_Mode : Boolean) is
      C : Key_Code;
      Files_Count : Natural;
   begin
      loop
         C := Get_Keystroke(Left_Win);
         
         case C is
            when Key_Up =>
               if Current_Group > 1 then
                  Current_Group := Current_Group - 1;
                  Current_File := 1;
               end if;
               
            when Key_Down =>
               if Current_Group < Positive(Duplicates.Length) then
                  Current_Group := Current_Group + 1;
                  Current_File := 1;
               end if;
               
            when Key_Left =>
               if Current_File > 1 then
                  Current_File := Current_File - 1;
               end if;
               
            when Key_Right =>
               Files_Count := Natural(Duplicates.Element(Current_Group).Paths.Length);
               if Current_File < Files_Count then
                  Current_File := Current_File + 1;
               end if;
               
                        when Key_Next_Page =>
               declare
                  Max_Display : constant Positive :=
                    Positive'Max(1, Natural(Left_Height) - 3);
               begin
                  if Current_Group + Max_Display <= Positive(Duplicates.Length) then
                     Current_Group := Current_Group + Max_Display;
                  else
                     Current_Group := Positive(Duplicates.Length);
                  end if;
                  Current_File := 1;
               end;

            when Key_Previous_Page =>
               declare
                  Max_Display : constant Positive :=
                    Positive'Max(1, Natural(Left_Height) - 3);
               begin
                  if Current_Group > Max_Display then
                     Current_Group := Current_Group - Max_Display;
                  else
                     Current_Group := 1;
                  end if;
                  Current_File := 1;
               end;
            
            when Character'Pos('h') | Character'Pos('H') =>
               Show_Full_Hash := not Show_Full_Hash;
               
            when Character'Pos('d') | Character'Pos('D') =>
               if not Link_Mode then
                  if not Duplicates.Is_Empty then
                     Delete_File(Link_Mode);
                     if Duplicates.Is_Empty then
                        exit;
                     end if;
                  end if;
               end if;
               
            when Character'Pos('s') | Character'Pos('S') =>
               if Link_Mode then
                  if not Duplicates.Is_Empty then
                     Make_Symlink_Source(Link_Mode);
                     if Duplicates.Is_Empty then
                        exit;
                     end if;
                  end if;
               end if;
               
            when Character'Pos('q') | Character'Pos('Q') =>
               exit;
               
            when others =>
               null;
         end case;
         
         Redraw_Screen("", Use_Link);
      end loop;
   end Handle_Input;
   
   
   procedure Display_TUI(Hashes : Core.String_Map; Elapsed_Time : Duration; Link_Mode : Boolean := False) is
   begin
      Use_Link := Link_Mode;
      Populate_Duplicates(Hashes);
      
      if Duplicates.Is_Empty then
         return;
      end if;
      
      Processing_Time_Str := To_Unbounded_String(Format_Duration(Elapsed_Time));
      Install_Interrupt_Handler;
      Init_Windows;
      
      begin
         Redraw_Screen("", Use_Link);
         Handle_Input(Use_Link);
      exception
         when others =>
            Cleanup_Windows;
            raise;
      end;
      
      Cleanup_Windows;
      Remove_Interrupt_Handler;
   end Display_TUI;
   
end TUI;

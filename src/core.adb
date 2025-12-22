with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.SHA256;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Interfaces;

with Functions;
with TUI;

package body Core is
   function Compute_File_SHA256(File_Path : String) return String is
      use Ada.Streams.Stream_IO;
      use GNAT.SHA256;
      use Ada.Streams;

      File    : File_Type;
      Stream  : Stream_Access;
      Buffer  : Stream_Element_Array(1 .. 4096);
      Last    : Stream_Element_Offset;
      Context : GNAT.SHA256.Context;
   begin
      Context := Initial_Context;
      Open(File, In_File, File_Path);
      Stream := Ada.Streams.Stream_IO.Stream(File);

      loop
         Read(Stream.all, Buffer, Last);
         exit when Last = 0;
         Update(Context, Buffer(1..Last));
      end loop;

      Close(File);
      return Digest(Context);

   exception
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         raise;
   end Compute_File_SHA256;
   
   
   function Create_Sample(Dir : String) return Boolean is
      use Ada.Text_IO;
      
      File_Path : constant String := Dir & "/ignorefile";
      File : File_Type;
   begin
      Create(File => File, Mode => Out_File, Name => File_Path);

      Put_Line (File, "# System files");
      Put_Line (File, ".DS_Store");
      Put_Line (File, "thumbs.db");
      New_Line (File);
      Put_Line (File, "# versioning folders");
      Put_Line (File, ".git");
      Put_Line (File, ".svn");
      New_Line (File);
      Put_Line (File, "# dependancies");
      Put_Line (File, "node_modules");
      Put_Line (File, "vendor");
      New_Line (File);
      Put_Line (File, "# full path");
      Put_Line (File, "/home/user/temp");
      Put_Line (File, "/var/log/debug.log");
      New_Line (File);
      Put_Line (File, "# build");
      Put_Line (File, "*.o");
      Put_Line (File, "build/");
      Put_Line (File, "dist/");
      New_Line (File);
      Put_Line (File, "# other stuff");
      Put_Line (File, ".config/");
      Put_Line (File, ".cache/");
      Put_Line (File, ".local/");
      
      Close (File);
      return True;

   exception
      when Name_Error =>
         Put_Line(Standard_Error, "Error: The specified path is invalid.");
         return False;
      when Use_Error =>
         Put_Line(Standard_Error, "Error: Permission denied or file locked.");
         return False;
      when others =>
         if Is_Open(File) then
            Close(File);
         end if;
         Put_Line(Standard_Error, "An unexpected error occurred.");
         raise;
   end Create_Sample;
    
   
   function Trim(S : String) return String is
      use Ada.Strings;
   begin
      return Ada.Strings.Fixed.Trim(S, Both);
   end Trim;
   
   
   procedure Display_Duplicates(Hashes : String_Map) is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use String_Maps;
      use Path_Vectors;
   
      Duplicate_Count : Natural := 0;
      Total_Duplicates : Natural := 0;
   
      procedure Process_Entry(Position : String_Maps.Cursor) is
         Hash : constant Unbounded_String := Key(Position);
         Paths : constant Path_Vector := Element(Position);
         Path_Count : constant Natural := Natural(Paths.Length);
      begin
         if Path_Count > 1 then
            Duplicate_Count := Duplicate_Count + 1;
            Total_Duplicates := Total_Duplicates + (Path_Count - 1);
         
            Put_Line("Duplicate #" & Duplicate_Count'Image & " (hash: " & To_String(Hash) & ") ===");
            Put_Line("  " & Path_Count'Image & " files with identical content:");
            for Path of Paths loop
               Put_Line("    - " & To_String(Path));
            end loop;
            New_Line;
         end if;
      end Process_Entry;
   
   begin
      Put_Line("Searching for Duplicates");
      New_Line;
      Hashes.Iterate(Process_Entry'Access);
   
      if Duplicate_Count = 0 then
         Put_Line("No duplicates found.");
      else
         Put_Line("Summary");
         Put_Line("Found" & Duplicate_Count'Image & " sets of duplicates");
         Put_Line("Total" & Total_Duplicates'Image & " redundant files");
      end if;
   end Display_Duplicates;
   
   
   procedure Populate_Ignored_List(Ignore_File_Path : String; List_Of_Ignored : in out String_Set) is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use String_Sets;

      use Functions;
      
      File : File_Type;
      Raw_Line : Unbounded_String;
   begin
      Open(File, In_File, Ignore_File_Path);
      while not End_Of_File(File) loop
         Raw_Line := To_Unbounded_String(Get_Line(File));
         declare
            Line : constant String := Trim(To_String(Raw_Line));
         begin
            if Line'Length > 0 then
               if Line(Line'First) /= '#' then
                  List_Of_Ignored.Include(Line, True);
                  Display_Message(Green, Line & " won't be taken into account.");
               end if;
            end if;
         end;
      end loop;
      Close(File);
      
   exception
      when Name_Error =>
         Display_Message(Red, "Error: File " & Ignore_File_Path & " doesn't exists.");
         raise;
      when others =>
         if Is_Open(File) then
            Close(File);
            Display_Message(Red, "Error: Unexpected error while parsing " & Ignore_File_Path & ". Aborted.");
         end if;
         raise;
   end Populate_Ignored_List;
   
   
   procedure Process_File(File_Path : String; Hashes : in out String_Map) is
      use Ada.Strings.Unbounded;
      use Functions;
      use Path_Vectors;
   
      Hash_Value : constant String := Compute_File_SHA256(File_Path);
      Hash_Key : constant Unbounded_String := To_Unbounded_String(Hash_Value);
      Path : constant Unbounded_String := To_Unbounded_String(File_Path);
   begin
      if Hashes.Contains(Hash_Key) then
         declare
            Paths : Path_Vector := Hashes.Element(Hash_Key);
         begin
            Paths.Append(Path);
            Hashes.Replace(Hash_Key, Paths);
         end;
      else
         declare
            Paths : Path_Vector;
         begin
            Paths.Append(Path);
            Hashes.Insert(Hash_Key, Paths);
         end;
      end if;
   exception
      when others =>
         Display_Message(Red, "Error: Unknown error when processing file " & File_Path);
         raise;
   end Process_File;
   

   procedure Loop_Rec(Path : String; Must_Ignore : Boolean; Hashes : in out String_Map; Ignored : in out String_Set; Verbose_Mode : Boolean) is
      use Ada.Text_IO;
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      
      use Functions;
      
      Search : Search_Type;
      Entry_Type : Directory_Entry_Type;
   begin
      Start_Search(Search, Directory => Path, Pattern => "*");
      while More_Entries(Search) loop
         Get_Next_Entry(Search, Entry_Type);
         declare
            Simple_Entry_Name : constant String := Simple_Name(Entry_Type);
            Full_Entry_Name : Unbounded_String;
            Entry_Kind : File_Kind;
            Skip_Entry : Boolean := False;
         begin
            if Simple_Entry_Name /= "." and Simple_Entry_Name /= ".." then
               begin
                  Full_Entry_Name := To_Unbounded_String(Full_Name(Entry_Type));
                  Entry_Kind := Kind(Entry_Type);
               exception
                  when Ada.IO_Exceptions.Name_Error =>
                     Display_Message(Red, "Broken symlink skipped: " & Simple_Entry_Name);
                     Skip_Entry := True;
                  when Ada.IO_Exceptions.Use_Error =>
                     Display_Message(Green, "Special file skipped: " & Simple_Entry_Name);
                     Skip_Entry := True;
               end;
               
               if not Skip_Entry then
                  declare
                     Full_Name_Str : constant String := To_String(Full_Entry_Name);
                  begin
                     if Must_Ignore then
                        if not Ignored.Contains(Simple_Entry_Name) and not Ignored.Contains(Full_Name_Str) then
                           case Entry_Kind is
                              when Ordinary_File =>
                                 if Verbose_Mode then
                                    Display_Message(Blue, "Processing file " & Full_Name_Str);
                                 end if;
                                 Process_File(Full_Name_Str, Hashes);
                              when Directory =>
                                 if Verbose_Mode then
                                    Display_Message(Blue, "Looping through directory " & Full_Name_Str);
                                 end if;
                                 Loop_Rec(Full_Name_Str, Must_Ignore, Hashes, Ignored, Verbose_Mode);
                              when Special_File =>
                                 Display_Message(Green, "Special file skipped: " & Full_Name_Str);
                           end case;
                        else
                           Put_Line(Full_Name_Str & " ignored.");
                        end if;
                     else
                        case Entry_Kind is
                           when Ordinary_File =>
                              Process_File(Full_Name_Str, Hashes);
                           when Directory =>
                              Loop_Rec(Full_Name_Str, Must_Ignore, Hashes, Ignored, Verbose_Mode);
                           when Special_File =>
                              Display_Message(Green, "Special file skipped: " & Full_Name_Str);
                        end case;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;
      End_Search(Search);
      
   exception
      when E : others =>
         Display_Message(Red, "Unexpected error in Loop_Rec: " & Ada.Exceptions.Exception_Message(E));
         if More_Entries(Search) then
            End_Search(Search);
         end if;
   end Loop_Rec;
   
   
   procedure Start_Searching(Folder_Path : String; Ignore_Path : String; Verbose_Mode : Boolean) is
      use Ada.Text_IO;
                  
      Hashes : String_Map;
      Ignored : String_Set;
      Must_Ignore : Boolean := False;
   begin
      if Ignore_Path /= "" then
         Populate_Ignored_List(Ignore_Path, Ignored);
         Must_Ignore := True;
      end if;
      Loop_Rec(Folder_Path, Must_Ignore, Hashes, Ignored, Verbose_Mode);
      TUI.Display_TUI(Hashes);
      
      if Hashes.Is_Empty then
         Put_Line("No duplicates found.");
      end if;
   end Start_Searching;
   
end Core;

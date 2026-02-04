with Ada.Directories;
with Ada.Text_IO;

package body Ignorefile_Handler is

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
      Put_Line (File, "build");
      Put_Line (File, "dist");
      New_Line (File);
      Put_Line (File, "# other stuff");
      Put_Line (File, ".config");
      Put_Line (File, ".cache");
      Put_Line (File, ".local");
      
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
   

   function Generate_Custom_File(Dir : String; Home : String) return Boolean is
      use Ada.Text_IO;
      use Ada.Directories;

      File_Path : constant String := Dir & "/ignorefile";
      File : File_Type;
      Search : Search_Type;
      Dir_Ent : Directory_Entry_Type;
   begin
      Create(File => File, Mode => Out_File, Name => File_Path);

      Start_Search(Search, Directory => Home, Pattern => "*");
      while More_Entries(Search) loop
         Get_Next_Entry(Search, Dir_Ent);
         declare
            Name : constant String := Simple_Name(Dir_Ent);
         begin
            if Name'Length > 1 and then Name(Name'First) = '.' and then Name /= ".." then
               if Kind(Dir_Ent) = Directory then
                  Put_Line(File, Name);
               else
                  Put_Line(File, Name);
               end if;
            end if;
         end;
      end loop;
      End_Search(Search);

      Close(File);
      return True;
   end Generate_Custom_File;
   
end Ignorefile_Handler;

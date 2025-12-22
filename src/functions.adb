with Ada.Text_IO;

package body Functions is
   procedure Change_Color (C : Color_Code) is
      use Ada.Text_IO;
   begin
      case C is
         when Red =>
            Put(ASCII.ESC & "[31m");

         when Orange =>
            Put(ASCII.ESC & "[38;5;208m");

         when Green =>
            Put(ASCII.ESC & "[32m");

         when Blue =>
            Put(ASCII.ESC & "[34m");

         when others =>
            Put(ASCII.ESC & "[0m");
      end case;
   end Change_Color;


   procedure Display_Message (C : Color_Code; M : String) is
      use Ada.Text_IO;
   begin
      Change_Color(C);
      Put_Line(M);
      Change_Color(Reset);
   end Display_Message;


   procedure Display_Help is
      use Ada.Text_IO;
   begin
      Put_Line("Usage: dfm [OPTIONS]");
      Put_Line("Options:");
      Put_Line("  -h, --help          Displays help.");
      Put_Line("  -f, --folder PATH   Specify research folder.");
      Put_Line("  -i, --ignore PATH   Path to ignore file.");
      Put_Line("  -s, --sample PATH   Get an ignore file sample at chosen path.");
      Put_Line("  -v, --verbose       Enable verbose mode, showing all the files processed.");
   end Display_Help;


   procedure Display_Help_Extended is
      use Ada.Text_IO;
   begin
      Put_Line("Usage: dfm [OPTIONS]");
      Put_Line("Options:");
      Put_Line("  -h, --help          Displays help.");
      Put_Line("  -f, --folder PATH   Specify research folder. If no path is provided, default path is $HOME.");
      Put_Line("  -i, --ignore PATH   Path to ignore file.");
      Put_Line("                      Ignore file is a txt file that must contain each path, folder names or file names to be ignored during the research. The file must contain one (1) path or name per line.");
      Put_Line("  -s, --sample PATH   Get an ignore file sample at chosen path. If no path is provided, default path is $HOME.");
      Put_Line("  -v, --verbose       Enable verbose mode, showing all the files processed.");
   end Display_Help_Extended;

end Functions;

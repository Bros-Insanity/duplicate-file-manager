package Functions is
   type Color_Code is (Reset, Red, Orange, Blue, Green);
   
   procedure Change_Color(C : Color_Code);
   procedure Display_Message(C : Color_Code; M : String);
   
   procedure Display_Help;
   procedure Display_Help_Extended;
end Functions;

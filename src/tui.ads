with Core;

package TUI is
   
   procedure Display_TUI(Hashes : Core.String_Map);
   function Compute_Recovered_Size(Hashes : Core.String_Map) return Long_Long_Integer;
   function Format_Size(Size_In_Bytes : Long_Long_Integer) return String;
   
end TUI;

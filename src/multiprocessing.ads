with Core;

package Multiprocessing is
   
   procedure Initialize(Worker_Count : Positive);
   procedure Submit_File(File_Path : String);
   procedure Finalize(Hashes : in out Core.String_Map; Verbose_Mode : Boolean);
   procedure Shutdown;
   function Remaining_Files return Natural;
   
end Multiprocessing;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;

with Functions;

package body Multiprocessing is
   
   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);
   
   protected type File_Queue is
      entry Enqueue(File_Path : String);
      entry Dequeue(File_Path : out Unbounded_String; Success : out Boolean);
      procedure Set_Done;
      procedure Worker_Finished;
      function Is_Done return Boolean;
      function Remaining_Count return Natural;
   private
      Queue : String_Vectors.Vector;
      Done : Boolean := False;
      Active_Workers : Natural := 0;
   end File_Queue;
   
   protected body File_Queue is
      entry Enqueue(File_Path : String) when True is
      begin
         Queue.Append(File_Path);
      end Enqueue;
      
      entry Dequeue(File_Path : out Unbounded_String; Success : out Boolean) 
         when not Queue.Is_Empty or Done is
      begin
         if not Queue.Is_Empty then
            File_Path := To_Unbounded_String(Queue.First_Element);
            Queue.Delete_First;
            Success := True;
            Active_Workers := Active_Workers + 1;
         else
            Success := False;
         end if;
      end Dequeue;
      
      procedure Set_Done is
      begin
         Done := True;
      end Set_Done;
      
      procedure Worker_Finished is
      begin
         if Active_Workers > 0 then
            Active_Workers := Active_Workers - 1;
         end if;
      end Worker_Finished;
      
      function Is_Done return Boolean is
      begin
         return Done and Queue.Is_Empty and Active_Workers = 0;
      end Is_Done;
      
      function Remaining_Count return Natural is
      begin
         return Natural(Queue.Length) + Active_Workers;
      end Remaining_Count;

   end File_Queue;
   
   
   protected type Hash_Storage is
      procedure Add_Hash(File_Path : String; Hash : String);
      procedure Get_All_Hashes(Hashes : in out Core.String_Map);
   private
      Results : Core.String_Map;
   end Hash_Storage;
   
   protected body Hash_Storage is
      procedure Add_Hash(File_Path : String; Hash : String) is
         use Core.Path_Vectors;
         
         Hash_Key : constant Unbounded_String := To_Unbounded_String(Hash);
         Path : constant Unbounded_String := To_Unbounded_String(File_Path);
      begin
         if Results.Contains(Hash_Key) then
            declare
               Paths : Core.Path_Vector := Results.Element(Hash_Key);
            begin
               Paths.Append(Path);
               Results.Replace(Hash_Key, Paths);
            end;
         else
            declare
               Paths : Core.Path_Vector;
            begin
               Paths.Append(Path);
               Results.Insert(Hash_Key, Paths);
            end;
         end if;
      end Add_Hash;
      
      procedure Get_All_Hashes(Hashes : in out Core.String_Map) is
         use Core.String_Maps;
         
         procedure Copy_Entry(Position : Cursor) is
            Hash_Key : constant Unbounded_String := Key(Position);
            Paths : constant Core.Path_Vector := Element(Position);
         begin
            Hashes.Insert(Hash_Key, Paths);
         end Copy_Entry;
      begin
         Results.Iterate(Copy_Entry'Access);
      end Get_All_Hashes;
   end Hash_Storage;
   
   Queue_Instance : File_Queue;
   Storage_Instance : Hash_Storage;
   
   task type Worker_Task is
      entry Start(ID : Positive);
   end Worker_Task;
   
   task body Worker_Task is
      Worker_ID : Positive;
      File_Path : Unbounded_String;
      Success : Boolean;
   begin
      accept Start(ID : Positive) do
         Worker_ID := ID;
      end Start;
      
      loop
         Queue_Instance.Dequeue(File_Path, Success);
         
         exit when not Success and Queue_Instance.Is_Done;
         
         if Success then
            declare
               Path_Str : constant String := To_String(File_Path);
               Hash : constant String := Core.Compute_File_SHA256(Path_Str);
            begin
               Storage_Instance.Add_Hash(Path_Str, Hash);
            exception
               when others =>
                  Functions.Display_Message(Functions.Red, "Worker" & Worker_ID'Image & ": Error processing " & Path_Str);
            end;
            
            Queue_Instance.Worker_Finished;
         end if;
      end loop;
      
   exception
      when E : others =>
         Functions.Display_Message(Functions.Red, "Worker" & Worker_ID'Image & ": Fatal error");
         Functions.Display_Message(Functions.Red, Exception_Message(E));
   end Worker_Task;
   
   type Worker_Array is array (Positive range <>) of Worker_Task;
   type Worker_Array_Access is access Worker_Array;
   
   Workers : Worker_Array_Access;
   
   procedure Initialize(Worker_Count : Positive) is
   begin
      Workers := new Worker_Array(1..Worker_Count);
      for I in Workers'Range loop
         Workers(I).Start(I);
      end loop;
      
      Functions.Display_Message(Functions.Blue, "Initialized" & Worker_Count'Image & " worker threads");
   end Initialize;
   
   procedure Submit_File(File_Path : String) is
   begin
      Queue_Instance.Enqueue(File_Path);
   end Submit_File;
   
   
   procedure Set_Terminal_Echo(Enabled : Boolean) is
      use GNAT.OS_Lib;
      
      Stty_Path :  GNAT.OS_Lib.String_Access := Locate_Exec_On_Path("stty");
      Success : Boolean;
   begin
      if Stty_Path /= null then
         declare
            Args : Argument_List := (1 => new String'(if Enabled then "echo" else "-echo"));
         begin
            Spawn(Stty_Path.all, Args, Success);
            Free(Args(1));
         end;
         Free(Stty_Path);
      end if;
   exception
      when others =>
         null;
   end Set_Terminal_Echo;
   
   procedure Finalize(Hashes : in out Core.String_Map; Verbose_Mode : Boolean) is
      use Ada.Text_IO;
      
      Time_Since_Last_Report : Duration := 0.0;
      Report_Interval : constant Duration := 5.0;
      Poll_Interval : constant Duration := 0.1;
      Key : Character;
      Key_Available : Boolean;
      Keyboard_Readable : Boolean := True;
   begin
      Queue_Instance.Set_Done;
      
      Functions.Display_Message(Functions.Blue, "Waiting for workers to finish...");
      if not Verbose_Mode then
         Functions.Display_Message(Functions.Cyan, "Press R at any time to show the number of files remaining.");
         Set_Terminal_Echo(False);
      end if;
      
      while not Queue_Instance.Is_Done loop
         delay Poll_Interval;
         
         if Verbose_Mode then
            Time_Since_Last_Report := Time_Since_Last_Report + Poll_Interval;
            if Time_Since_Last_Report >= Report_Interval then
               Functions.Display_Message(Functions.Cyan,
                                         Natural'Image(Queue_Instance.Remaining_Count) & " file(s) remaining to process...");
               Time_Since_Last_Report := 0.0;
            end if;
         elsif Keyboard_Readable then
            begin
               Get_Immediate(Key, Key_Available);
            exception
               when others =>
                  Keyboard_Readable := False;
                  Key_Available := False;
            end;
            
            if Key_Available and then (Key = 'r' or Key = 'R') then
               Functions.Display_Message(Functions.Cyan,
                                         Natural'Image(Queue_Instance.Remaining_Count) & " file(s) remaining to process...");
            end if;
         end if;
      end loop;
      
      if not Verbose_Mode then
    	  Set_Terminal_Echo(True);
   	end if;

      
   	Storage_Instance.Get_All_Hashes(Hashes);
   	Functions.Display_Message(Functions.Green, "All files processed");
   exception
      when others =>
         if not Verbose_Mode then
            Set_Terminal_Echo(True);
         end if;
         raise;
   end Finalize;	

   
   procedure Shutdown is
   begin
      null;
   end Shutdown;
   
   
   function Remaining_Files return Natural is
   begin
      return Queue_Instance.Remaining_Count;
   end Remaining_Files;
   
end Multiprocessing;

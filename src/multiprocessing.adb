with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

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
   
   procedure Finalize(Hashes : in out Core.String_Map) is
   begin
      Queue_Instance.Set_Done;
      
      Functions.Display_Message(Functions.Blue, "Waiting for workers to finish...");
      while not Queue_Instance.Is_Done loop
         delay 0.1;
      end loop;
      
      Storage_Instance.Get_All_Hashes(Hashes);
      Functions.Display_Message(Functions.Green, "All files processed");
   end Finalize;
   
   procedure Shutdown is
   begin
      null;
   end Shutdown;
   
end Multiprocessing;

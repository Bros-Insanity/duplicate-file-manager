with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
   
package Core is
   
   package Path_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");
   
   use type Path_Vectors.Vector;
   
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Boolean,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   
   package String_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
         Element_Type    => Path_Vectors.Vector,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => Ada.Strings.Unbounded."=");
   
   subtype String_Map is String_Maps.Map;
   subtype String_Set is String_Sets.Map;
   subtype Path_Vector is Path_Vectors.Vector;
   
   type File_Size is range 0 .. 2**63 - 1;
   
   function Compute_File_SHA256(File_Path : String) return String;
   
   function Create_Sample(Dir : String) return Boolean;
   
   function Trim(S : String) return String;
   
   procedure Display_Duplicates(Hashes : String_Map);
   procedure Populate_Ignored_List(Ignore_File_Path : String; List_Of_Ignored : in out String_Set);
   
   procedure Start_Searching(Folder_Path : String; Ignore_Path : String; Verbose_Mode : Boolean);
end Core;

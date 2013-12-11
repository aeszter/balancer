with Ada.Containers.Doubly_Linked_Lists;
with SGE.Jobs; use SGE.Jobs;
with SGE.Partitions; use SGE.Partitions;

package Partitions is
   procedure Init;
   function CPU_Available (For_Job : Job; Mark_As_Used : Boolean) return Boolean;
   function GPU_Available (Mark_As_Used : Boolean) return Boolean;

   type Index_Card is record
      Nodes : Partition;
      Free_Hosts : Ada.Containers.Count_Type;
   end record;

   function New_Card (P : Partition) return Index_Card;

   package Catalogs is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Index_Card);
private
   Catalog : Catalogs.List;
end Partitions;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with SGE.Jobs; use SGE.Jobs;
with SGE.Partitions; use SGE.Partitions;

package Partitions is
   procedure Init;
   function CPU_Available (For_Job : Job; Mark_As_Used : Boolean) return Boolean;
   function GPU_Available (Mark_As_Used : Boolean) return Boolean;

   function Free_Slots return Natural;


   package Slot_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Positive,
                                                         Element_Type => Natural);
   subtype Slot_Map is Slot_Maps.Map;

   -- Key : number of free slots
   -- Element : how many hosts with exactly Key free slots

   type Index_Card is record
      Nodes : Partition;
      Free_Hosts : Ada.Containers.Count_Type;
      Free_Slots : Slot_Map;
   end record;

   procedure Search_Free_Slots (Where        : in out Index_Card;
                                Minimum      : Positive;
                                Mark_As_Used : Boolean;
                                Found        : out Boolean);
   --  Purpose:
   -- Check whether the given Partition represented by Where has
   -- at least one host with (at least) Minimum free slots.
   -- If Mark_As_Used, remove that host from the list of available slots;
   -- Although one could simply reduce the node from (free) to (free)-Minimum,
   -- the use of slot lists (of which we only use the minimum) means this is
   -- not worthwhile


   function New_Card (P : Partition) return Index_Card;

   package Catalogs is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Index_Card);
private
   Catalog : Catalogs.List;

end Partitions;

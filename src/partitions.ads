with Ada.Containers.Ordered_Maps;
with SGE.Jobs; use SGE.Jobs;
with SGE.Partitions; use SGE.Partitions;
with SGE.Host_Properties;

package Partitions is

   type Selection_Criteria is (Minimum, Maximum);
   procedure Init;
   function CPU_Available (For_Job      : Job;
                           Mark_As_Used : Boolean;
                           Fulfilling   : Selection_Criteria) return Boolean;
   function GPU_Available (Mark_As_Used : Boolean) return Boolean;

   function Free_Slots return Natural;


--   package Slot_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Positive,
--                                                         Element_Type => Natural);
--   subtype Slot_Map is Slot_Maps.Map;

   -- Key : number of free slots
   -- Element : how many hosts with exactly Key free slots

   type Index_Card is record
      Partition  : SGE.Host_Properties.Set_Of_Properties;
      Free_Hosts : Ada.Containers.Count_Type;
      Free_Slots : Natural;
   end record;

   function "<" (Left, Right : Index_Card) return Boolean;

   procedure Search_Free_Slots (Selector : not null access function (Card : Index_Card) return Boolean;
                                Mark_As_Used : Boolean;
                                Found        : out Boolean);
   --  Purpose:
   -- Check whether there is for which Selector returns true
   -- If Mark_As_Used, remove that host from the list of available slots;
   -- Although one could simply reduce the node from (free) to (free)-Minimum,
   -- the use of slot lists (of which we only use the minimum) means this is
   -- not worthwhile


   function New_Card (P : Partition; Free_Slots : Natural)
                      return Index_Card;

   package Catalogs is new Ada.Containers.Ordered_Maps
     (Key_Type => Index_Card,
     Element_Type => Natural);
private
   Catalog : Catalogs.Map;

end Partitions;

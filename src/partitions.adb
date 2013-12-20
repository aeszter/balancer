with SGE.Parser; use SGE.Parser;
with SGE.Queues;
with Parser;
with Utils;
with SGE.Host_Properties;

package body Partitions is
   use Partitions.Catalogs;
   use type Ada.Containers.Count_Type;


   function New_Card (P : Partition) return Index_Card is
      procedure Increment (Slot_Number : Positive; Count : in out Natural);
      procedure Copy (Position : SGE.Partitions.Countable_Maps.Cursor);
      Card : Index_Card;

   begin
      Card.Partition := P.Properties;
      Card.Free_Hosts := P.Available_Hosts.Length;
      Card.Free_Slots := Free_Slots;
      return Card;
   end New_Card;

   function "<" (Left, Right : Index_Card) return Boolean is
      use SGE.Host_Properties;
   begin
      if Left.Free_Slots < Right.Free_Slots then
         return True;
      elsif Left.Free_Slots > Right.Free_Slots then
         return False;
      else
         return Left.Partition < Right.Partition;
      end if;
   end "<";


   ----------
   -- Init --
   ----------

   procedure Init is
      procedure Copy (P : Partition);
      procedure Count (Position : Catalogs.Cursor);
      procedure Increment (Card : Index_Card; Count : in out Natural);
      SGE_Out : Tree;
      Total : Natural := 0;

      procedure Copy (P : Partition) is
         procedure Copy_Sub_Partition (Position : Countable_Maps.Cursor);

         procedure Copy_Sub_Partition (Position : Countable_Maps.Cursor) is
            Free_Slots : constant Natural := SGE.Partitions.Countable_Maps.Element (Position);
            Card : constant Index_Card := New_Card (P, Free_Slots);
            Existing : constant Cursor := Catalog.Find (Card);
         begin
            if Existing /= No_Element then
               Utils.Debug ("Sub partition exists: "
                      & SGE.Host_Properties.To_String (Card.Partition)
                      & "[" & Free_Slots'Img & "]");
               Catalog.Update_Element (Existing, Increment'Access);
            else
               Utils.Debug ("Inserting "
                      & SGE.Host_Properties.To_String (Card.Partition)
                      & "[" & Free_Slots'Img & "]");
               Catalog.Insert (Key => Card,
                               New_Item => 1);
            end if;
         end Copy_Sub_Partition;

      begin
         if P.Available_Slots.Is_Empty then
            return;
         end if;
         P.Available_Slots.Iterate (Copy_Sub_Partition'Access);
      end Copy;

      procedure Count (Position : Catalogs.Cursor) is
      begin
         Total := Total + Natural (Key (Position).Free_Hosts);
      end Count;

      procedure Increment (Card : Index_Card; Count : in out Natural) is
         pragma Unreferenced (Card);
      begin
         Count := Count + 1;
      end Increment;


   begin
      Utils.Debug ("--> Partitions.Init");
      SGE_Out := Setup (Selector => Parser.Resource_Selector & " -l h_rt=24:00:00");

      SGE.Queues.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
      SGE.Parser.Free;
      SGE.Partitions.Build_List;
      SGE.Partitions.Iterate (Copy'Access);
      Catalog.Iterate (Count'Access);

      Utils.Verbose_Message (Total'Img & " free nodes in" & Catalog.Length'Img & " partitions found");
      Utils.Verbose_Message ("Free slots: " & Free_Slots'Img);
      Utils.Debug ("<-- Partitions.Init");
   end Init;

   -------------------
   -- CPU_Available --
   -------------------

   function CPU_Available (For_Job : Job; Mark_As_Used : Boolean) return Boolean
   is
      function Selector (Card : Index_Card) return Boolean;

      Found : Boolean;

      function Selector (Card : Index_Card) return Boolean is
      begin
         if Card.Free_Slots >= Get_Minimum_Slots (For_Job) then
            return True;
         else
            return False;
         end if;
      end Selector;

   begin
      Search_Free_Slots (Selector => Selector'Access,
                         Mark_As_Used => Mark_As_Used,
                         Found        => Found);
      return Found;
   end CPU_Available;

   -------------------
   -- GPU_Available --
   -------------------
   -- Fixme: combine CPU_Available and GPU_Available into one unified Subprogram
   -- where only the partition suitability will be defined differently (via parameters)

   function GPU_Available (Mark_As_Used : Boolean) return Boolean is
      function Selector (Card : Index_Card) return Boolean;

      function Selector (Card : Index_Card) return Boolean is
      begin
         return SGE.Host_Properties.Has_GPU (Card.Partition);
      end Selector;

      Found : Boolean;

   begin
      Search_Free_Slots (Selector => Selector'Access,
                         Mark_As_Used => Mark_As_Used,
                         Found        => Found);
      return Found;
   end GPU_Available;


   function Free_Slots return Natural is
      procedure Tally (Position : Catalogs.Cursor);

      Total : Natural := 0;

      procedure Tally (Position : Catalogs.Cursor) is
         Card : constant Index_Card := Key (Position);
         Count : constant Natural := Element (Position);
         Local_Slots : constant Natural := Count * Card.Free_Slots;
      begin
         Utils.Debug ("Partition: " & SGE.Host_Properties.To_String (Card.Partition)
                      & Card.Free_Slots'Img & " =>" & Count'Img);
         Total := Total + Local_Slots;
      end Tally;

   begin
      Utils.Debug ("Free slots:");
      Catalog.Iterate (Tally'Access);
      return Total;
   end Free_Slots;

   procedure Search_Free_Slots (Selector : not null access function (Card : Index_Card) return Boolean;
                                Mark_As_Used : Boolean;
                                Found        : out Boolean) is
      procedure Decrement (Card : Index_Card; Count : in out Natural);

      procedure Decrement (Card : Index_Card; Count : in out Natural) is
         pragma Unreferenced (Card);
      begin
         Count := Count - 1;
      end Decrement;

      Position : Catalogs.Cursor := Catalog.First;

   begin
      while Position /= Catalogs.No_Element loop
         if Element (Position) > 0
           and then Selector (Key (Position)) then
            Found := True;
            if Mark_As_Used then
               Catalog.Update_Element (Position, Decrement'Access);
            end if;
            return;
         end if;
         Next (Position);
      end loop;
      Found := False;
   end Search_Free_Slots;


end Partitions;

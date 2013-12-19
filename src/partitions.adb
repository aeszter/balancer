with SGE.Parser; use SGE.Parser;
with SGE.Queues;
with Parser;
with Utils;

package body Partitions is
   use Partitions.Catalogs;
   use type Ada.Containers.Count_Type;

   function New_Card (P : Partition; Free_Slots : Natural; Node_Count : Positive)
                      return Index_Card is
      Card : Index_Card;

   begin
      Card.Nodes := P;
      Card.Free_Hosts := P.Available_Hosts.Length;
      Card.Free_Slots := Free_Slots;
      Card.Count := Node_Count;
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
         return Left.Nodes.Properties < Right.Nodes.Properties;
      end if;
   end "<";


   ----------
   -- Init --
   ----------

   procedure Init is
      SGE_Out : Tree;
      Total : Natural := 0;

      procedure Copy (P : Partition) is
         procedure Copy_Sub_Partition (Free_Slots : Natural; Count : Natural) is
         begin
            Catalog.Insert (New_Card (P, Free_Slots, Count));
         end Copy_Sub_Partition;

      begin
         if P.Available_Slots.Is_Empty then
            return;
         end if;
         P.Available_Slots.Iterate (Copy_Sub_Partition'Access);
      end Copy;

      procedure Count (Position : Catalogs.Cursor) is
      begin
         Total := Total + Natural (Element (Position).Free_Hosts);
      end Count;

   begin
      SGE_Out := Setup (Selector => Parser.Resource_Selector);

      SGE.Queues.Append_List (Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
      SGE.Parser.Free;
      SGE.Partitions.Build_List;
      SGE.Partitions.Iterate (Copy'Access);
      Catalog.Iterate (Count'Access);

      Utils.Verbose_Message (Total'Img & " free nodes in" & Catalog.Length'Img & " partitions found");
      Utils.Verbose_Message ("Free slots: " & Free_Slots'Img);
   end Init;

   -------------------
   -- CPU_Available --
   -------------------

   function CPU_Available (For_Job : Job; Mark_As_Used : Boolean) return Boolean
   is
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

   function GPU_Available (Mark_As_Used : Boolean) return Boolean is
      function Selector (Card : Index_Card) return Boolean is
      begin
         return Has_GPU (Card.Nodes);
      end Selector;

   begin
      Search_Free_Slots (Selector => Selector'Access,
                         Mark_As_Used => Mark_As_Used,
                         Found        => Found);
      return Found;
   end GPU_Available;


   function Free_Slots return Natural is
      Total : Natural := 0;

      procedure Sub_Tally (Position : Slot_Maps.Cursor) is
         Slot_Number : constant Natural := Slot_Maps.Key (Position);
         Count       : constant Natural := Slot_Maps.Element (Position);
      begin
         Utils.Debug (Slot_Number'Img & " =>" & Count'Img & ",", New_Line => False);
         Total := Total + Count * Slot_Number;
      end Sub_Tally;

      procedure Tally (Position : Catalogs.Cursor) is
         Card : constant Index_Card := Element (Position);
         Local_Slots : Natural := Card.Count * Card.Free_Slots;
      begin
         Utils.Debug ("Partition: " & To_String (Card.Nodes.Properties)
                      & Card.Free_Slots'Img & " =>" & Card.Count'Img);
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
      use Slot_Maps;

      procedure Decrement (Card : in out Index_Card) is
      begin
         Card.Count := Card.Count - 1;
      end Decrement;

      Position : Catalogs.Cursor := Catalog.First;

   begin
      while Position /= Catalogs.No_Element loop
         if Element (Position).Count >= 0
           and then Selector (Element (Position)) then
            Found := True;
            if Mark_As_Used then
               Catalog.Update_Element_Preserving_Key (Position, Decrement'Access);
            end if;
            return;
         end if;
      end loop;
      Found := False;
   end Search_Free_Slots;


end Partitions;

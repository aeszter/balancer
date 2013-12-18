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

      procedure Increment (Slot_Number : Positive; Count : in out Natural) is
         pragma Unreferenced (Slot_Number);
      begin
         Count := Count + 1;
      end Increment;

      procedure Copy (Position : SGE.Partitions.Countable_Maps.Cursor) is
         Slot_Number : constant Natural := SGE.Partitions.Countable_Maps.Element (Position);
         Slot_Position : Slot_Maps.Cursor;
      begin
         if Slot_Number = 0 then
            return;
         elsif
            Card.Free_Slots.Contains (Slot_Number) then
            Slot_Position := Card.Free_Slots.Find (Slot_Number);
            Card.Free_Slots.Update_Element (Slot_Position, Increment'Access);
         else
            Card.Free_Slots.Insert (Key      => Slot_Number,
                                    New_Item => 1);
         end if;
      end Copy;

   begin
      Card.Nodes := P;
      Card.Free_Hosts := P.Available_Hosts.Length;
      P.Available_Slots.Iterate (Copy'Access);
      return Card;
   end New_Card;

   ----------
   -- Init --
   ----------

   procedure Init is
      procedure Copy (P : Partition);
      procedure Count (Position : Catalogs.Cursor);
      SGE_Out : Tree;
      Total : Natural := 0;

      procedure Copy (P : Partition) is
      begin
         if P.Available_Slots.Is_Empty then
            return;
         end if;
         Catalog.Append (New_Card (P));
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
      Found    : Slot_Maps.Cursor;
      Position : Catalogs.Cursor := Catalog.First;
      Card     : Index_Card;

      procedure Decrement_Slots (Slot_Number : Positive; Count : in out Natural) is
         pragma Unreferenced (Slot_Number);
      begin
         Utils.Trace ("...and using one of" & Count'Img);
         Count := Count - 1;
      end Decrement_Slots;

      procedure Call_Decrement_Slots (Partition : in out Index_Card) is
      begin
         Slot_Maps.Update_Element (Container => Partition.Free_Slots,
                                   Position  => Found,
                                   Process   => Decrement_Slots'Access);
      end Call_Decrement_Slots;

      use Slot_Maps;

   begin
      while Position /= Catalogs.No_Element loop
         Card := Element (Position);
         Utils.Trace ("Trying " & SGE.Host_Properties.To_String (Card.Nodes.Properties) & "...");
         Search_Free_Slots (Where        => Card,
                            Minimum      => Get_Minimum_Slots (For_Job),
                            Found        => Found);
         if Found /= Slot_Maps.No_Element then
            if Mark_As_Used then
               Catalog.Update_Element (Position => Position,
                                       Process  => Call_Decrement_Slots'Access);
            end if;
            return True;
         end if;
         Utils.Trace ("...already full");
         Next (Position);
      end loop;
      return False;
   end CPU_Available;

   -------------------
   -- GPU_Available --
   -------------------
   -- Fixme: combine CPU_Available and GPU_Available into one unified Subprogram
   -- where only the partition suitability will be defined differently (via parameters)

   function GPU_Available (Mark_As_Used : Boolean) return Boolean is
      Found    : Slot_Maps.Cursor;
      Position : Catalogs.Cursor := Catalog.First;
      Card     : Index_Card;

      procedure Decrement_Slots (Slot_Number : Positive; Count : in out Natural) is
         pragma Unreferenced (Slot_Number);
      begin
         Utils.Trace ("...and using one of" & Count'Img);
         Count := Count - 1;
      end Decrement_Slots;

      procedure Call_Decrement_Slots (Partition : in out Index_Card) is
      begin
         Slot_Maps.Update_Element (Container => Partition.Free_Slots,
                                   Position  => Found,
                                   Process   => Decrement_Slots'Access);
      end Call_Decrement_Slots;

      use Slot_Maps;

   begin
      while Position /= Catalogs.No_Element loop
         Card := Element (Position);
         Utils.Trace ("Trying " & SGE.Host_Properties.To_String (Card.Nodes.Properties) & "...");
         if Has_GPU (Card.Nodes) then
            Utils.Trace ("  Heuristic: use 4 slots for GPU job");
            Search_Free_Slots (Where        => Card,
                               Minimum      => 4,
                               Found        => Found);
            -- FIXME: Minimum => 4 is a heuristic value
            -- Maybe use job characteristics here?
            if Found /= Slot_Maps.No_Element then
               if Mark_As_Used then
                  Catalog.Update_Element (Position => Position,
                                          Process  => Call_Decrement_Slots'Access);
               end if;
               return True;
            end if;
         else
            Utils.Trace ("... no GPU");
         end if;
         Utils.Trace ("...already full");
         Next (Position);
      end loop;
      return False;
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
      begin
         Utils.Debug ("Partition:", New_Line => False);
         Element (Position).Free_Slots.Iterate (Sub_Tally'Access);
         Utils.Debug ("");
      end Tally;

   begin
      Utils.Debug ("Free slots:");
      Catalog.Iterate (Tally'Access);
      return Total;
   end Free_Slots;

   procedure Search_Free_Slots (Where        : Index_Card;
                         Minimum      : Positive;
                         Found        : out Slot_Maps.Cursor) is
      use Slot_Maps;

      Position : constant Slot_Maps.Cursor := Where.Free_Slots.Ceiling (Minimum);

   begin
      if Position = Slot_Maps.No_Element
        or else Element (Position) = 0 then
         Found := Slot_Maps.No_Element;
         return;
      else
         Utils.Trace ("Found" & Key (Position)'Img & ">=" & Minimum'Img & " free slots");
         Found := Position;
         return;
      end if;
   end Search_Free_Slots;


end Partitions;

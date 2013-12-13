with SGE.Parser; use SGE.Parser;
with SGE.Queues;
with Parser;
with Utils;

package body Partitions is
   use Partitions.Catalogs;
   use type Ada.Containers.Count_Type;

   function New_Card (P : Partition) return Index_Card is
      Card : Index_Card;
   begin
      Card.Nodes := P;
      Card.Free_Hosts := P.Available_Hosts.Length;
      return Card;
   end New_Card;

   ----------
   -- Init --
   ----------

   procedure Init is
      SGE_Out : Tree;
      Total : Natural := 0;

      procedure Copy (P : Partition) is
      begin
         if P.Available_Hosts.Is_Empty then
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
   end Init;

   -------------------
   -- CPU_Available --
   -------------------

   function CPU_Available (For_Job : Job; Mark_As_Used : Boolean) return Boolean
   is
      Found : Boolean := False;
      Position : Catalogs.Cursor := Catalog.First;
      Card : Index_Card;
   begin
      while Position /= Catalogs.No_Element loop
         Card := Element (Position);
         if Get_Cores (Card.Nodes) >= Get_Minimum_Slots (For_Job) then
            Found := True;
            if Mark_As_Used then
               Card.Free_Hosts := Card.Free_Hosts - 1;
            end if;
            exit;
         end if;
         Next (Position);
      end loop;
      return Found;
   end CPU_Available;

   -------------------
   -- GPU_Available --
   -------------------

   function GPU_Available (Mark_As_Used : Boolean) return Boolean is
      Found : Boolean := False;
      Position : Catalogs.Cursor := Catalog.First;
      Card : Index_Card;
   begin
      while Position /= Catalogs.No_Element loop
         Card := Element (Position);
         if Has_GPU (Card.Nodes) then
            Found := True;
            if Mark_As_Used then
               Card.Free_Hosts := Card.Free_Hosts - 1;
            end if;
            exit;
         end if;
         Next (Position);
      end loop;
      return Found;
   end GPU_Available;

end Partitions;

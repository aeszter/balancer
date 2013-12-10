with SGE.Jobs; use SGE.Jobs;
with SGE.Parser;
with SGE.Resources;
with Partitions;
with Resources;
with Statistics;
with Parser;
with Utils;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;



package body Jobs is

   function Comma_Convert (Encoded_String : String) return String;

   ----------
   -- Init --
   ----------

   procedure Init is
      SGE_Out : SGE.Parser.Tree;
   begin
      SGE_Out := SGE.Parser.Setup (Selector => "-u * -s p");
      Append_List (SGE.Parser.Get_Job_Nodes_From_Qstat_U (SGE_Out));
      SGE.Parser.Free;
      SGE_Out := SGE.Parser.Setup (Selector => "-j *");
      Create_Overlay (SGE.Parser.Get_Job_Nodes_From_Qstat_J (SGE_Out));
      Apply_Overlay;
      SGE.Parser.Free;
      Utils.Verbose_Message (SGE.Jobs.Count'Img & " pending jobs");
      SGE.Jobs.Prune_List (Keep => Is_Eligible'Access);
      SGE.Jobs.Iterate (Parser.Add_Pending_Since'Access);
      Utils.Verbose_Message (SGE.Jobs.Count'Img & " eligible for re-queueing");
   end Init;


   -------------
   -- Balance --
   -------------

   procedure Balance_One_Job (J : Job) is
   begin
      if Queued_For_CPU (J)
        and then Partitions.GPU_Available (Mark_As_Used => True) then
         Migrate_To_GPU (J);
         Statistics.To_GPU;
      elsif Queued_For_GPU (J)
        and then Partitions.CPU_Available (For_Job => J, Mark_As_Used => True) then
         Migrate_To_CPU (J);
         Statistics.To_CPU;
      else
         Utils.Verbose_Message ("Job" & Get_ID (J) & " queued for neither CPU nor GPU");
         Statistics.Aimless_Job;
      end if;
   exception
      when E : Parser.Security_Error =>
         Ada.Text_IO.Put_Line (Exception_Message (E) & " while processing job" & Get_ID (J));
      when E : SGE.Parser.Parser_Error =>
         Ada.Text_IO.Put_Line (Exception_Message (E) & " while processing job" & Get_ID (J));
      when E : Support_Error =>
         Ada.Text_IO.Put_Line ("Job" & Get_ID (J) & " unexpectedly lacks Balancer support: "
                               & Exception_Message (E));
      when E : others =>
         Ada.Text_IO.Put_Line ("unexcpected error in job" & Get_ID (J) & ": "
                               & Exception_Message (E));
   end Balance_One_Job;

   procedure Balance is
   begin
      SGE.Jobs.Iterate (Balance_One_Job'Access);
   end Balance;

   function Is_Eligible (J : Job) return Boolean is
   begin
      return Supports_Balancer (J);
   end Is_Eligible;

   function Queued_For_CPU (J : Job) return Boolean is
      Res : constant SGE.Resources.Hashed_List := SGE.Jobs.Get_Hard_Resources (J);
   begin
      return not Res.Contains (To_Unbounded_String ("gpu"));
   end Queued_For_CPU;

   function Queued_For_GPU (J : Job) return Boolean is
      Res : constant SGE.Resources.Hashed_List := SGE.Jobs.Get_Hard_Resources (J);
   begin
      return Res.Contains (To_Unbounded_String ("gpu"));
   end Queued_For_GPU;

   procedure Migrate_To_CPU (J : Job) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
   begin
      New_Resources.Delete (Key => To_Unbounded_String ("gpu"));
      Parser.Alter_Job (Job                => J,
                        Insecure_Resources => SGE.Resources.To_String (New_Resources),
                        Slots              => Comma_Convert (
                          Get_Context (J => J, Key => "SLOTSCPU")));
   end Migrate_To_CPU;

   procedure Migrate_To_GPU (J : Job) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
   begin
      Resources.Add (New_Resources, Name => "gpu", Value => "1");
      Parser.Alter_Job (Job                => J,
                        Insecure_Resources => Resources.To_Requirement (New_Resources),
                        Slots               => Comma_Convert (
                          Get_Context (J => J, Key => "SLOTSGPU")));
   end Migrate_To_GPU;

   function Comma_Convert (Encoded_String : String) return String is
      package Str renames Ada.Strings;

      Conversion : constant Str.Maps.Character_Mapping :=
                     Str.Maps.To_Mapping (From => ";",
                                          To   => ",");
   begin
      return Str.Fixed.Translate (Source  => Encoded_String,
                                  Mapping => Conversion);
   end Comma_Convert;

end Jobs;

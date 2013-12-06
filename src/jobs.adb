with SGE.Jobs; use SGE.Jobs;
with SGE.Parser;
with Partitions;
with Statistics;
with Utils;



package body Jobs is
   package Parser renames SGE.Parser;

   ----------
   -- Init --
   ----------

   procedure Init is
      SGE_Out : Parser.Tree;
   begin
      SGE_Out := Parser.Setup (Selector => "-u * -s p");
      Append_List (Parser.Get_Job_Nodes_From_Qstat_U (SGE_Out));
      Parser.Free;
      SGE_Out := Parser.Setup (Selector => "-j *");
      Create_Overlay (Parser.Get_Job_Nodes_From_Qstat_J (SGE_Out));
      Apply_Overlay;
      Parser.Free;
      Utils.Verbose_Message (SGE.Jobs.Count'Img & " pending jobs");
      SGE.Jobs.Iterate (Add_Pending_Since'Access);
      SGE.Jobs.Prune_List (Keep => Is_Eligible'Access);
      Utils.Verbose_Message (SGE.Jobs.Count'Img & " eligible for re-queueing");
   end Init;


   -------------
   -- Balance --
   -------------

   procedure Balance_One_Job (J : Job) is
   begin
      if Queued_For_CPU (J) and then
        Partitions.GPU_Available (Mark_As_Used => True) then
         Migrate_To_GPU (J);
         Statistics.To_GPU;
      elsif Queued_For_GPU (J) and then
        Partitions.CPU_Available (For_Job => J, Mark_As_Used => True) then
         Migrate_To_CPU (J);
         Statistics.To_CPU;
      else
         Utils.Verbose_Message ("Job" & Get_ID (J) & " queued for neither CPU nor GPU");
         Statistics.Aimless_Job;
      end if;
   end Balance_One_Job;

   procedure Balance is
   begin
      SGE.Jobs.Iterate (Balance_One_Job'Access);
   end Balance;

   procedure Add_Pending_Since (J : Job) is
   begin
      --  if (! "PENDINGSINCE" in slots) system ("qalter " id " -ac PENDINGSINCE=`date +%s` >>/tmp/qalter 2>&1");
      pragma Compile_Time_Warning (True, "unimplemented");
      raise Program_Error with "Add_Pending_Since unimplemented";
   end Add_Pending_Since;

   function Is_Eligible (J : Job) return Boolean is
   begin
      return Supports_Balancer (J);
   end Is_Eligible;

   function Queued_For_CPU (J : Job) return Boolean is
   begin
      pragma Compile_Time_Warning (True, "unimplemented");
      raise Program_Error with "Queued_For_CPU unimplemented";
      -- look for -l gpu=...
      return False;
   end Queued_For_CPU;

   function Queued_For_GPU (J : Job) return Boolean is
   begin
      pragma Compile_Time_Warning (True, "unimplemented");
      raise Program_Error with "Queued_For_GPU unimplemented";
      -- look for -l gpu=...
      return False;
   end Queued_For_GPU;

   procedure Migrate_To_CPU (J : Job) is
   begin
      pragma Compile_Time_Warning (True, "unimplemented");
      raise Program_Error with "Migrate_To_CPU unimplemented";
      --  system("qalter " $1 " -l " $3 " -pe " $5 " " slots["SLOTSCPU"] " -ac LASTMIG=`date +%s` >/dev/null");
   end Migrate_To_CPU;

   procedure Migrate_To_GPU (J : Job) is
   begin
      pragma Compile_Time_Warning (True, "unimplemented");
      raise Program_Error with "Migrate_To_GPU unimplemented";
         --  system("qalter " $1 " -l " $3 ",gpu=1 -pe " $5 " " slots["SLOTSGPU"] " -ac LASTMIG=`date +%s` >/dev/null");

   end Migrate_To_GPU;
end Jobs;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces.C;
with SGE.Jobs; use SGE.Jobs;
with SGE.Parser;
with SGE.Resources;
with Partitions;
with Resources;
with Statistics;
with Parser;
with Users;
with Utils;



package body Jobs is

   function Comma_Convert (Encoded_String : String) return String;
   procedure Balance_CPU_GPU (J : Job);
   procedure Extend_Slots_Below (J : Job);
   procedure Extend_Slots_Above (Position : Job_Lists.Cursor);

   ----------
   -- Init --
   ----------

   procedure Init is
      SGE_Out : SGE.Parser.Tree;
   begin
      SGE_Out := SGE.Parser.Setup (Selector => "-u * -r -s p");
      Append_List (SGE.Parser.Get_Job_Nodes_From_Qstat_U (SGE_Out));
      SGE.Parser.Free;
      SGE_Out := SGE.Parser.Setup (Selector => "-j *");
      Create_Overlay (SGE.Parser.Get_Job_Nodes_From_Qstat_J (SGE_Out));
      Apply_Overlay;
      SGE.Parser.Free;
      Utils.Verbose_Message (SGE.Jobs.Count'Img & " pending jobs");
      SGE.Jobs.Prune_List (Keep => Is_Eligible'Access);
      SGE.Jobs.Iterate (Parser.Add_Pending_Since'Access);
      SGE.Jobs.Iterate (Users.Add_Job'Access);
      SGE.Jobs.Iterate (Add_Chain_Head'Access);
      Utils.Verbose_Message (SGE.Jobs.Count'Img
                             & " by" & Users.Total_Users'Img
                             & " users eligible for re-queueing");
      Utils.Verbose_Message (Chain_Heads.Length'Img & " chain heads found");
   end Init;

   procedure Add_Chain_Head (J : Job) is
      procedure Test_Hold (ID : Natural);

      Any_Held : Boolean := False;

      procedure Test_Hold (ID : Natural) is
      begin
         if On_Hold (Find_Job (ID)) then
            Any_Held := True;
         end if;
      exception
         when Constraint_Error =>
            --  job not listed, i.e. not pending
            null;
      end Test_Hold;

   begin
      if not On_Hold (J) then
         return;
      end if;
      Iterate_Predecessors (J, Test_Hold'Access);
      if Any_Held then
         return;
      end if;
      Chain_Heads.Append (J);
      Utils.Trace ("Found chain head " & Get_ID (J));
   end Add_Chain_Head;

   -------------
   -- Balance --
   -------------

   procedure Extend_Slots_Below (J : Job) is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;
   begin
      if On_Hold (J) then
         return;
      end if;
      Utils.Trace ("Looking at " & Get_Owner (J)
                   & "'s job " & Get_ID (J));
      if not Supports_Balancer (J, Low_Cores) then
         Utils.Trace ("Low_Cores not supported");
         return;
      end if;

      if Queued_For_CPU (J)
        and then not Partitions.CPU_Available (J, Mark_As_Used => False,
                                              Fulfilling => Partitions.Minimum) then
         declare
            Threshold     : constant Duration := Duration'Value (
                            Get_Context (J   => J,
                                         Key => "WAITREDUCE"));
            Slot_Range    : constant String := Comma_Convert (
                            Get_Context (J   => J,
                                         Key => "SLOTSREDUCE"));
            Pending_Since : constant Time := To_Ada_Time (Interfaces.C.long'Value (
                            Get_Context (J   => J,
                                         Key => "PENDINGSINCE")));
            Runtime       : constant String := Get_Context (J   => J,
                                                            Key => "RTREDUCE");
         begin
            if Has_Context (J, "LASTRED") then
               declare
                  Last_Reduction : constant Time := Get_Last_Reduction (J);
                  -- will not raise an exception since Has_Context ("LASTRED") is true
                  Last_Migration : constant Time := Get_Last_Migration (J);
               begin
                  if Last_Reduction > Last_Migration then
                     Utils.Trace ("already reduced");
                     return;
                  end if;
               exception
                  when Constraint_Error =>
                     Utils.Trace ("already reduced");
                     return;
               end;
            end if;
            if Clock > Pending_Since + Threshold then
               Reduce_Slots (J, Slot_Range, Runtime);
               Statistics.Reduce_Range;
            else
               Utils.Trace ("too recent");
            end if;
         end;
      else
         Utils.Trace ("not queued for CPU, or free CPUs found");
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
         Ada.Text_IO.Put_Line ("unexpected error in job " & Get_ID (J) & ": "
                               & Exception_Message (E));
   end Extend_Slots_Below;

   procedure Extend_Slots_Above (Position : Job_Lists.Cursor) is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;
      J    : constant Job := Job_Lists.Element (Position);
      User : constant String := Get_Owner (J);

   begin
      Utils.Trace ("Looking at " & User & "'s job " & Get_ID (J));
      if not Supports_Balancer (J, High_Cores) then
         Utils.Trace ("High_Cores not supported");
         return;
      end if;

      if Users.Count_Jobs (For_User => User) < Max_Pending_On_Underutilisation then
         declare
            Slot_Range : constant String := Comma_Convert (
                            Get_Context (J   => J,
                                         Key => "SLOTSEXTEND"));
         begin
            if Has_Context (J, "LASTEXT") then
               Utils.Trace ("already extended");
               return;
            end if;
            if Partitions.CPU_Available (For_Job      => J,
                                         Mark_As_Used => False,
                                         Fulfilling => Partitions.Maximum) then
               Extend_Slots (J, Slot_Range);
               Statistics.Extend_Range;
            else
               Utils.Trace ("too few slots free");
            end if;
         end;
      else
         Utils.Trace ("user has too many qw jobs");
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
         Ada.Text_IO.Put_Line ("unexpected error in job " & Get_ID (J) & ": "
                               & Exception_Message (E));
   end Extend_Slots_Above;

   procedure Balance_CPU_GPU (J : Job) is
   begin
      if On_Hold (J) then
         return;
      end if;
      Utils.Trace ("Looking at " & Get_Owner (J)
                   & "'s job " & Get_ID (J));
      if not Supports_Balancer (J, CPU_GPU) then
         Utils.Trace ("CPU_GPU not supported");
         return;
      end if;

      if Queued_For_CPU (J) then
         if Partitions.GPU_Available (Mark_As_Used => True) then
            Migrate_To_GPU (J);
            Statistics.To_GPU;
         else
            Statistics.No_GPU;
         end if;
      elsif Queued_For_GPU (J) then
         if Partitions.CPU_Available (For_Job      => J,
                                      Mark_As_Used => True,
                                      Fulfilling => Partitions.Minimum) then
            Migrate_To_CPU (J);
            Statistics.To_CPU;
         else
            Statistics.No_CPU;
         end if;
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
         Ada.Text_IO.Put_Line ("unexpected error in job " & Get_ID (J) & ": "
                               & Exception_Message (E));
   end Balance_CPU_GPU;

   procedure Balance is
   begin
      Utils.Trace ("Extending slot ranges to fewer cores");
      Users.Iterate (Extend_Slots_Below'Access);
      Utils.Trace ("Shifting jobs between CPU and GPU");
      Users.Iterate (Balance_CPU_GPU'Access);
      Utils.Trace ("Extending slot ranges to more cores");
      Chain_Heads.Iterate (Extend_Slots_Above'Access);
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
                        Insecure_Resources => Resources.To_Requirement (New_Resources),
                        Slots              => Comma_Convert (
                          Get_Context (J => J, Key => "SLOTSCPU")),
                       Timestamp_Name => "LASTMIG");
   end Migrate_To_CPU;

   procedure Migrate_To_GPU (J : Job) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
   begin
      Resources.Add (New_Resources, Name => "gpu", Value => "1");
      Parser.Alter_Job (Job                => J,
                        Insecure_Resources => Resources.To_Requirement (New_Resources),
                        Slots               => Comma_Convert (
                          Get_Context (J => J, Key => "SLOTSGPU")),
                       Timestamp_Name => "LASTMIG");
   end Migrate_To_GPU;

   procedure Reduce_Slots (J : Job; To : String; Runtime : String) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
   begin
      if Runtime /= "" then
         New_Resources.Delete (Key => To_Unbounded_String ("h_rt"));
         Resources.Add (To => New_Resources, Name => "h_rt", Value => Runtime);
      end if;
      Parser.Alter_Job (Job                => J,
                        Insecure_Resources => Resources.To_Requirement (New_Resources),
                        Slots              => To,
                       Timestamp_Name => "LASTRED");
   end Reduce_Slots;

   procedure Extend_Slots (J : Job; To : String) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
   begin
      New_Resources.Delete (Key => To_Unbounded_String ("gpu"));
      Parser.Alter_Job (Job                => J,
                        Insecure_Resources => Resources.To_Requirement (New_Resources),
                        Slots              => To,
                       Timestamp_Name => "LASTEXT");
   end Extend_Slots;

   function Comma_Convert (Encoded_String : String) return String is
      package Str renames Ada.Strings;

      Conversion : constant Str.Maps.Character_Mapping :=
                     Str.Maps.To_Mapping (From => ";",
                                          To   => ",");
   begin
      return Str.Fixed.Translate (Source  => Encoded_String,
                                  Mapping => Conversion);
   end Comma_Convert;

   function Equal_Jobs (Left, Right : Job) return Boolean is
   begin
      return Get_ID (Left) = Get_ID (Right);
   end Equal_Jobs;

end Jobs;

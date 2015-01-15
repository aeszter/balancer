with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces.C;
with SGE.Jobs; use SGE.Jobs;
with SGE.Parser;
with SGE.Utils; use SGE.Utils;
with Partitions;
with Resources;
with Statistics;
with Parser;
with Users;
with Utils;
with SGE.Quota;
with SGE.Context;
with Sanitiser;



package body Jobs is
   Chain_Count : Natural;

   procedure Balance_CPU_GPU (J : Job);
   function Comma_Convert (Encoded_String : String) return String;
--   procedure Extend_Slots_Above (Position : Job_Lists.Cursor);
--   procedure Extend_Slots_Below (J : Job);
   procedure Apply_Changes (Position : Changed_Lists.Cursor);
   function Timestamp (J : Changed_Job) return String;

   procedure Add_Resource (J : in out Changed_Job; Res : String) is
      Separator : constant Natural := Ada.Strings.Fixed.Index (Res, "=");
      Name      : constant String := Res (Res'First .. Separator - 1);
      Value     : constant String := Res (Separator + 1 .. Res'Last);
   begin
      Resources.Add (To => J.Resources, Name => Name, Value => Value);
   end Add_Resource;

   procedure Apply_Changes (Position : Changed_Lists.Cursor) is
      J : constant Changed_Job := Changed_Lists.Element (Position);
   begin
      Parser.Alter_Job (ID => J.ID,
                        Insecure_Resources => Resources.To_Requirement (J.Resources),
                        Slots              => SGE.Ranges.To_SGE_Input (J.Slots),
                        PE                 => To_String (J.PE),
                        Timestamp_Name => Timestamp (J));
   end Apply_Changes;

   procedure Apply_Recorded_Changes is
   begin
      Utils.Trace ("Applying changes to" & Modified.Length'Img & " jobs");
      Modified.Iterate (Apply_Changes'Access);
   end Apply_Recorded_Changes;

   procedure Apply_Rules_Only (J : Job) is
      Item : Changed_Job := Init (ID => Get_ID (J), New_State => undefined, Old_State => undefined);
   begin
      if Queued_For_GPU (J) then
         Item.Old_State := gpu;
         Item.New_State := gpu;
      else
         Item.Old_State := cpu;
         Item.New_State := cpu;
      end if;
      Item.Resources := Get_Hard_Resources (J);
      Set_Slots (Item, Get_Slot_List (J));
      Set_PE (Item, Get_PE (J));
      Sanitiser.Apply_Rules (Item);
      Modified.Append (Item);
   end Apply_Rules_Only;


--     procedure Add_Chain_Head (J : Job) is
--        procedure Test_Hold (ID : Natural);
--
--        Any_Held : Boolean := False;
--
--        procedure Test_Hold (ID : Natural) is
--        begin
--           if On_Hold (Find_Job (ID)) then
--              Any_Held := True;
--           end if;
--        exception
--           when Constraint_Error =>
--              --  job not listed, i.e. not pending
--              null;
--        end Test_Hold;
--
--     begin
--        if not On_Hold (J) then
--           return;
--        end if;
--        Iterate_Predecessors (J, Test_Hold'Access);
--        if Any_Held then
--           return;
--        end if;
--        Chain_Count := Chain_Count + 1;
--        if not Supports_Balancer (J, High_Cores) then
--           return;
--        end if;
--        Chain_Heads.Append (J);
--        Utils.Trace ("Found chain head " & Get_ID (J));
--     end Add_Chain_Head;

   procedure Balance is
   begin
--      Utils.Trace ("Extending slot ranges to fewer cores");
--      Users.Iterate (Extend_Slots_Below'Access);
      Utils.Trace ("Shifting jobs between CPU and GPU");
      Users.Iterate (Balance_CPU_GPU'Access);
--      Utils.Trace ("Extending slot ranges to more cores");
--      Chain_Heads.Iterate (Extend_Slots_Above'Access);
      Apply_Recorded_Changes;
   end Balance;

   procedure Balance_CPU_GPU (J : Job) is
      use Ada.Calendar;
      use Ada.Calendar.Conversions;

      Pending_Since : Time;
      Threshold     : constant Duration := Duration (1_200); -- seconds

   begin
      if On_Hold (J) then
         return;
      end if;
      if Quota_Inhibited (J) then
         Statistics.Quota_Inhibited (Get_ID (J));
         return;
      end if;
      Utils.Trace ("Looking at " & To_String (Get_Owner (J))
                   & "'s job " & Get_ID (J));
      if not Supports_Balancer (J, CPU_GPU) then
         Utils.Trace ("CPU_GPU not supported");
         return;
      end if;
      if Has_Context (J, SGE.Context.Pending_Since) then
         Pending_Since := To_Ada_Time (Interfaces.C.long'Value (
                          Get_Context (J   => J,
                                       Key => SGE.Context.Pending_Since)));
      else
         Pending_Since := Clock;
      end if;

      if Clock < Pending_Since + Threshold then
         Utils.Trace ("too recent");
         Statistics.Recent_Job;
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
      return Positive'(Get_ID (Left)) = Get_ID (Right);
   end Equal_Jobs;

   function Equal_Jobs (Left, Right : Changed_Job) return Boolean is
   begin
      return Left.ID = Right.ID;
   end Equal_Jobs;

--     procedure Extend_Slots (J : Job; To : String) is
--        New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
--        Item : Changed_Job := (ID => Get_ID (J), New_State => extend, Slots => To, Timestamp => "LASTEXT");
--     begin
--        New_Resources.Delete (Key => To_Unbounded_String ("gpu"));
--        Item.Resources := Resources.To_Requirement (New_Resources);
--        Modified.Append (Item);
--     end Extend_Slots;

--     procedure Extend_Slots_Above (Position : Job_Lists.Cursor) is
--        use Ada.Calendar;
--        use Ada.Calendar.Conversions;
--        J    : constant Job := Job_Lists.Element (Position);
--        User : constant String := To_String (Get_Owner (J));
--
--     begin
--        if Quota_Inhibited (J) then
--           Statistics.Quota_Inhibited (Get_ID (J));
--           return;
--        end if;
--        Utils.Trace ("Looking at " & User & "'s job " & Get_ID (J));
--        if not Supports_Balancer (J, High_Cores) then
--           Utils.Trace ("High_Cores not supported");
--           return;
--        end if;
--
--        if Users.Count_Jobs (For_User => User) < Max_Pending_On_Underutilisation then
--           declare
--              Slot_Range : constant String := Comma_Convert (
--                              Get_Context (J   => J,
--                                           Key => SGE.Context.Slots_Extend));
--           begin
--              if Has_Context (J, SGE.Context.Last_Extension) then
--                 Utils.Trace ("already extended");
--                 return;
--              end if;
--              if Partitions.CPU_Available (For_Job      => J,
--                                           Mark_As_Used => False,
--                                           Fulfilling => Partitions.Maximum) then
--                 Extend_Slots (J, Slot_Range);
--                 Statistics.Extend_Range;
--              else
--                 Utils.Trace ("too few slots free");
--              end if;
--           end;
--        else
--           Utils.Trace ("user has too many qw jobs");
--        end if;
--     exception
--        when E : Parser.Security_Error =>
--           Ada.Text_IO.Put_Line (Exception_Message (E) & " while processing job" & Get_ID (J));
--        when E : SGE.Parser.Parser_Error =>
--           Ada.Text_IO.Put_Line (Exception_Message (E) & " while processing job" & Get_ID (J));
--        when E : Support_Error =>
--           Ada.Text_IO.Put_Line ("Job" & Get_ID (J) & " unexpectedly lacks Balancer support: "
--                                 & Exception_Message (E));
--        when E : others =>
--           Ada.Text_IO.Put_Line ("unexpected error in job " & Get_ID (J) & ": "
--                                 & Exception_Message (E));
--     end Extend_Slots_Above;
--
--     procedure Extend_Slots_Below (J : Job) is
--        use Ada.Calendar;
--        use Ada.Calendar.Conversions;
--     begin
--        if On_Hold (J) then
--           return;
--        end if;
--        if Quota_Inhibited (J) then
--           Statistics.Quota_Inhibited (Positive'(Get_ID (J)));
--           return;
--        end if;
--        Utils.Trace ("Looking at " & To_String (Get_Owner (J))
--                     & "'s job " & Get_ID (J));
--        if not Supports_Balancer (J, Low_Cores) then
--           Utils.Trace ("Low_Cores not supported");
--           return;
--        end if;
--
--        if Queued_For_CPU (J)
--          and then not Partitions.CPU_Available (J, Mark_As_Used => False,
--                                                Fulfilling => Partitions.Minimum) then
--           declare
--              Threshold     : constant Duration := Duration'Value (
--                              Get_Context (J   => J,
--                                           Key => SGE.Context.Wait_Reduce));
--              Slot_Range    : constant String := Comma_Convert (
--                              Get_Context (J   => J,
--                                           Key => SGE.Context.Slots_Reduce));
--              Pending_Since : constant Time := To_Ada_Time (Interfaces.C.long'Value (
--                              Get_Context (J   => J,
--                                           Key => SGE.Context.Pending_Since)));
--              Runtime       : constant String := Get_Context (J   => J,
--                                                              Key => SGE.Context.Reduced_Runtime);
--           begin
--              if Has_Context (J, SGE.Context.Last_Reduction) then
--                 declare
--                    Last_Reduction : constant Time := Get_Last_Reduction (J);
--                    -- will not raise an exception since Has_Context ("LASTRED") is true
--                    Last_Migration : constant Time := Get_Last_Migration (J);
--                 begin
--                    if Last_Reduction > Last_Migration then
--                       Utils.Trace ("already reduced");
--                       return;
--                    end if;
--                 exception
--                    when Constraint_Error =>
--                       Utils.Trace ("already reduced");
--                       return;
--                 end;
--              end if;
--              if Clock > Pending_Since + Threshold then
--                 Reduce_Slots (J, Slot_Range, Runtime);
--                 Statistics.Reduce_Range;
--              else
--                 Utils.Trace ("too recent");
--              end if;
--           end;
--        else
--           Utils.Trace ("not queued for CPU, or free CPUs found");
--        end if;
--     exception
--        when E : Parser.Security_Error =>
--           Ada.Text_IO.Put_Line (Exception_Message (E) & " while processing job" & Get_ID (J));
--        when E : SGE.Parser.Parser_Error =>
--           Ada.Text_IO.Put_Line (Exception_Message (E) & " while processing job" & Get_ID (J));
--        when E : Support_Error =>
--           Ada.Text_IO.Put_Line ("Job" & Get_ID (J) & " unexpectedly lacks Balancer support: "
--                                 & Exception_Message (E));
--        when E : others =>
--           Ada.Text_IO.Put_Line ("unexpected error in job " & Get_ID (J) & ": "
--                                 & Exception_Message (E));
--     end Extend_Slots_Below;

   function Get_ID (J : Changed_Job) return String is
   begin
      return J.ID'Img;
   end Get_ID;

   function Get_PE (J : Changed_Job) return String is
   begin
      return To_String (J.PE);
   end Get_PE;

   function Get_Reservation (J : Changed_Job) return SGE.Utils.Tri_State is
   begin
      return J.Reserve;
   end Get_Reservation;

   function Get_Resources (J : Changed_Job) return SGE.Resources.Hashed_List is
   begin
      return J.Resources;
   end Get_Resources;

   function Get_Slots (J : Changed_Job) return SGE.Ranges.Step_Range_List is
   begin
      return J.Slots;
   end Get_Slots;

   ----------
   -- Init --
   ----------

   procedure Init is
      SGE_Out : SGE.Parser.Tree;
      function Not_On_Hold (J : Job) return Boolean;

      function Not_On_Hold (J : Job) return Boolean is
      begin
         return not On_Hold (J);
      end Not_On_Hold;

   begin
      SGE_Out := SGE.Parser.Setup (Selector => "-u * -r -s p");
      Append_List (SGE.Parser.Get_Job_Nodes_From_Qstat_U (SGE_Out));
      SGE.Parser.Free;
      SGE_Out := SGE.Parser.Setup (Selector => "-j *");
      Create_Overlay (SGE.Parser.Get_Job_Nodes_From_Qstat_J (SGE_Out));
      Apply_Overlay;
      SGE.Parser.Free;
      SGE_Out := SGE.Parser.Setup (Command  => "qquota",
                                   Selector => "-l slots -u *");
      SGE.Quota.Append_List (SGE.Parser.Get_Elements_By_Tag_Name (Doc      => SGE_Out,
                                                                  Tag_Name => "qquota_rule"));
      SGE.Parser.Free;
      Utils.Verbose_Message (SGE.Jobs.Count (Not_On_Hold'Access)'Img & " pending jobs");
      if Utils.On_Automatic then
         SGE.Jobs.Prune_List (Keep => Is_Eligible'Access);
         SGE.Jobs.Update_Quota;
         SGE.Jobs.Iterate (Parser.Add_Pending_Since'Access);
      end if;
      SGE.Jobs.Iterate (Users.Add_Job'Access);
      Chain_Count := 0;
--      SGE.Jobs.Iterate (Add_Chain_Head'Access);
      Utils.Verbose_Message (SGE.Jobs.Count (Not_On_Hold'Access)'Img
                             & " by" & Users.Total_Users'Img
                             & " users eligible for re-queueing");
--      Utils.Verbose_Message (Chain_Count'Img & " chain heads found ("
--                             & Chain_Heads.Length'Img & " with extension support)");
   end Init;

   function Init (ID : Positive; Old_State, New_State : State) return Changed_Job is
      J : Changed_Job;
   begin
      J.ID := ID;
      J.Old_State := Old_State;
      J.New_State := New_State;
      return J;
   end Init;

   -------------
   -- Balance --
   -------------

   function Is_Eligible (J : Job) return Boolean is
   begin
      return Supports_Balancer (J) and then not Has_Error (J);
   end Is_Eligible;

   function Match (J : Changed_Job; Old_State, New_State : State) return Boolean is
      function Match (S, Pattern : State) return Boolean;

      function Match (S, Pattern : State) return Boolean is
      begin
         if Pattern = any then
            return True;
         end if;
         return S = Pattern;
      end Match;

   begin
      return Match (J.Old_State, Old_State) and then Match (J.New_State, New_State);
   end Match;

   procedure Migrate_To_CPU (J : Job) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
      Item : Changed_Job := Init (ID => Get_ID (J), New_State => cpu, Old_State => gpu);
   begin
      New_Resources.Delete (Key => To_Unbounded_String ("gpu"));
      Item.Resources := New_Resources;
      Set_Slots (Item, Comma_Convert (Get_Context (J => J, Key => SGE.Context.Slots_CPU)));
      Set_PE (Item, Get_PE (J));
      Sanitiser.Apply_Rules (Item);
      Modified.Append (Item);
   end Migrate_To_CPU;

   procedure Migrate_To_GPU (J : Job) is
      New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
      Item : Changed_Job := Init (ID => Get_ID (J), New_State => gpu, Old_State => cpu);
   begin
      Resources.Add (New_Resources, Name => "gpu", Value => "1");
      Item.Resources := New_Resources;
      Set_Slots (Item, Comma_Convert (Get_Context (J => J, Key => SGE.Context.Slots_GPU)));
      Set_PE (Item, Get_PE (J));
      Sanitiser.Apply_Rules (Item);
      Modified.Append (Item);
   end Migrate_To_GPU;

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

--     procedure Reduce_Slots (J : Job; To : String; Runtime : String) is
--        New_Resources : SGE.Resources.Hashed_List := Get_Hard_Resources (J);
--        Item : Changed_Job := (ID => Get_ID (J), New_State => reduced, Timestamp => "LASTRED", Slots => To);
--     begin
--        if Runtime /= "" then
--           New_Resources.Delete (Key => To_Unbounded_String ("h_rt"));
--           Resources.Add (To => New_Resources, Name => "h_rt", Value => Runtime);
--        end if;
--        Item.Resources := New_Resources;
--        Modified.Append (Item);
--     end Reduce_Slots;

   procedure Remove_Resource (J : in out Changed_Job; Res : Unbounded_String) is
   begin
      Utils.Trace ("Removing " & To_String (Res));
      J.Resources.Exclude (Res);
   end Remove_Resource;

   procedure Set_PE (J : in out Changed_Job; To : Unbounded_String) is
   begin
      J.PE := To;
   end Set_PE;

   procedure Set_Reservation (J : in out Changed_Job; To : Boolean) is
   begin
      J.Reserve := To_Tri_State (To);
   end Set_Reservation;

   procedure Set_Resources (J : in out Changed_Job; To : Unbounded_String) is
   begin
      J.Resources.Clear;
      Add_Resource (J, To_String (To));
   end Set_Resources;

   procedure Set_Slots (J : in out Changed_Job; To : String) is
   begin
      J.Slots := SGE.Ranges.To_Step_Range_List (To);
   end Set_Slots;

   procedure Set_Slots (J : in out Changed_Job; To : SGE.Ranges.Step_Range_List) is
   begin
      J.Slots := To;
   end Set_Slots;

   procedure Shift (J : Natural; To : String) is
   begin
      if To = "gpu" then
         Migrate_To_GPU (Find_Job (J));
         Statistics.To_GPU;
      elsif To = "cpu" then
         Migrate_To_CPU (Find_Job (J));
         Statistics.To_CPU;
      elsif To = "rules" then
         Apply_Rules_Only (Find_Job (J));
      else
         raise Constraint_Error with "unknown destination """ & To & """";
      end if;
   end Shift;

   function Timestamp (J : Changed_Job) return String is
   begin
      case J.New_State is
         when cpu => return "LASTMIG";
         when gpu => return "LASTMIG";
         when any =>
            raise Constraint_Error with "Job" & J.ID'Img & " has illegal state ""any""";
         when undefined =>
         raise Constraint_Error with "Unknown state encountered in" & J.ID'Img;
      end case;
   end Timestamp;

end Jobs;

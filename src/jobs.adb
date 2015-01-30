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

   procedure Balance_CPU_GPU (J : Job);
   function Comma_Convert (Encoded_String : String) return String;
   procedure Apply_Changes (Position : Changed_Lists.Cursor);
   function Timestamp (J : Changed_Job) return String;

   procedure Add_Message (J : in out Changed_Job; Message : String) is
   begin
      J.Messages.Append (To_Unbounded_String (Message));
   end Add_Message;

   procedure Add_Resource (J : in out Changed_Job; Res : String) is
      Separator : constant Natural := Ada.Strings.Fixed.Index (Res, "=");
      Name      : constant String := Res (Res'First .. Separator - 1);
      Value     : constant String := Res (Separator + 1 .. Res'Last);
   begin
      Resources.Add (To => J.Resources, Name => Name, Value => Value);
      J.Changed := True;
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

   procedure Balance is
   begin
      Utils.Trace ("Shifting jobs between CPU and GPU");
      Users.Iterate (Balance_CPU_GPU'Access);
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

   procedure Freeze (J : in out Changed_Job) is
   begin
      J.Changed := False;
   end Freeze;

   function Get_ID (J : Changed_Job) return String is
   begin
      return J.ID'Img;
   exception
      when Constraint_Error =>
         -- ID 0 (unset)
         return "";
   end Get_ID;

   function Get_ID (J : Changed_Job) return Positive is
   begin
      return J.ID;
   end Get_ID;

   function Get_Messages (J : Changed_Job) return String is
      use SGE.Utils.String_Lists;
      procedure Store (Position : Cursor);

      Result : Unbounded_String;

      procedure Store (Position : Cursor) is
      begin
         if Length (Result) /= 0 then
            Append (Result, ", ");
         end if;
         Append (Result, Element (Position));
      end Store;

   begin
      Iterate (J.Messages, Store'Access);
      return To_String (Result);
   end Get_Messages;

   function Get_Name (J : Changed_Job) return String is
   begin
      return To_String (J.Name);
   end Get_Name;

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
      Utils.Verbose_Message (SGE.Jobs.Count (Not_On_Hold'Access)'Img
                             & " by" & Users.Total_Users'Img
                             & " users eligible for re-queueing");
   end Init;

   function Init (ID : Positive; Old_State, New_State : State) return Changed_Job is
      J : Changed_Job;
   begin
      J.ID := ID;
      J.Old_State := Old_State;
      J.New_State := New_State;
      return J;
   end Init;

   function Is_Changed (J : Changed_Job) return Boolean is
   begin
      return J.Changed;
   end Is_Changed;

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

   procedure Remove_Resource (J : in out Changed_Job; Res : Unbounded_String) is
   begin
      Utils.Trace ("Removing " & To_String (Res));
      J.Resources.Exclude (Res);
      J.Changed := True;
   end Remove_Resource;

   procedure Set_ID (J : in out Changed_Job; ID : String) is
   begin
      Set_ID (J, Natural'Value (ID));
   end Set_ID;

   procedure Set_ID (J : in out Changed_Job; ID : Positive) is
   begin
      J.ID := ID;
   end Set_ID;

   procedure Set_Name (J : in out Changed_Job; Name : String) is
   begin
      J.Name := To_Unbounded_String (Name);
   end Set_Name;

   procedure Set_New_State (J : in out Changed_Job) is
   begin
      if J.Resources.Contains (To_Unbounded_String ("gpu")) then
         J.New_State := gpu;
      else
         J.New_State := cpu;
      end if;
   end Set_New_State;

   procedure Set_Old_State (J : in out Changed_Job; To : State) is
   begin
      J.Old_State := To;
   end Set_Old_State;

   procedure Set_PE (J : in out Changed_Job; To : Unbounded_String) is
   begin
      J.PE := To;
      J.Changed := True;
   end Set_PE;

   procedure Set_Reservation (J : in out Changed_Job; To : Boolean) is
   begin
      if J.Reserve /= To_Tri_State (To) then
      J.Reserve := To_Tri_State (To);
      J.Changed := True;
      end if;
   end Set_Reservation;

   procedure Set_Resources (J : in out Changed_Job; To : String) is
      use Ada.Strings.Fixed;

      Index_List : array (1 .. 256) of Natural;
      Next_Index : Natural := 1;
   begin
      J.Resources.Clear;
      Index_List (Next_Index) := 1;
      while Index_List (Next_Index) < To'Last loop
         Next_Index := Next_Index + 1;
         Index_List (Next_Index) := 1 + Index (To (Index_List (Next_Index - 1) .. To'Last), ",");
         if Index_List (Next_Index) = 1 then
            Index_List (Next_Index) := To'Last + 2;
         end if;
         Add_Resource (J, To (Index_List (Next_Index - 1) .. Index_List (Next_Index) - 2));
      end loop;
      J.Changed := True;
   end Set_Resources;

   procedure Set_Slots (J : in out Changed_Job; To : String) is
   begin
      J.Slots := SGE.Ranges.To_Step_Range_List (To);
      J.Changed := True;
   end Set_Slots;

   procedure Set_Slots (J : in out Changed_Job; To : SGE.Ranges.Step_Range_List) is
   begin
      J.Slots := To;
      J.Changed := True;
   end Set_Slots;

   procedure Set_Slots_Max (J : in out Changed_Job; To : Positive) is
      Min : Positive := 1;
   begin
      if not J.Slots.Is_Empty then
         Min := J.Slots.Min;
         J.Slots.Clear;
      end if;
      J.Slots.Append (SGE.Ranges.New_Range (Min  => Min,
                                 Step => 1,
                                 Max  => To));
      J.Changed := True;
   end Set_Slots_Max;

   procedure Set_Slots_Min (J : in out Changed_Job; To : Positive) is
      Max : Positive := 1_024;
   begin
      if not J.Slots.Is_Empty then
         Max := J.Slots.Max;
         J.Slots.Clear;
      end if;
      J.Slots.Append (SGE.Ranges.New_Range (Min  => To,
                                 Step => 1,
                                 Max  => Max));
      J.Changed := True;
   end Set_Slots_Min;

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

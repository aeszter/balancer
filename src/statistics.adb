with Ada.Text_IO;

package body Statistics is

   function No_Slots return Natural;
   procedure Increment (What : in out Natural);


   procedure Increment (What : in out Natural) is
   begin
      What := What + 1;
      Pristine := False;
   end Increment;

   function Is_Pristine return Boolean is
   begin
      return Pristine;
   end Is_Pristine;

   -----------
   -- Print --
   -----------

   procedure Print is
      use Ada.Containers;
      procedure Put (Item : String) renames Ada.Text_IO.Put_Line;

   begin
      if Pristine then
         return;
      end if;
      Put ("Statistics:");
      if Global_Stats.To_CPU > 0 then
         Put (Global_Stats.To_CPU'Img & " jobs migrated to CPU-only queues");
      end if;
      if Global_Stats.To_GPU > 0 then
         Put (Global_Stats.To_GPU'Img & " jobs migrated to GPU queues");
      end if;
      if No_Slots > 0 then
         Put (No_Slots'Img & " jobs not migrated ("
              & Global_Stats.No_CPU_Slots'Img & " to CPU/"
              & Global_Stats.No_GPU_Slots'Img & " to GPU) "
                & "because there are no free slots");
      end if;
      if Global_Stats.Quota.Length > 0 then
         Put (Global_Stats.Quota.Length'Img
               & " jobs not migrated because of a quota limit");
      end if;
      if Global_Stats.Recent > 0 then
         Put (Global_Stats.Recent'Img
               & " jobs too recent to be migrated");
      end if;
      if Global_Stats.Range_Reduction > 0 then
         Put (Global_Stats.Range_Reduction'Img
               & " jobs changed to lower slot numbers");
      end if;
      if Global_Stats.Range_Extension > 0 then
         Put (Global_Stats.Range_Extension'Img
                               & " jobs changed to higher slot numbers");
      end if;
      if Global_Stats.Aimless > 0 then
         Put (Global_Stats.Aimless'Img & " jobs without destination found");
      end if;
   end Print;

   ------------
   -- To_CPU --
   ------------

   procedure To_CPU is
   begin
      Increment (Global_Stats.To_CPU);
   end To_CPU;

   procedure No_CPU is
   begin
      Increment (Global_Stats.No_CPU_Slots);
   end No_CPU;

   ------------
   -- To_GPU --
   ------------

   procedure To_GPU is
   begin
      Increment (Global_Stats.To_GPU);
   end To_GPU;

   procedure No_GPU is
   begin
      Increment (Global_Stats.No_GPU_Slots);
   end No_GPU;

   procedure Quota_Inhibited (ID : Positive) is
   begin
      Global_Stats.Quota.Include (ID);
      Pristine := False;
   end Quota_Inhibited;

   -----------------
   -- Aimless_Job --
   -----------------

   procedure Aimless_Job is
   begin
      Increment (Global_Stats.Aimless);
   end Aimless_Job;

   procedure Recent_Job is
   begin
      Increment (Global_Stats.Recent);
   end Recent_Job;

   procedure Extend_Range is
   begin
      Increment (Global_Stats.Range_Extension);
   end Extend_Range;

   procedure Reduce_Range is
   begin
      Increment (Global_Stats.Range_Reduction);
   end Reduce_Range;

   function No_Slots return Natural is
   begin
      return Global_Stats.No_CPU_Slots + Global_Stats.No_GPU_Slots;
   end No_Slots;
end Statistics;

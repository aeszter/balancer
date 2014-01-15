with Ada.Text_IO;

package body Statistics is

   function No_Slots return Natural;

   -----------
   -- Print --
   -----------

   procedure Print is
      procedure Put (Item : String) renames Ada.Text_IO.Put_Line;

   begin
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
      if Global_Stats.Quota > 0 then
         Put (Global_Stats.Quota'Img
               & " jobs not migrated because of a quota limit");
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
      Global_Stats.To_CPU := Global_Stats.To_CPU + 1;
   end To_CPU;

   procedure No_CPU is
   begin
      Global_Stats.No_CPU_Slots := Global_Stats.No_CPU_Slots + 1;
   end No_CPU;

   ------------
   -- To_GPU --
   ------------

   procedure To_GPU is
   begin
      Global_Stats.To_GPU := Global_Stats.To_GPU + 1;
   end To_GPU;

   procedure No_GPU is
   begin
      Global_Stats.No_GPU_Slots := Global_Stats.No_GPU_Slots + 1;
   end No_GPU;

   procedure Quota_Inhibited is
   begin
      Global_Stats.Quota := Global_Stats.Quota + 1;
   end Quota_Inhibited;

   -----------------
   -- Aimless_Job --
   -----------------

   procedure Aimless_Job is
   begin
      Global_Stats.Aimless := Global_Stats.Aimless + 1;
   end Aimless_Job;

   procedure Extend_Range is
   begin
      Global_Stats.Range_Extension := Global_Stats.Range_Extension + 1;
   end Extend_Range;

   procedure Reduce_Range is
   begin
      Global_Stats.Range_Reduction := Global_Stats.Range_Reduction + 1;
   end Reduce_Range;

   function No_Slots return Natural is
   begin
      return Global_Stats.No_CPU_Slots + Global_Stats.No_GPU_Slots;
   end No_Slots;
end Statistics;

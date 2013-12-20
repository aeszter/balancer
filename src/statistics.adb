with Ada.Text_IO;

package body Statistics is

   -----------
   -- Print --
   -----------

   procedure Print is
   begin
      Ada.Text_IO.Put_Line ("Statistics:");
      if Global_Stats.To_CPU > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.To_CPU'Img & " jobs migrated to CPU-only queues");
      end if;
      if Global_Stats.To_GPU > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.To_GPU'Img & " jobs migrated to GPU queues");
      end if;
      if Global_Stats.No_Slots > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.No_Slots'Img & " jobs not migrated because there are no free slots");
      end if;
      if Global_Stats.Aimless > 0 then
         Ada.Text_IO.Put_Line (Global_Stats.Aimless'Img & " jobs without destination found");
      end if;
   end Print;

   ------------
   -- To_CPU --
   ------------

   procedure To_CPU is
   begin
      Global_Stats.To_CPU := Global_Stats.To_CPU + 1;
   end To_CPU;

   procedure No_Slots is
   begin
      Global_Stats.No_Slots := Global_Stats.No_Slots + 1;
   end No_Slots;

   ------------
   -- To_GPU --
   ------------

   procedure To_GPU is
   begin
      Global_Stats.To_GPU := Global_Stats.To_GPU + 1;
   end To_GPU;

   -----------------
   -- Aimless_Job --
   -----------------

   procedure Aimless_Job is
   begin
      Global_Stats.Aimless := Global_Stats.Aimless + 1;
   end Aimless_Job;

end Statistics;

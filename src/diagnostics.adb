with Ada.Text_IO; use Ada.Text_IO;
with POSIX.Process_Times; use POSIX.Process_Times;

-----------------
-- Diagnostics --
-----------------

package body Diagnostics is

   type Seconds is delta 0.1 digits 5;

   function To_Seconds (T : Tick_Count) return Seconds;
   procedure Put_Time;
   procedure Put_Memory;


   procedure Print is
   begin
      Put_Time;
      Put_Memory;
   end Print;


   ----------------
   -- To_Seconds --
   ----------------

   function To_Seconds (T : Tick_Count) return Seconds is
   begin
      return Seconds (T) / Seconds (Ticks_Per_Second);
   end To_Seconds;


   ---------------------------------------------------------------------------
   -- Put_Time ---------------------------------------------------------------
   --  Purpose: Put User and CPU times for both self and children to Standard Out
   --  Parameters: none
   ---------------------------------------------------------------------------

   procedure Put_Time is
      Times : Process_Times;
      Self  : Seconds;
      Children : Seconds;
   begin
      Times := Get_Process_Times;
      Self := To_Seconds (User_CPU_Time_Of (Times) + System_CPU_Time_Of (Times));
      Children := To_Seconds (Descendants_User_CPU_Time_Of (Times)
                   + Descendants_System_CPU_Time_Of (Times));
      Put_Line (Self'Img & "s self " & Children'Img & "s children");
   end Put_Time;


   procedure Put_Memory is
      Status_File : Ada.Text_IO.File_Type;
      Line        : String (1 .. 256);
      Last : Natural; -- length of line read
   begin
      Open (File     => Status_File,
            Mode     => In_File,
            Name     => "/proc/self/status");
      while not End_Of_File (Status_File) loop
         Get_Line (Status_File, Line, Last);
         if Line (Line'First .. Line'First + 6) = "VmPeak:" then
            Ada.Text_IO.Put (Line (Line'First + 7 .. Last));
            Ada.Text_IO.Put (" virtual");
         elsif Line (Line'First .. Line'First + 5) = "VmHWM:" then
            Ada.Text_IO.Put (Line (Line'First + 6 .. Last));
            Ada.Text_IO.Put (" RSS");
         end if;
      end loop;
      Close (Status_File);
   end Put_Memory;

end Diagnostics;

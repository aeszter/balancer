with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with Diagnostics;
with Utils; use Utils;
with Jobs;
with JSV;
with Partitions;
with Statistics;
with SGE.Utils;
with Sanitiser;


procedure Balancer is
   Exit_Unknown_Error : constant Exit_Status := 1;
   My_Name : constant String := Ada.Command_Line.Command_Name;
begin
   if My_Name (My_Name'Last - 2 .. My_Name'Last) = "jsv" then
      JSV.Main_Loop;
      return;
   elsif My_Name (My_Name'Last - 7 .. My_Name'Last) /= "balancer" then
      Utils.Error_Message ("Warning: this executable should be called "
                           & "either ""jsv"" or ""balancer"". "
                           & "Running in balancer mode now, "
                           & "but this behaviour may change.");
   end if;

   Utils.Check_Options;
   Utils.Verbose_Message ("Balancer " & Utils.Version & " by aeszter@mpibpc.mpg.de");
   Utils.Verbose_Message ("SGElib " & SGE.Utils.Version);
   Debug ("Debugging enabled");

   Sanitiser.Init;
   Jobs.Init;

   if Utils.On_Automatic then
      Partitions.Init;

      Jobs.Balance;
   elsif Utils.On_Manual then
      Utils.Rewind_Manual_Jobs;
      while Utils.Has_Manual_Job loop
         declare
            ID : constant Positive := Utils.Get_Manual_Job;
         begin
            Jobs.Shift (J => ID, To => Utils.Get_Destination);
         exception
            when Constraint_Error => Utils.Error_Message ("Skipping job" & ID'Img);
         end;
         Utils.Next_Manual_Job;
      end loop;
      Jobs.Apply_Recorded_Changes;
   else
      raise Program_Error with "neither automatic nor manual mode";
   end if;

   Statistics.Print;
   if not Statistics.Is_Pristine then
      Diagnostics.Print;
      Ada.Text_IO.Put_Line (Utils.Version);
   end if;
exception
   when E : others =>
      Put_Line (File => Standard_Error,
                Item => "Unexpected error (" & Exception_Name (E) & "): " & Exception_Message (E));
      Put (File => Standard_Error,
           Item => Exception_Information (E));
      Set_Exit_Status (Code => Exit_Unknown_Error);
end Balancer;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with Diagnostics;
with Utils; use Utils;
with Jobs;
with Partitions;
with Statistics;
with SGE.Utils;


procedure Balancer is
   Exit_Unknown_Error : constant Exit_Status := 1;

begin
   Utils.Check_Options;
   Utils.Verbose_Message ("Balancer " & Utils.Version & " by aeszter@mpibpc.mpg.de");
   Utils.Verbose_Message ("SGElib " & SGE.Utils.Version);
   Debug ("Debugging enabled");

   Jobs.Init;
   Partitions.Init;

   Jobs.Balance;
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

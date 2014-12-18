with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with POSIX.Process_Primitives;
with Ada.Numerics; use Ada.Numerics;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;
with Ada.Calendar;
with Ada.Characters.Handling;


package body Utils is

   -----------
   -- Debug --
   -----------

   procedure Debug (Message : String; New_Line : Boolean := True) is
   begin
      if Debug_Enabled then
         if New_Line then
            Put_Line (Standard_Error, Message);
         else
            Put (Standard_Error, Message);
         end if;
      end if;
   end Debug;

   -------------
   -- Dry_Run --
   -------------

   function Dry_Run (Message : String) return Boolean is
   begin
      if not Action then
         Ada.Text_IO.Put_Line (Message);
      end if;
      return not Action;
   end Dry_Run;

   procedure Enable_Debug is
   begin
      Debug_Enabled := True;
   end Enable_Debug;

   function Get_Job return Natural is
   begin
      return Integer'Value (To_String (Manual_Job));
   end Get_Job;

   function Random return Float_Random.Uniformly_Distributed is
   begin
      return Float_Random.Random (Random_Generator);
   end Random;

   procedure Trace (Message : String) is
   begin
      if Trace_Policy then
         Put_Line (Message);
      end if;
   end Trace;

   ---------------------
   -- Verbose_Message --
   ---------------------

   procedure Verbose_Message (Message : String) is
   begin
      if Verbose then
         Put_Line (Message);
      end if;
   end Verbose_Message;

   procedure Error_Message (Message : String; Bug_ID : Natural := 0) is
   begin
      Put_Line (Message);
      if Bug_ID /= 0 then
         Put_Line ("See Bug" & Bug_ID'Img
                   & ": http://ram/bugzilla/show_bug.cgi?id=" & Bug_ID'Img);
      end if;
   end Error_Message;


   ------------------
   -- Enable_Debug --
   ------------------

   -------------------
   -- Check_Options --
   -------------------

   procedure Check_Options is
   begin
      for Arg in 1 .. Argument_Count loop
         if Argument (Arg) = "-d" or else
           Argument (Arg) = "--debug" then
            Debug_Enabled := True;
         elsif Argument (Arg) = "-v" or else
           Argument (Arg) = "--verbose" then
            Verbose := True;
         elsif Argument (Arg) = "-n" or else
           Argument (Arg) = "--no-action" then
            Action := False;
         elsif Argument (Arg) = "-s" or else
           Argument (Arg) = "--statistics" then
            Stats := True;
         elsif Argument (Arg) = "-p" or else
           Argument (Arg) = "--policy" then
            Trace_Policy := True;
         elsif Argument (Arg) = "-m" or else
           Argument (Arg) = "--manual" then
            Mode := manual;
            Manual_Job := To_Unbounded_String (Argument (Arg + 1));
            Manual_Destination := To_Unbounded_String (
                       Ada.Characters.Handling.To_Lower (Argument (Arg + 2)));
            return;
         elsif Argument (Arg) = "-h" or else
           Argument (Arg) = "--help" then
            Ada.Text_IO.Put_Line ("Options may be given in full or with a single hyphen "
                                  & "and the first letter only");
            Ada.Text_IO.Put_Line ("--debug gives debugging output");
            Ada.Text_IO.Put_Line ("--verbose states which actions are taken");
            Ada.Text_IO.Put_Line ("--no-action only goes through the motions "
                                  & "without actually calling qmod or cmsh; "
                                  & " implies --verbose");
            Ada.Text_IO.Put_Line ("--statistics shows a summary of what has been done");
            Ada.Text_IO.Put_Line ("--policy prints details about decisions taken");
            Ada.Text_IO.Put_Line ("--manual ID (cpu|gpu) unconditionally puts the given job "
                                 & "into the cpu or gpu queue");
            Ada.Text_IO.Put_Line ("--help shows this message, then terminates");
            POSIX.Process_Primitives.Exit_Process;
         else
            raise Program_Error with "unknown option: " & Argument (Arg);
         end if;
      end loop;
   end Check_Options;

   function On_Automatic return Boolean is
   begin
      return Mode = automatic;
   end On_Automatic;

   function On_Manual return Boolean is
   begin
      return Mode = manual;
   end On_Manual;

   function Get_Destination return String is
   begin
      return To_String (Manual_Destination);
   end Get_Destination;

   function Stats_Enabled return Boolean is
   begin
      return Stats;
   end Stats_Enabled;


   procedure Init_Random is
   begin
      Float_Random.Reset (Random_Generator);
   end Init_Random;


   function Now return String is
      Raw_Time : constant Picture_String := "%s";
      -- Result is one hour off compared to date +%s
      -- as tested on 2013-12-11
      -- The reason is unknown.
   begin
      return Image (Date => Ada.Calendar.Clock, Picture => Raw_Time);
   end Now;

   function To_Number (Num : String) return Integer is
   begin
      return Integer'Value (Num);
   end To_Number;


end Utils;

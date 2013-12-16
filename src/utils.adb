with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with POSIX.Process_Primitives;
with Ada.Numerics; use Ada.Numerics;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;
with Ada.Calendar;


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

   procedure Enable_Debug is
   begin
      Debug_Enabled := True;
   end Enable_Debug;


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
            Ada.Text_IO.Put_Line ("--help shows this message, then terminates");
            POSIX.Process_Primitives.Exit_Process;
         else
            raise Program_Error with "unknown option: " & Argument (Arg);
         end if;
      end loop;
   end Check_Options;

   function Stats_Enabled return Boolean is
   begin
      return Stats;
   end Stats_Enabled;


   function Random return Float_Random.Uniformly_Distributed is
   begin
      return Float_Random.Random (Random_Generator);
   end Random;

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


end Utils;

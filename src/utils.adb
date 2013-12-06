with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with POSIX.Process_Primitives;
with Ada.Numerics; use Ada.Numerics;


package body Utils is

   -----------
   -- Debug --
   -----------

   procedure Debug (Message : String) is
   begin
      if Debug_Enabled then
         Put_Line (Standard_Error, Message);
      end if;
   end Debug;

   ---------------------
   -- Verbose_Message --
   ---------------------

   procedure Verbose_Message (Message : String) is
   begin
      if Verbose then
         Put_Line (Message);
      end if;
   end Verbose_Message;

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
         elsif Argument (Arg) = "-s" or else
           Argument (Arg) = "--statistics" then
            Stats := True;
         elsif Argument (Arg) = "-h" or else
           Argument (Arg) = "--help" then
            Ada.Text_IO.Put_Line ("Options may be given in full or with a single hyphen "
                                  & "and the first letter only");
            Ada.Text_IO.Put_Line ("--debug gives debugging output");
            Ada.Text_IO.Put_Line ("--verbose states which actions are taken");
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


end Utils;

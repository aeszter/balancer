with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Utils;
with Ada.Text_IO;

package Utils is
   Version : String := "feature:jsv";

   Assumption_Error : exception;

   procedure Debug (Message : String; New_Line : Boolean := True);
   procedure Verbose_Message (Message : String);
   procedure Error_Message (Message : String; Bug_ID : Natural := 0);
   procedure Trace (Message : String);
   procedure Open_Message_File (Name : String);

   procedure Enable_Debug;
   function Dry_Run (Message         : String) return Boolean;
   -- return whether Action is false
   -- if so, print Message

   function Stats_Enabled return Boolean;

   function On_Automatic return Boolean;
   function On_Manual return Boolean;

   function Get_Destination return String;
   procedure Rewind_Manual_Jobs;
   procedure Next_Manual_Job;
   function Has_Manual_Job return Boolean;
   function Get_Manual_Job return Positive;

   function Random return Ada.Numerics.Float_Random.Uniformly_Distributed;
   procedure Init_Random;
   function Now return String;

   procedure Check_Options;

   function To_Number (Num : String) return Integer;

   type Operation_Mode is (automatic, manual);

private
   Action           : Boolean := True;
   Debug_Enabled    : Boolean := False;
   Verbose          : Boolean := False;
   Stats            : Boolean := False;
   Trace_Policy     : Boolean := False;
   Random_Generator : Ada.Numerics.Float_Random.Generator;
   Mode             : Operation_Mode := automatic;
   Manual_Destination : Unbounded_String;
   Manual_Jobs      : SGE.Utils.ID_List;
   Current_Manual_Job : SGE.Utils.ID_Lists.Cursor;
   Message_File     : Ada.Text_IO.File_Type;
end Utils;

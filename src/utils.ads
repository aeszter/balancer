with Ada.Numerics.Float_Random;

package Utils is
   Version : String := "v0.1";

   Assumption_Error : exception;

   procedure Debug (Message : String);
   procedure Verbose_Message (Message : String);
   procedure Enable_Debug;
   function Dry_Run (Message         : String) return Boolean;
   -- return whether Action is false
   -- if so, print Message

   function Stats_Enabled return Boolean;

   function Random return Ada.Numerics.Float_Random.Uniformly_Distributed;
   procedure Init_Random;
   function Now return String;

   procedure Check_Options;


private
   Action           : Boolean := True;
   Debug_Enabled    : Boolean := False;
   Verbose          : Boolean := False;
   Stats            : Boolean := False;
   Random_Generator : Ada.Numerics.Float_Random.Generator;
end Utils;
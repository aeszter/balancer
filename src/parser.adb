with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Parser;
with SGE.Spread_Sheets;
with SGE.Jobs; use SGE.Jobs;
with Jobs;
with Utils;

package body Parser is

   function Context_Clean (Input : String) return String;
   -- Note: this function is security-critical
   -- Make sure to call this on all data read from Context and passed to
   -- qalter (or other shell commands)

   ---------------
   -- Alter_Job --
   ---------------

   procedure Alter_Job
     (Job       : SGE.Jobs.Job;
      Insecure_Resources : String := "";
      Slots     : String := "")
   is
      Requirements : Unbounded_String := To_Unbounded_String (Get_ID (Job));
      Output : SGE.Spread_Sheets.Spread_Sheet;
      Timestamp    : constant String := "-sc LASTMIG=" & Utils.Now;
      pragma Unreferenced (Output);
      -- Can we do something useful with the output?
   begin
      if Slots /= "" then
         if To_String (SGE.Jobs.Get_PE (Job)) = "" then
            raise Jobs.Support_Error with "no PE found";
         end if;
         Requirements := Requirements & " -pe " & SGE.Jobs.Get_PE (Job) &
                                         " " & Context_Clean (Slots);
      end if;
      if Insecure_Resources /= "" then
         Requirements := Requirements & " -l " & Insecure_Resources;
      end if;
      Output := SGE.Parser.Setup_No_XML (Command => "qalter",
                                        Selector =>  To_String (Requirements) & Timestamp);
   end Alter_Job;

   procedure Add_Pending_Since (J : Job) is
      Output : SGE.Spread_Sheets.Spread_Sheet;
      pragma Unreferenced (Output);
      -- Can we do something useful with the output?
   begin
      if Get_Context (J, "PENDINGSINCE") = "" then
         Output := SGE.Parser.Setup_No_XML (Command => "qalter",
                                         Selector => Get_ID (J) & " -ac PENDINGSINCE=" & Utils.Now);
      end if;
   end Add_Pending_Since;


   function Context_Clean (Input : String) return String is
      Output : String := Input;

      function Is_Harmless_Dash (Char : in Character; Where : Positive) return Boolean is
      begin
         if Char = '-' and then
              Where > Output'First and then
           Ada.Characters.Handling.Is_Alphanumeric (Output (Where - 1)) then
            return True; -- a dash, not the first character, and the previous one is alphanumeric
            --  so this does not start a commandline switch
         else
            return False; -- not a dash, or not preceded by a harmless character
         end if;
      end Is_Harmless_Dash;

      function Is_Suspicious (Char : Character) return Boolean is
      begin
         case Char is
         when '&' => return True;
            when '\' => return True;
            when others => return False;
         end case;
      end Is_Suspicious;

   begin
      for Pos in Output'Range loop
         if not Ada.Characters.Handling.Is_Letter (Output (Pos))
           and then not Ada.Characters.Handling.Is_Decimal_Digit (Output (Pos))
           and then Output (Pos) /= ','
           and then not Is_Harmless_Dash (Char  => Output (Pos), Where => Pos) then
            if Is_Suspicious (Output (Pos)) then
               raise Security_Error with "Suspicious character '"
                 & Output (Pos) & "' encountered";
            end if;
            Output (Pos) := '_';
         end if;
      end loop;
      return Output;
   end Context_Clean;

end Parser;

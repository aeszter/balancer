with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with RulesP; with RulesS;
with Utils;
with FileIO;

package body Sanitiser is

   Rules_File : constant String := "/etc/balancer.rules";

   procedure Apply_Rules (J : Changed_Job) is
      procedure Apply_Rule (Position : Rule_Lists.Cursor);

      procedure Apply_Rule (Position : Rule_Lists.Cursor) is
         The_Rule : constant Rule := Rule_Lists.Element (Position);
      begin
         if Jobs.Match (J         => J,
                        Old_State => The_Rule.From,
                        New_State => The_Rule.To) then
            Utils.Trace ("Found a match: " & To_String (The_Rule.Name));
            Utils.Error_Message ("Further checking and application still unimplemented");
         end if;
      end Apply_Rule;
   begin
      Utils.Trace ("Applying rules to" & Get_ID (J));
      Rules.Iterate (Apply_Rule'Access);
   end Apply_Rules;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      FileIO.Open (RulesS.Source, Rules_File, False);
      RulesP.Parse;
   exception
      when E : others =>
         Put_Line ("Could not read " & Rules_File & ". Jobs will not be sanitised.");
         Put_Line (File => Standard_Error,
                  Item => Exception_Name (E) & ": " & Exception_Message (E));
   end Init;

end Sanitiser;

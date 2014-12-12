with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with RulesP;

package body Sanitiser is

   Rules_File : constant String := "/etc/balancer.rules";
   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      RulesP.Parse;
   exception
      when E : others =>
         Put_Line ("Could not read " & Rules_File & ". Jobs will not be sanitised.");
         Put_Line (File => Standard_Error,
                  Item => Exception_Name (E) & ": " & Exception_Message (E));
   end Init;

end Sanitiser;

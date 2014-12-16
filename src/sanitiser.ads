with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Jobs; use Jobs;

package Sanitiser is

   procedure Init;
   procedure Apply_Rules (J : Changed_Job);

private
   type Rule is record
      Name : Unbounded_String;
      From, To : State;
   end record;

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Rule);
   Rules : Rule_Lists.List;

end Sanitiser;

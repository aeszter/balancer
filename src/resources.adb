with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Resources; use SGE.Resources; use SGE.Resources.Resource_Lists;


package body Resources is

   --------------------
   -- To_Requirement --
   --------------------

   function To_Requirement (From : SGE.Resources.Hashed_List) return String is
      Result : Unbounded_String;
      Position : SGE.Resources.Resource_Lists.Cursor := From.First;
   begin
      loop
         Result := Result & Key (Position) & "=" & Element (Position).Value;
         exit when Position = From.Last;
         Result := Result & ",";
         Next (Position);
      end loop;
      return To_String (Result);
   end To_Requirement;

   ---------
   -- Add --
   ---------

   procedure Add
     (To   : in out SGE.Resources.Hashed_List;
      Name, Value : String)
   is
   begin
      To.Insert (Key      => To_Unbounded_String (Name),
                 New_Item => New_Resource (Name, Value));
   end Add;

end Resources;

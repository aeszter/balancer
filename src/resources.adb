with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with SGE.Resources; use SGE.Resources; use SGE.Resources.Resource_Lists;


package body Resources is

   procedure Add
     (To   : in out SGE.Resources.Hashed_List;
      Name, Value : String)
   is
   begin
      if To.Contains (To_Unbounded_String (Name)) then
         To.Delete (To_Unbounded_String (Name));
      end if;
      To.Insert (Key      => To_Unbounded_String (Name),
                 New_Item => New_Resource (Name, Value));
   end Add;

   --------------------
   -- To_Requirement --
   --------------------

   function To_Requirement (From : SGE.Resources.Hashed_List) return String is
      function Seconds (Resource : SGE.Resources.Resource) return String;

      Result : Unbounded_String;
      Name : Unbounded_String;
      Position : SGE.Resources.Resource_Lists.Cursor := From.First;

      function Seconds (Resource : SGE.Resources.Resource) return String is
      begin
         return Ada.Strings.Fixed.Trim (Source => Resource.Numerical'Img,
                                        Side   => Ada.Strings.Left);
      end Seconds;

   begin
      loop
         Name := Key (Position);
         if Name = "h_rt" then
            Result := Result & Name & "=" & Seconds (Element (Position));
         else
            Result := Result & Name & "=" & Element (Position).Value;
         end if;
         exit when Position = From.Last;
         Result := Result & ",";
         Next (Position);
      end loop;
      return To_String (Result);
   end To_Requirement;

   ---------
   -- Add --
   ---------

end Resources;

with SGE.Resources;


package Resources is
   function To_Requirement (From : SGE.Resources.Hashed_List) return String;
   procedure Add (To   : in out SGE.Resources.Hashed_List;
                  Name, Value : String);
end Resources;


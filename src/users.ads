with SGE.Jobs;
with Ada.Containers.Ordered_Maps;

package Users is
   procedure Add_Job (J : SGE.Jobs.Job);
   procedure Iterate (Process : not null access procedure (J : SGE.Jobs.Job));
private
   type User_Name is new String (1 .. 7);
   type Index_Card is record
      User : User_Name;
      Counter : Positive;
   end record;

   function "<" (Left, Right : Index_Card) return Boolean;

   package Job_Lists is new Ada.Containers.Ordered_Maps (Key_Type => Index_Card,
                                                         Element_Type => SGE.Jobs.Job,
                                                         "="          => SGE.Jobs."=");
   package Job_Counts is new Ada.Containers.Ordered_Maps (Key_Type     => User_Name,
                                                         Element_Type => Positive);

   List : Job_Lists.Map;
   Count : Job_Counts.Map;
end Users;

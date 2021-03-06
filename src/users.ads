with SGE.Jobs;
with Ada.Containers.Ordered_Maps;
with SGE.Utils; use SGE.Utils;

package Users is
   procedure Add_Job (J : SGE.Jobs.Job);
   procedure Iterate (Process : not null access procedure (J : SGE.Jobs.Job));
   function Total_Users return Natural;
   function Count_Jobs (For_User : String) return Natural;
private
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

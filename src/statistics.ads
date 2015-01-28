with Ada.Containers.Ordered_Sets;

package Statistics is

   procedure Print;
   function Is_Pristine return Boolean;

   procedure To_CPU;
   procedure To_GPU;
   procedure No_CPU;
   procedure No_GPU;
   procedure Aimless_Job;
   procedure Recent_Job;
   procedure Quota_Inhibited (ID : Positive);

private
   package Job_Counter is new Ada.Containers.Ordered_Sets (Element_Type => Positive);

   type Data is record
      To_GPU, To_CPU : Natural := 0;
      Aimless        : Natural := 0;
      Recent         : Natural := 0;
      No_CPU_Slots, No_GPU_Slots : Natural := 0;
      Quota          : Job_Counter.Set;
   end record;

   Global_Stats : Data;
   Pristine : Boolean := True;
end Statistics;

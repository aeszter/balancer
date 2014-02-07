with Ada.Containers.Ordered_Sets;

package Statistics is

   procedure Print;

   procedure To_CPU;
   procedure To_GPU;
   procedure No_CPU;
   procedure No_GPU;
   procedure Aimless_Job;
   procedure Reduce_Range;
   procedure Extend_Range;
   procedure Quota_Inhibited (ID : Positive);

private
   package Job_Counter is new Ada.Containers.Ordered_Sets (Element_Type => Positive);

   type Data is record
      To_GPU, To_CPU : Natural := 0;
      Aimless        : Natural := 0;
      No_CPU_Slots, No_GPU_Slots : Natural := 0;
      Quota          : Job_Counter.Set;
      Range_Reduction            : Natural := 0;
      Range_Extension : Natural := 0;
   end record;

   Global_Stats : Data;
end Statistics;

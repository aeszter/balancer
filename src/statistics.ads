package Statistics is

   procedure Print;

   procedure To_CPU;
   procedure To_GPU;
   procedure No_CPU;
   procedure No_GPU;
   procedure Aimless_Job;
   procedure Reduce_Range;

private
   type Data is record
      To_GPU, To_CPU : Natural := 0;
      Aimless        : Natural := 0;
      No_CPU_Slots, No_GPU_Slots : Natural := 0;
      Range_Reduction : Natural := 0;
   end record;

   Global_Stats : Data;
end Statistics;

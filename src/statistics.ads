package Statistics is

   procedure Print;

   procedure To_CPU;
   procedure To_GPU;
   procedure No_Slots;
   procedure Aimless_Job;

private
   type Data is record
      To_GPU, To_CPU : Natural := 0;
      Aimless        : Natural := 0;
      No_Slots : Natural := 0;
   end record;

   Global_Stats : Data;
end Statistics;

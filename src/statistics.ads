package Statistics is

   procedure Print;

   procedure To_CPU;
   procedure To_GPU;
   procedure Aimless_Job;

private
   type Data is record
      To_GPU, To_CPU : Natural := 0;
   end record;

   Global_Stats : Data;
end Statistics;

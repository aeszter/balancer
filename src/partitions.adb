package body Partitions is

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Init unimplemented");
      raise Program_Error;
   end Init;

   -------------------
   -- CPU_Available --
   -------------------

   function CPU_Available
     (For_Job : Job;
      Mark_As_Used : Boolean)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "CPU_Available unimplemented");
      raise Program_Error;
      return CPU_Available (For_Job, Mark_As_Used);
   end CPU_Available;

   -------------------
   -- GPU_Available --
   -------------------

   function GPU_Available (Mark_As_Used : Boolean) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "GPU_Available unimplemented");
      raise Program_Error;
      return GPU_Available (Mark_As_Used);
   end GPU_Available;

end Partitions;

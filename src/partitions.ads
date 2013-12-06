with SGE.Jobs; use SGE.Jobs;

package Partitions is
   procedure Init;
   function CPU_Available (For_Job : Job; Mark_As_Used : Boolean) return Boolean;
   function GPU_Available (Mark_As_Used : Boolean) return Boolean;
end Partitions;

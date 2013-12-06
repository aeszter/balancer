with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Jobs;

package Jobs is
   subtype Job is SGE.Jobs.Job;

   procedure Init;
   procedure Balance;
   procedure Balance_One_Job (J : Job);
   procedure Add_Pending_Since (J : Job);
   function Is_Eligible (J : Job) return Boolean;
   function Queued_For_CPU (J : Job) return Boolean;
   function Queued_For_GPU (J : Job) return Boolean;
   procedure Migrate_To_CPU (J : Job);
   procedure Migrate_To_GPU (J : Job);


end Jobs;

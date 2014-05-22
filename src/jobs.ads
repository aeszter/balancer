with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with SGE.Jobs;

package Jobs is
   subtype Job is SGE.Jobs.Job;

   Support_Error : exception; -- Job is missing a required feature

   procedure Init;
   procedure Balance;
   procedure Shift (J : Natural; To : String);
   function Is_Eligible (J : Job) return Boolean;
   function Queued_For_CPU (J : Job) return Boolean;
   function Queued_For_GPU (J : Job) return Boolean;
   procedure Migrate_To_CPU (J : Job);
   procedure Migrate_To_GPU (J : Job);
   procedure Reduce_Slots (J : Job; To : String; Runtime : String);
   procedure Extend_Slots (J : Job; To : String);
   procedure Add_Chain_Head (J : SGE.Jobs.Job);

private
   function Equal_Jobs (Left, Right : Job) return Boolean;

   package Job_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job,
                                                               "=" => Equal_Jobs);

   Chain_Heads   : Job_Lists.List;

   Max_Pending_On_Underutilisation : constant Positive := 10;
end Jobs;

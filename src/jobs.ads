with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with SGE.Jobs;
with SGE.Resources;
with SGE.Ranges;
with SGE.Utils;

package Jobs is
   subtype Job is SGE.Jobs.Job;
   type Changed_Job is private;

   Empty_Job : constant Changed_Job;

   type State is (undefined, cpu, gpu, any);

   Support_Error : exception; -- Job is missing a required feature

   procedure Init;
   procedure Balance;
   procedure Shift (J : Natural; To : String);
   procedure Apply_Recorded_Changes;
   function Is_Eligible (J : Job) return Boolean;
   function Queued_For_CPU (J : Job) return Boolean;
   function Queued_For_GPU (J : Job) return Boolean;
   procedure Migrate_To_CPU (J : Job);
   procedure Migrate_To_GPU (J : Job);
   procedure Apply_Rules_Only (J : Job);

--   procedure Reduce_Slots (J : Job; To : String; Runtime : String);
--   procedure Extend_Slots (J : Job; To : String);
--   procedure Add_Chain_Head (J : SGE.Jobs.Job);
   function Match (J : Changed_Job; Old_State, New_State : State) return Boolean;

   function Get_ID (J : Changed_Job) return String;
   function Get_ID (J : Changed_Job) return Positive;
   function Get_Reservation (J : Changed_Job) return SGE.Utils.Tri_State;
   function Get_PE (J : Changed_Job) return String;
   function Get_Resources (J : Changed_Job) return SGE.Resources.Hashed_List;
   function Get_Slots (J : Changed_Job) return SGE.Ranges.Step_Range_List;

   procedure Set_Slots (J : in out Changed_Job; To : SGE.Ranges.Step_Range_List);
   procedure Set_PE (J : in out Changed_Job; To : Unbounded_String);
   procedure Set_Reservation (J : in out Changed_Job; To : Boolean);
   procedure Set_Resources (J : in out Changed_Job; To : Unbounded_String);
   procedure Add_Resource (J : in out Changed_Job; Res : String);
   procedure Remove_Resource (J : in out Changed_Job; Res : Unbounded_String);

private
   type Changed_Job is record
      ID : Natural;
      Reserve : SGE.Utils.Tri_State := SGE.Utils.Undecided;
      Old_State, New_State : State := undefined;
      PE                   : Unbounded_String := Null_Unbounded_String;
      Resources            : SGE.Resources.Hashed_List;
      Slots                : SGE.Ranges.Step_Range_List;
   end record;

   Empty_Job : constant Changed_Job := (ID      => 0,
                                        Reserve => SGE.Utils.Undecided,
                                        Old_State => undefined,
                                        New_State => undefined,
                                        PE        => Null_Unbounded_String,
                                        Resources => SGE.Resources.Empty_List,
                                        Slots     => SGE.Ranges.Empty_Range
                                       );

   function Init (ID : Positive; Old_State, New_State : State) return Changed_Job;
   procedure Set_Slots (J : in out Changed_Job; To : String);

   function Equal_Jobs (Left, Right : Job) return Boolean;
   function Equal_Jobs (Left, Right : Changed_Job) return Boolean;

   package Job_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job,
                                                                "="          => Equal_Jobs);
   package Changed_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Changed_Job,
                                                                    "="          => Equal_Jobs);

   Chain_Heads : Job_Lists.List;
   Modified    : Changed_Lists.List;

   Max_Pending_On_Underutilisation : constant Positive := 10;
end Jobs;

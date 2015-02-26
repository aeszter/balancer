with SGE.Jobs;
with SGE.Utils; use SGE.Utils;
with SGE.Taint; use SGE.Taint;

package Parser is
   Security_Error : exception;
   Resource_Selector : constant Trusted_String :=
                         Implicit_Trust ("-F h_rt,eth,ib,ibs,ssd,gpu,mem_total,num_proc,cm,gm,q,slots");

   procedure Alter_Job (ID                 : Positive;
                        Insecure_Resources : String := "";
                        PE                 : String := "";
                        Slots              : String := "";
                        Reservation        : Tri_State := Undecided;
                        Timestamp_Name     : Trusted_String);
   procedure Add_Pending_Since (J : SGE.Jobs.Job);
end Parser;

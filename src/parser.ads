with SGE.Jobs;

package Parser is
   Security_Error : exception;

   procedure Alter_Job (Job                : SGE.Jobs.Job;
                        Insecure_Resources : String := "";
                        Slots              : String := "");
   procedure Add_Pending_Since (J : SGE.Jobs.Job);
end Parser;

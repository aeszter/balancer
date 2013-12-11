with SGE.Jobs;

package Parser is
   Security_Error : exception;
   Resource_Selector : constant String := "-F h_rt,eth,ib,ibs,ssd,gpu,mem_total,num_proc,cm,q";

   procedure Alter_Job (Job                : SGE.Jobs.Job;
                        Insecure_Resources : String := "";
                        Slots              : String := "");
   procedure Add_Pending_Since (J : SGE.Jobs.Job);
end Parser;

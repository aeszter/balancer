with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings;

package JSV is
   type States is (undefined, starting, started, calculating, result_sent);
   type Server_Commands is (start, param, verify, env, quit);
   type JSV_Commands is (send_env, started, param, result, error, log, env);
   type Modifiers is (add, modify, delete);
   type Log_Level is (info, warning, error);

   procedure Main_Loop;

private

   procedure Init_Job_Data;
   --  reset all data fields to sensible default values
   --  in preparation for a new job to be checked
   procedure Get_Next_Command (Command : out Server_Commands;
                               Parameter : out Unbounded_String;
                               Value : out Unbounded_String;
                               Modifier  : out Modifiers);
   --  Get one command from the Master
   procedure Send (Command : JSV_Commands;
                   Param   : String := "";
                   Value   : String := "");
   --  Send one command to the master
   procedure Log (Message : String;
                  Level   : Log_Level := info);
   --  Log a message to the master; this is ignored if the master is not listening
   procedure Error (Message : String);
   --  Log an error; this goes to the master if it is listening, or to stderr otherwise
   procedure Handle_Incoming_Parameter  (Parameter : String; Value : String);
   --  store an incoming parameter (and its value), or ignore it, as needed
   procedure Accept_Job;
   --  tell the master that the job is to be accepted as is
   procedure Change_Job;
   --  tell the master that the job is to be accepted with (already given) changes
   procedure Reject_Job (Message : String);
   --  tell the master that the job is to be rejected

   Not_Ready_Error : exception;
   --  thrown when something happens in the wrong state
end JSV;

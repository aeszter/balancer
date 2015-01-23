with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Ada.Strings;

package JSV is
   type States is (undefined, starting, started, calculating, result_sent);
   type Server_Commands is (start, param, verify, env, quit);
   type JSV_Commands is (send_env, started, param, result, error, log, env);
   type Modifiers is (add, modify, delete);
   type Log_Level is (info, warning, error);

   package Resource_Names is new Generic_Bounded_Length (Max => 10);
   subtype Resource_Name is Resource_Names.Bounded_String;
   function To_Key (Source : String; Drop : Ada.Strings.Truncation := Ada.Strings.Error)
                    return Resource_Name
                    renames Resource_Names.To_Bounded_String;
   package Parameter_Lists is new Ordered_Maps (Key_Type     => Resource_Name,
                                                Element_Type => Unbounded_String,
                                                "<"          => Resource_Names."<");
   subtype Parameter_List is Parameter_Lists.Map;

   Queue_Key : constant Resource_Name := To_Key ("q");
   Infiniband_Key : constant Resource_Name := To_Key ("infiniband");

   procedure Main_Loop;

   function To_String (Source : Parameter_List) return String;

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
   procedure Parse_Parameters (Params : in out Parameter_List; Input : String);
   --  Parse a string of comma-separated parameters into a proper Ordered_Map
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

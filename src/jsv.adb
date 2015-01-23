with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with JSV; use JSV.Parameter_Lists; use JSV.Resource_Names;

package body JSV is
   State : States := undefined;

   procedure Accept_Job is
   begin
      Send (Command => result, Param => "STATE ACCEPT", Value => "Accepted");
   end Accept_Job;

   procedure Change_Job is
   begin
      Send (Command => result, Param => "STATE CORRECT", Value => "Accepted with corrections");
   end Change_Job;

   procedure Decide_On_Job;

   -------------------
   -- Decide_On_Job --
   -------------------

   procedure Decide_On_Job is
   begin
      null;
   end Decide_On_Job;

   procedure Error (Message : String) is
      procedure Error_To_Server;
      procedure Error_To_Stderr;

      procedure Error_To_Server is
      begin
         Put_Line ("ERROR " & Message);
      end Error_To_Server;

      procedure Error_To_Stderr is
      begin
         Put_Line (File => Standard_Error,
                   Item => "Error: " & Message);
         raise Program_Error;
      end Error_To_Stderr;

   begin
      case State is
         when undefined =>
            Error_To_Stderr;
         when starting =>
            Error_To_Server;
         when started =>
            Error_To_Stderr;
         when result_sent =>
            Error_To_Stderr;
         when calculating =>
            Error_To_Server;
      end case;
   end Error;

   procedure Get_Next_Command (Command : out Server_Commands;
                               Parameter : out Unbounded_String;
                               Value : out Unbounded_String;
                               Modifier  : out Modifiers) is
   begin
      if State = starting or else
        State = calculating then
         Error (Message => "Get_Next_Command called in state """ & State'Img & """");
      else
         declare
            Line : constant String := Get_Line;
            Next_Index, Prev_Index : Natural := 1;
         begin
            Next_Index := Index (Source => Line, From => Prev_Index, Pattern => " ");
            if Next_Index = 0 then
               Next_Index := Line'Last + 1;
            end if;
            if Line (Prev_Index .. Next_Index - 1) = "BEGIN" then
               Command := verify;
            else
               Command := Server_Commands'Value (Line (Prev_Index .. Next_Index - 1));
            end if;
            Prev_Index := Next_Index + 1;
            case Command is
               when start =>
                  Modifier := add; -- ignore
                  return; -- no parameters
               when verify =>
                  return; -- no parameters
               when quit =>
                  return; -- no parameters
               when param =>
                  Next_Index := Index (Source => Line, From => Prev_Index, Pattern => " ");
                  Parameter := To_Unbounded_String (Line (Prev_Index .. Next_Index - 1));
                  Value := To_Unbounded_String (Line (Next_Index + 1 .. Line'Last));
               when env =>
                  Next_Index := Index (Source => Line, From => Prev_Index, Pattern => " ");
                  Modifier := Modifiers'Value (Line (Prev_Index .. Next_Index - 1));
                  Prev_Index := Next_Index;
                  Next_Index := Index (Source => Line, From => Prev_Index, Pattern => " ");
                  Parameter := To_Unbounded_String (Line (Prev_Index .. Next_Index - 1));
                  Value := To_Unbounded_String (Line (Next_Index .. Line'Last));
            end case;
         exception
            when E : Constraint_Error =>
               Put_Line (Standard_Error, Exception_Message (E) & " at " & Line (Prev_Index .. Next_Index - 1));
         end;
      end if;
   end Get_Next_Command;

   procedure Handle_Incoming_Parameter  (Parameter : String; Value : String) is
   begin
      if Parameter = "l_hard" then
         Parse_Parameters (Hard, Value);
      elsif Parameter = "q_hard" then
         Hard.Insert (Key => Queue_Key, New_Item => To_Unbounded_String (Value));
      elsif Parameter = "l_soft" then
         Parse_Parameters (Soft, Value);
      elsif Parameter = "pe_name" then
         Parallel_Environment := To_Unbounded_String (Value);
      elsif Parameter = "pe_min" then
         Min_Slots := Positive'Value (Value);
      elsif Parameter = "pe_max" then
         Max_Slots := Positive'Value (Value);
      elsif Parameter = "R" then
         if Value = "y" then
            Reserve := True;
         elsif Value = "n" then
            Reserve := False;
         else
            Error ("Could not parse reservation: " & Value);
         end if;
      end if;
   end Handle_Incoming_Parameter;

   procedure Init_Job_Data is
   begin
      Hard.Clear;
      Soft.Clear;
      Parallel_Environment := Null_Unbounded_String;
      Min_Slots := 1;
      Max_Slots := 1;
      Reserve := False;
   end Init_Job_Data;

   procedure Log (Message : String;
                  Level   : Log_Level := info) is

      procedure Log_To_Stderr;
      procedure Log_To_Server;

      procedure Log_To_Server is
      begin
         Put_Line ("LOG " & Level'Img & " " & Message);
      end Log_To_Server;

      procedure Log_To_Stderr is
      begin
         null;
         --  output to stderr seems to prevent the job from being accepted
         --  Put_Line (File => Standard_Error,
         --           Item => Level'Img & ": " & Message);
      end Log_To_Stderr;

   begin
      case State is
         when undefined =>
            Log_To_Stderr;
         when starting =>
            Log_To_Server;
         when started =>
            Log_To_Stderr;
         when result_sent =>
            Log_To_Stderr;
         when calculating =>
            Log_To_Server;
      end case;
   end Log;

   procedure Main_Loop is
      Cmd : Server_Commands;
      Val, Parameter : Unbounded_String;
      Modifier       : Modifiers;
   begin
      loop
         Get_Next_Command (Command     => Cmd,
                           Parameter   => Parameter,
                           Value       => Val,
                           Modifier    => Modifier);
         case Cmd is
            when start =>
               if State = undefined or else
                 State = result_sent then
                  State := starting;
                  Init_Job_Data;
                  Send (Command => started);
                  State := started;
               else
                  Error ("Got ""START"" in state """ & State'Img & """");
                  State := undefined; -- should never get here
               end if;
            when param =>
               if State = started then
                  Handle_Incoming_Parameter (Parameter => To_String (Parameter),
                                             Value     => To_String (Val));
               else
                  Error ("Got ""PARAM"" in state """ & State'Img & """");
               end if;
            when verify =>
               if State = started then
                  State := calculating;
               else
                  Error ("Got ""BEGIN"" in state """ & State'Img & """");
               end if;
            when env =>
               if State = started then
                  null; -- ignore for now
               else
                  Error ("Got ""ENV"" in state """ & State'Img & """");
               end if;
            when quit =>
               exit;
         end case;
         if State = calculating then
            Decide_On_Job;
            State := result_sent;
         end if;
      end loop;
   exception
      when E : others =>
         Put_Line (Standard_Error, "Unhandled Exception: " & Exception_Message (E));
   end Main_Loop;

   procedure Parse_Parameters (Params : in out Parameter_List; Input : String) is
      Index_List : array (1 .. 256) of Natural;
      Next_Index : Natural := 1;
      Equals : Natural;
   begin
      Index_List (Next_Index) := 1;
      while Index_List (Next_Index) < Input'Last loop
         Next_Index := Next_Index + 1;
         Index_List (Next_Index) := 1 + Index (Input (Index_List (Next_Index - 1) .. Input'Last), ",");
         if Index_List (Next_Index) = 1 then
            Index_List (Next_Index) := Input'Last + 2;
         end if;
         Equals := Index (Input (Index_List (Next_Index - 1) .. Index_List (Next_Index) - 2), "=");
         Params.Insert (Key      => To_Bounded_String (Input (Index_List (Next_Index - 1) .. Equals - 1)),
                        New_Item => To_Unbounded_String (Input (Equals + 1 .. Index_List (Next_Index) - 2)));
      end loop;
   end Parse_Parameters;

   function Queue return String is
   begin
      if State /= calculating then
         raise Not_Ready_Error;
      else
         return To_String (Hard.Element (Key => Queue_Key));
      end if;
   exception
      when Constraint_Error =>
         return "*"; -- no specification found, so any queue is fine
   end Queue;

   procedure Reject_Job (Message : String) is
   begin
      Send (Command => result, Param => "STATE REJECT", Value => Message);
   end Reject_Job;

   procedure Send (Command : JSV_Commands;
                   Param   : String := "";
                   Value   : String := "") is
   begin
      Put_Line (Command'Img & " " & Param & " " & Value);
   end Send;

   function To_String (Source : Parameter_List) return String is
      Item : Parameter_Lists.Cursor := Source.First;
      S : Unbounded_String;
   begin
      while Item /= Parameter_Lists.No_Element loop
         S := S & To_String (Key (Item)) & "=" & Element (Item);
         if Item /= Source.Last then
            S := S & ",";
         end if;
         Next (Item);
      end loop;
      return To_String (S);
   end To_String;

end JSV;


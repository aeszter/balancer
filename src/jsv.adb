with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;
with Jobs; use Jobs;
with Resources;
with Sanitiser;
with Utils;
with SGE.Utils;

package body JSV is
   State : States := undefined;
   J     : Changed_Job;

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
      Sanitiser.Apply_Rules (J);
      if Is_Changed (J) then
         Send_Changes;
         Change_Job;
      else
         Accept_Job;
      end if;
   exception
      when E : others =>
         Utils.Error_Message (Exception_Message (E) & " while processing job " & Get_ID (J));
         Utils.Error_Message ("accepting job");
         Accept_Job;
   end Decide_On_Job;

   procedure Get_Next_Command (Command : out Server_Commands;
                               Parameter : out Unbounded_String;
                               Value : out Unbounded_String;
                               Modifier  : out Modifiers) is
   begin
      if State = starting or else
        State = calculating then
         Utils.Error_Message ("Get_Next_Command called in state """ & State'Img & """");
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
               Utils.Error_Message (Exception_Message (E) & " at " & Line (Prev_Index .. Next_Index - 1));
         end;
      end if;
   end Get_Next_Command;

   procedure Handle_Incoming_Parameter  (Parameter : String; Value : String) is
   begin
      if Parameter = "l_hard" then
         Set_Resources (J, Value);
      elsif Parameter = "q_hard" then
         Utils.Verbose_Message ("Hard queue " & Value & " ignored: not implemented");
      elsif Parameter = "l_soft" then
         Utils.Verbose_Message ("Soft resources " & Value & " ignored: not implemented");
      elsif Parameter = "pe_name" then
         Set_PE (J, To_Unbounded_String (Value));
      elsif Parameter = "pe_min" then
         Set_Slots_Min (J, Positive'Value (Value));
      elsif Parameter = "pe_max" then
         Set_Slots_Max (J, Positive'Value (Value));
      elsif Parameter = "R" then
         if Value = "y" then
            Set_Reservation (J, True);
         elsif Value = "n" then
            Set_Reservation (J, False);
         else
            Utils.Error_Message ("Could not parse reservation: " & Value);
         end if;
      elsif Parameter = "N" then
         Set_Name (J, Value);
      end if;
   end Handle_Incoming_Parameter;

   procedure Init_Job_Data is
   begin
      J := Empty_Job;
   end Init_Job_Data;

   procedure Main_Loop is
      Cmd : Server_Commands;
      Val, Parameter : Unbounded_String;
      Modifier       : Modifiers;
   begin
      Sanitiser.Init;
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
                  Utils.Error_Message ("Got ""START"" in state """ & State'Img & """");
                  State := undefined; -- should never get here
               end if;
            when param =>
               if State = started then
                  Handle_Incoming_Parameter (Parameter => To_String (Parameter),
                                             Value     => To_String (Val));
               else
                  Utils.Error_Message ("Got ""PARAM"" in state """ & State'Img & """");
               end if;
            when verify =>
               if State = started then
                  State := calculating;
               else
                  Utils.Error_Message ("Got ""BEGIN"" in state """ & State'Img & """");
               end if;
            when env =>
               if State = started then
                  null; -- ignore for now
               else
                  Utils.Error_Message ("Got ""ENV"" in state """ & State'Img & """");
               end if;
            when quit =>
               exit;
         end case;
         if State = calculating then
            Set_Old_State (J, Jobs.any);
            Set_New_State (J);
            Freeze (J);
            Decide_On_Job;
            State := result_sent;
         end if;
      end loop;
   exception
      when E : others =>
         Utils.Error_Message ("Unhandled Exception: " & Exception_Message (E));
   end Main_Loop;

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

   procedure Send_Changes is
      use SGE.Utils;

      PE : constant String := Get_PE (J);
   begin
      Utils.Verbose_Message ("Applying changes to job " & Get_Name (J));
      if PE /= "" then
         Send (Command => param,
               Param   => "pe_name",
               Value   => PE);
         Send (Command => param,
               Param   => "pe_min",
               Value   => Get_Slots (J).Min'Img);
         Send (Command => param,
               Param   => "pe_max",
               Value   => Get_Slots (J).Max'Img);
      end if;
      if Get_Reservation (J) = True then
         Send (Command => param,
               Param   => "R",
               Value   => "y");
      elsif Get_Reservation (J) = False then
         Send (Command => param,
               Param   => "R",
               Value   => "n");
      end if;
      Send (Command => param,
            Param   => "l_hard",
            Value   => Resources.To_Requirement (Get_Resources (J)));
   end Send_Changes;

end JSV;


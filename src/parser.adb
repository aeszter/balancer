with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Parser;
with SGE.Spread_Sheets;
with SGE.Jobs; use SGE.Jobs;
with Jobs;
with Utils;
with SGE.Context;

package body Parser is

   procedure Add_Pending_Since (J : Job) is
      function Is_OK (Input : String) return Boolean;

      Output : SGE.Spread_Sheets.Spread_Sheet;
      Add    : constant Trusted_String := Sanitise (Get_ID (J))
                 & Implicit_Trust (" -ac PENDINGSINCE=")
                 & Sanitise (Utils.Now);
      Remove : constant Trusted_String := Sanitise (Get_ID (J))
                 & Implicit_Trust (" -dc PENDINGSINCE");
      Existing : constant String := Get_Context (J, SGE.Context.Pending_Since);
      Exit_Status : Natural;
      pragma Unreferenced (Output);
      -- Can we do something useful with the output?

      function Is_OK (Input : String) return Boolean is
         Dummy : Integer;
         pragma Unreferenced (Dummy);
      begin
         Dummy := Positive'Value (Input);
         return True;
      exception
         when others =>
            return False;
      end Is_OK;

   begin
      if On_Hold (J) then
         return;
      end if;
      if Existing = "" then
         if not Utils.Dry_Run ("qalter " & Value (Add)) then
            SGE.Parser.Setup_No_XML (Command     => Trust_As_Command ("qalter"),
                                     Subpath     => Implicit_Trust ("/bin/linux-x64/"),
                                     Selector    => Add,
                                     Output      => Output,
                                     Exit_Status => Exit_Status);
            case Exit_Status is
               when 0 => null; -- OK
               when 1 => Utils.Verbose_Message ("Exit Status 1, evaluate output (Bug #1849)");
               when others =>
                  Utils.Error_Message ("qalter exited with status" & Exit_Status'Img
                                       & ". This is a bug in the balancer because it is "
                                       & "unhandled in Parser.Add_Pending_Since.");
            end case;

         end if;
      elsif not Is_OK (Existing) then
         Utils.Verbose_Message ("removing corrupt timestamp");
         if not Utils.Dry_Run ("qalter "  & Value (Remove)) then
            SGE.Parser.Setup_No_XML (Command     => Trust_As_Command ("qalter"),
                                     Subpath     => Implicit_Trust ("/bin/linux-x64/"),
                                     Selector    => Remove,
                                     Output      => Output,
                                     Exit_Status => Exit_Status);
            case Exit_Status is
               when 0 => null; -- OK
               when 1 => Utils.Verbose_Message ("Exit Status 1, evaluate output (Bug #1849)");
               when others =>
                  Utils.Error_Message ("qalter exited with status" & Exit_Status'Img
                                       & ". This is a bug in the balancer because it is "
                                       & "unhandled in Parser.Add_Pending_Since.");
            end case;
         end if;
      end if;
   exception
      when E : SGE.Parser.Parser_Error =>
         Ada.Text_IO.Put_Line ("Could not timestamp job " & Get_ID (J));
         Utils.Verbose_Message (Exception_Message (E));
      when E : others =>
         Ada.Text_IO.Put_Line ("Unknown error in Parser.Add_Pending_Since (" & Get_ID (J) & "): ");
         Ada.Text_IO.Put_Line (Exception_Message (E));
   end Add_Pending_Since;

   ---------------
   -- Alter_Job --
   ---------------

   procedure Alter_Job
     (ID                 : Positive;
                        Insecure_Resources : String := "";
                        PE                 : String := "";
                        Slots              : String := "";
                        Reservation        : Tri_State := Undecided;
      Timestamp_Name     : Trusted_String)
   is
      function PE_String return Trusted_String;
      function Reserve_String return Trusted_String;
      function Resource_String return Trusted_String;

      ID_String : constant Trusted_String := Sanitise (Ada.Strings.Fixed.Trim (Source => ID'Img,
                                                                               Side   => Ada.Strings.Left));
      Output       : SGE.Spread_Sheets.Spread_Sheet;
      Timestamp    : constant Trusted_String := Implicit_Trust (" -ac ")
                       & Timestamp_Name & Implicit_Trust ("=") & Sanitise (Utils.Now);
      Exit_Status  : Natural;

      function PE_String return Trusted_String is
      begin
         if PE = "" then
            return Implicit_Trust ("");
         else
            return Implicit_Trust (" -pe ") & Sanitise (PE) & Implicit_Trust (" ") & Sanitise (Slots);
         end if;
      end PE_String;

      function Reserve_String return Trusted_String is
      begin
         case Reservation is
         when True =>
            return Implicit_Trust (" -R y");
         when False =>
            return Implicit_Trust (" -R n");
         when Undecided =>
            return Implicit_Trust ("");
         end case;
      end Reserve_String;

      function Resource_String return Trusted_String is
      begin
         if Insecure_Resources /= "" then
            return Implicit_Trust (" -l ") & Sanitise (Insecure_Resources);
         else
            return Implicit_Trust ("");
         end if;
      end Resource_String;


   begin
      if Slots /= "" and then PE = "" then
         raise Jobs.Support_Error with "no PE found";
      end if;
      if PE /= "" and then Slots = "" then
         raise Jobs.Support_Error with "no slot range found";
      end if;
      if not Utils.Dry_Run ("qalter " & Value (ID_String & PE_String
                            & Resource_String & Reserve_String & Timestamp))
      then
         SGE.Parser.Setup_No_XML (Command => Trust_As_Command ("qalter"),
                                  Subpath => Implicit_Trust ("/bin/linux-x64/"),
                                  Selector => ID_String & PE_String
                                  & Resource_String & Reserve_String & Timestamp,
                                  Output      => Output,
                                  Exit_Status => Exit_Status);
         Output.Rewind;
         if Output.At_Separator then
            Output.Next;
         end if;
         case Exit_Status is
            when 0 => null; -- OK
            when 1 =>
               declare
                  Message : constant String := Output.Current;
                  Modified_Context : constant String := "modified context of job";
                  Length : constant Positive := Modified_Context'Length;
               begin
                  if Message = "denied: former resource request on consumable "
                    & """gpu"" of running job lacks in new resource request"
                  then
                     null; -- expected message even for qw jobs
                  elsif Message = "denied: resource request on consumable "
                    & """gpu"" of running job was not contained former resource request"
                  then
                     -- typo (missing "in") is part of qalter
                     null; -- expected message even for qw jobs
                  elsif Message (Message'First .. Message'First + Length - 1) = Modified_Context then
                     null; -- signifies success
                  else
                     Utils.Verbose_Message ("Exit Status 1, evaluate output (Bug #1849)");
                     Utils.Error_Message ("#" & Message & "#");
                  end if;
               exception
                  when others =>
                     Utils.Error_Message ("Unable to handle qalter exit status");
                     raise;
               end;
            when others =>
               Utils.Error_Message ("qalter exited with status" & Exit_Status'Img
                                    & ". This is a bug in the balancer because it is "
                                    & "unhandled in Parser.Alter_Job.");
         end case;
      end if;
   exception
      when E : SGE.Parser.Parser_Error =>
         Ada.Text_IO.Put_Line ("Could not alter job" & ID'Img);
         Utils.Verbose_Message ("#" & Exception_Message (E) & "#");
      when E : others =>
         Ada.Text_IO.Put_Line ("Unknown error in Parser.Alter_Job (" & ID'Img & "): ");
         Ada.Text_IO.Put_Line (Exception_Message (E));
   end Alter_Job;

end Parser;

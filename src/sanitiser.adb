with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Regpat;
with SGE.Utils;
with RulesP; with RulesS;
with Utils;

package body Sanitiser is

   Rules_File : constant String := "/etc/balancer.rules";

   procedure Apply_Branch_Chain (J : in out Changed_Job; Instructions : Branch_Chain);
   function Evaluate (J : Changed_Job; Condition : Operation_List) return Boolean;
   procedure Apply (J : in out Changed_Job; Action : Operation_List);

   procedure Add_Rule (R : Rule) is
   begin
      Rules.Append (R);
   end Add_Rule;

   procedure Append (Container : in out Operation_List; New_Item : Operation) is
   begin
      Operation_Lists.Append (Operation_Lists.List (Container), New_Item);
   end Append;

   procedure Append (Container : in out Branch_Chain; New_Item : Decision) is
   begin
      Decision_Chains.Append (Decision_Chains.List (Container), New_Item);
   end Append;

   procedure Apply (J : in out Changed_Job; Action : Operation_List) is
      procedure Apply_Operation (Position : Operation_Lists.Cursor := Action.First);
      procedure Apply_To_PE (Op : Operator; Value : Multitype);
      procedure Apply_To_Resources (Op : Operator; Value : Multitype);
      procedure Apply_To_Slots (Op : Operator; Value : Multitype);
      procedure Apply_To_Reservation (Op : Operator; Value : Multitype);

      procedure Apply_Operation (Position : Operation_Lists.Cursor := Action.First) is
         O : constant Operation := Operation_Lists.Element (Position);
      begin
         case O.Object is
            when PE =>
               if O.Value.Switch /= str then
                  raise Rule_Error with O.Value.Switch'Img
                    & " encountered for PE Object";
               end if;
               Apply_To_PE (O.Oper, O.Value);
            when Resources =>
               if O.Value.Switch /= str then
                  raise Rule_Error with O.Value.Switch'Img
                    & "encountered for Resource Object";
               end if;
               Apply_To_Resources (O.Oper, O.Value);
            when Slots =>
               if O.Value.Switch /= slots then
                  raise Rule_Error with O.Value.Switch'Img
                    & " encountered for Slots Object";
               end if;
               Apply_To_Slots (O.Oper, O.Value);
            when Reservation =>
               if O.Value.Switch /= bool then
                  raise Rule_Error with O.Value.Switch'Img
                    & " encountered for Reservation Object";
               end if;
               Apply_To_Reservation (O.Oper, O.Value);
         end case;
      end Apply_Operation;

      procedure Apply_To_PE (Op : Operator; Value : Multitype) is
      begin
         case Op is
            when assign =>
               Set_PE (J, Value.String_Value);
            when others =>
               raise Rule_Error with "unsupported operator " & Op'Img &
               " in Apply_to_PE";
         end case;
      end Apply_To_PE;

      procedure Apply_To_Reservation (Op : Operator; Value : Multitype) is
      begin
         case Op is
            when assign =>
               Set_Reservation (J, Value.Bool_Value);
            when others =>
               raise Rule_Error with "unsupported operator " & Op'Img &
               " in Apply_to_Reservation";
         end case;
      end Apply_To_Reservation;

      procedure Apply_To_Resources (Op : Operator; Value : Multitype) is
      begin
         case Op is
            when assign =>
               Set_Resources (J, To_String (Value.String_Value));
            when add =>
               Add_Resource (J, Get_String (Value));
            when remove =>
               Remove_Resource (J, Value.String_Value);
            when others =>
               raise Rule_Error with "unsupported operator " & Op'Img &
               " in Apply_to_Resources";
         end case;
      end Apply_To_Resources;

      procedure Apply_To_Slots (Op : Operator; Value : Multitype) is
      begin
         case Op is
            when assign =>
               Set_Slots (J, Value.Slot_Value);
            when others =>
               raise Rule_Error with "unsupported operator " & Op'Img &
               " in Apply_to_Slots";
         end case;
      end Apply_To_Slots;

   begin
      Action.Iterate (Apply_Operation'Access);
   end Apply;

   procedure Apply_Branch_Chain (J : in out Changed_Job; Instructions : Branch_Chain) is
      use Decision_Chains;
      Position : Decision_Chains.Cursor := Instructions.First;
      This_Decision : Decision;
   begin
      while Position /= Decision_Chains.No_Element loop
         This_Decision := Element (Position);
         if Evaluate (J, This_Decision.Condition) then
            Apply (J, This_Decision.Action);
            exit;
         end if;
         Next (Position);
      end loop;
   end Apply_Branch_Chain;

   procedure Apply_Rules (J : in out Changed_Job) is
      ID : Natural := 0;

      procedure Apply_Rule (Position : Rule_Lists.Cursor);

      procedure Apply_Rule (Position : Rule_Lists.Cursor) is
         The_Rule : constant Rule := Rule_Lists.Element (Position);
      begin
         if Jobs.Match (J         => J,
                        Old_State => The_Rule.From,
                        New_State => The_Rule.To) then
            Utils.Trace ("Found a match: " & To_String (The_Rule.Name));
            Apply_Branch_Chain (J, The_Rule.Contents);
         end if;
      end Apply_Rule;
   begin
      ID := Get_ID (J); -- do not move to declarative part:
                        -- we could miss exceptions raised by Get_ID.
      Utils.Trace ("Applying rules to" & ID'Img);
      Rules.Iterate (Apply_Rule'Access);
   exception
      when E: others =>
         if ID = 0 then
            Utils.Error_Message ("Exception encountered while applying rules to defective job: "
                                 & Exception_Message (E));
         else
            Utils.Error_Message ("Exception encountered while applying rules to job"
                                 & ID'Img & ": "
                                 & Exception_Message (E));
         end if;
   end Apply_Rules;

   function Check_PE (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean is
      use GNAT.Regpat;
   begin
      if Value.Switch /= str then
         raise Program_Error with "Check_PE called with non-string literal";
      end if;
      case Op is
         when equal
            => return Get_PE (J) = Value.String_Value;
         when matches
            => return GNAT.Regpat.Match (Expression => To_String (Value.String_Value),
                                         Data       => Get_PE (J));
         when others
            => raise Rule_Error with Op'Img & "encountered when comparing PEs";
      end case;
   end Check_PE;

   function Check_Reservation (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean is
      use SGE.Utils;
   begin
      if Op /= equal then
         raise Rule_Error with "unexpected operator" & Op'Img & " when comparing reservation";
      end if;
      if Value.Switch /= bool then
         raise Program_Error with "Check_Reservation called with non-boolean literal";
      end if;
      case Get_Reservation (J) is
         when Tri_State'(True)
            => return Value.Bool_Value;
         when False
            => return not Value.Bool_Value;
         when Undecided
            => raise Program_Error with "Job" & Get_ID (J) & " has undecided reservation";
      end case;
   end Check_Reservation;

   function Check_Resources (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean is
   begin
      if Value.Switch /= str then
         raise Program_Error with "Check_Resources called with non-string literal";
      end if;
      case Op is
         when others => Utils.Error_Message ("Check_Resources unimplemented");
      end case;
      return False;
   end Check_Resources;

   function Check_Slots (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean is
      use SGE.Ranges;
   begin
      if Value.Switch /= slots then
         raise Program_Error with "Check_Slots called with non-slots literal";
      end if;
      case Op is
         when subset
            => return SGE.Ranges.Is_Subset (Subset => Value.Slot_Value,
                                            Of_Set => Get_Slots (J));
         when intersects
            => return SGE.Ranges.Intersects (Left => Value.Slot_Value,
                                            Right => Get_Slots (J));
         when equal
            => return Value.Slot_Value = Get_Slots (J);
         when others
            => raise Rule_Error with Op'Img & " encountered when comparing slots";
      end case;
   end Check_Slots;

   overriding procedure Clear (Container : in out Operation_List) is
   begin
      Operation_Lists.Clear (Operation_Lists.List (Container));
   end Clear;

   function Evaluate (J : Changed_Job; Condition : Operation_List) return Boolean is
      use Operation_Lists;
      procedure Evaluate_One (Position : Operation_Lists.Cursor);

      Result : Boolean := True;

      procedure Evaluate_One (Position : Operation_Lists.Cursor) is
         Condition : constant Operation := Element (Position);
      begin
         case Condition.Object is
            when PE
               => Result := Result and then Check_PE (J, Condition.Oper, Condition.Value);
            when Slots
               => Result := Result and then Check_Slots (J, Condition.Oper, Condition.Value);
            when Resources
               => Result := Result and then Check_Resources (J, Condition.Oper, Condition.Value);
            when Reservation
               => Result := Result and then Check_Reservation (J, Condition.Oper, Condition.Value);
         end case;
      end Evaluate_One;

   begin
      Condition.Iterate (Evaluate_One'Access);
      return Result;
   end Evaluate;

   function Get_Name (R : Rule) return String is
   begin
      return To_String (R.Name);
   end Get_Name;

   function Get_String (L : Multitype) return String is
   begin
      if L.Switch /= str then
         raise Rule_Error with "found " & L.Switch'Img & " but str expected";
      end if;
      return To_String (L.String_Value);
   end Get_String;

   procedure Init is

      First : Boolean;

      procedure Print_Decisions (Position : Decision_Chains.Cursor);
      procedure Print_Rule (Position : Rule_Lists.Cursor);
      procedure Print_Operation (Position : Operation_Lists.Cursor);

      procedure Print_Decisions (Position : Decision_Chains.Cursor) is
         D : constant Decision := Decision_Chains.Element (Position);
      begin
         if First then
            Utils.Trace ("if");
         else
            Utils.Trace ("elsif");
         end if;
         if D.Condition.Is_Empty then
            Utils.Trace (" (true) ");
         else
            D.Condition.Iterate (Print_Operation'Access);
         end if;
         Utils.Trace ("then");
         D.Action.Iterate (Print_Operation'Access);
         First := False;
      end Print_Decisions;

      procedure Print_Operation (Position : Operation_Lists.Cursor) is
         O : constant Operation := Operation_Lists.Element (Position);
      begin
         case O.Oper is
            when equal
               => Utils.Trace (O.Object'Img & "=" & To_String (O.Value));
            when matches
               => Utils.Trace (O.Object'Img & "~" & To_String (O.Value));
            when intersects
               => Utils.Trace (O.Object'Img & "^" & To_String (O.Value));
            when subset
               => Utils.Trace (O.Object'Img & "<" & To_String (O.Value));
            when contains
               => Utils.Trace (O.Object'Img & ">" & To_String (O.Value));
            when assign
               => Utils.Trace ("Set " & O.Object'Img & " to " & To_String (O.Value));
            when add
               => Utils.Trace ("Add " & To_String (O.Value) & " to " & O.Object'Img);
            when remove
               => Utils.Trace ("Remove "& To_String (O.Value) & " from " & O.Object'Img);
         end case;
      end Print_Operation;

      procedure Print_Rule (Position : Rule_Lists.Cursor) is
         R : constant Rule := Rule_Lists.Element (Position);
      begin
         Utils.Trace (Get_Name (R) & ": " & R.From'Img & " -> " & R.To'Img);
         First := True;
         R.Contents.Iterate (Print_Decisions'Access);
      end Print_Rule;

   begin
      FileIO.Open (RulesS.Source, Rules_File, False);
      RulesP.Parse;
      Rules.Iterate (Print_Rule'Access);
   exception
      when E : others =>
         Put_Line ("Could not read " & Rules_File & ". Jobs will not be sanitised.");
         Put_Line (File => Standard_Error,
                  Item => Exception_Name (E) & ": " & Exception_Message (E));
   end Init;

   procedure Set_Action (Dest : in out Decision; Action : Operation_List) is
   begin
      Dest.Action := Action;
   end Set_Action;

   procedure Set_Bool (L : out Multitype; Value : Boolean) is
   begin
      L.Switch := bool;
      L.Bool_Value := Value;
   end Set_Bool;

   procedure Set_Condition (Dest : in out Decision; Condition : Operation_List) is
   begin
      Dest.Condition := Condition;
   end Set_Condition;

   procedure Set_Contents (R : in out Rule; Content : Branch_Chain) is
   begin
      R.Contents := Content;
   end Set_Contents;

   procedure Set_From (R : in out Rule; S : State) is
   begin
      R.From := S;
   end Set_From;

   procedure Set_Literal (Dest : in out Operation; Lit : Multitype) is
   begin
      Dest.Value := Lit;
   end Set_Literal;

   procedure Set_Name (R : in out Rule; Name : FileIO.String_Ptr) is
   begin
      Set_Unbounded_String (Target => R.Name,
                            Source => Name.all);
   end Set_Name;

   procedure Set_Operator (Dest : in out Operation; Op : Operator) is
   begin
      Dest.Oper := Op;
   end Set_Operator;

   procedure Set_Property (Dest : in out Operation; Prop : Kind) is
   begin
      Dest.Object := Prop;
   end Set_Property;

   procedure Set_Slots (L : out Multitype; Value : Step_Range_List) is
   begin
      L.Switch := slots;
      L.Slot_Value := Value;
   end Set_Slots;

   procedure Set_String (L : out Multitype; Value : String) is
   begin
      if Value (Value'First) /= '"' or else
        Value (Value'Last) /= '"' then
         raise Rule_Error with "String not properly quoted: "
           & Value;
      end if;
      L.Switch := str;
      L.String_Value := To_Unbounded_String (Value (Value'First + 1 .. Value'Last - 1));
   end Set_String;

   procedure Set_To (R : in out Rule; S : State) is
   begin
      R.To := S;
   end Set_To;

   function To_String (Lit : Multitype) return String is
   begin
      case Lit.Switch is
         when bool => return Lit.Bool_Value'Img;
         when slots => return SGE.Ranges.To_String (Lit.Slot_Value, Short => True);
         when str => return To_String (Lit.String_Value);
         when undefined => raise Program_Error with "Literal with undefined type encountered";
      end case;
   end To_String;

end Sanitiser;

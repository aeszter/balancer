with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with RulesP; with RulesS;
with Utils;

package body Sanitiser is

   Rules_File : constant String := "/etc/balancer.rules";

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

   procedure Apply_Rules (J : Changed_Job) is
      procedure Apply_Rule (Position : Rule_Lists.Cursor);

      procedure Apply_Rule (Position : Rule_Lists.Cursor) is
         The_Rule : constant Rule := Rule_Lists.Element (Position);
      begin
         if Jobs.Match (J         => J,
                        Old_State => The_Rule.From,
                        New_State => The_Rule.To) then
            Utils.Trace ("Found a match: " & To_String (The_Rule.Name));
            Utils.Error_Message ("Further checking and application still unimplemented");
         end if;
      end Apply_Rule;
   begin
      Utils.Trace ("Applying rules to" & Get_ID (J));
      Rules.Iterate (Apply_Rule'Access);
   end Apply_Rules;

   function Get_Name (R : Rule) return String is
   begin
      return To_String (R.Name);
   end Get_Name;

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
         D.Condition.Iterate (Print_Operation'Access);
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
      L.Switch := str;
      L.String_Value := To_Unbounded_String (Value);
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

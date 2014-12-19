with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Ranges;
with Jobs; use Jobs;
with FileIO;

package Sanitiser is

   Rule_Error : exception;

   use type FileIO.String_Ptr;
   subtype String_Ptr is FileIO.String_Ptr;
   subtype Step_Range is SGE.Ranges.Step_Range;
   subtype Step_Range_List is SGE.Ranges.Step_Range_List;

   type Kind is (PE, Slots, Reservation, Resources);
   type Operator is (equal, matches, intersects, subset, contains,
                     assign, add, remove);

   type Multitype is private;
   procedure Set_String (L : out Multitype; Value : String);
   procedure Set_Bool (L : out Multitype; Value : Boolean);
   procedure Set_Slots (L : out Multitype; Value : Step_Range_List);

   type Operation is private;
   procedure Set_Property (Dest : in out Operation; Prop : Kind);
   procedure Set_Operator (Dest : in out Operation; Op : Operator);
   procedure Set_Literal (Dest : in out Operation; Lit : Multitype);

   type Operation_List is tagged private;
   procedure Append (Container : in out Operation_List; New_Item : Operation);
   procedure Clear (Container : in out Operation_List);

   always : constant Operation_List;

   type Decision is private;
   procedure Set_Condition (Dest : in out Decision; Condition : Operation_List);
   procedure Set_Action (Dest : in out Decision; Action : Operation_List);

   type Branch_Chain is tagged private;
   procedure Append (Container : in out Branch_Chain; New_Item : Decision);

   type Rule is private;
   procedure Add_Rule (R : Rule);
   procedure Set_Name (R : in out Rule; Name : String_Ptr);
   function Get_Name (R : Rule) return String;
   procedure Set_From (R : in out Rule; S : State);
   procedure Set_To (R : in out Rule; S : State);
   procedure Set_Contents (R : in out Rule; Content : Branch_Chain);

   procedure Init;
   procedure Apply_Rules (J : in out Changed_Job);

   function Check_PE (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean;
   function Check_Slots (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean;
   function Check_Resources (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean;
   function Check_Reservation (J : Changed_Job; Op : Operator; Value : Multitype) return Boolean;


private

   type Value_Selector is (undefined, bool, slots, str);
   type Multitype is record
      Switch       : Value_Selector := undefined;
      String_Value : Unbounded_String;
      Bool_Value   : Boolean;
      Slot_Value   : Step_Range_List;
   end record;

   function To_String (Lit : Multitype) return String;


   type Operation is record
      Object : Kind;
      Oper   : Operator;
      Value  : Multitype;
   end record;

   package Operation_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Operation);
   type Operation_List is new Operation_Lists.List with null record;

   type Decision is record
      Condition, Action : Operation_List;
   end record;

   package Decision_Chains is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Decision);
   type Branch_Chain is new Decision_Chains.List with null record;

   type Rule is record
      Name : Unbounded_String;
      From, To : State;
      Contents : Branch_Chain;
   end record;

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Rule);
   Rules : Rule_Lists.List;

   always : constant Operation_List := Operation_List'(Operation_Lists.Empty_List with null record);

end Sanitiser;

with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Users is

   -------------
   -- Add_Job --
   -------------

   procedure Increment (Key : User_Name; Element : in out Positive);

   procedure Increment (Key : User_Name; Element : in out Positive) is
      pragma Unreferenced (Key);
   begin
      Element := Element + 1;
   end Increment;

   procedure Add_Job (J : SGE.Jobs.Job) is
      Key : Index_Card;
      User : constant User_Name := SGE.Jobs.Get_Owner (J);
      Position : Job_Counts.Cursor;
   begin
      if SGE.Jobs.On_Hold (J) then
         return;
      end if;
      Key.User := User;
      if Count.Contains (User) then
         Position := Count.Find (User);
         Key.Counter := Job_Counts.Element (Position) + 1;
         Count.Update_Element (Position => Position, Process => Increment'Access);
      else
         Key.Counter := 1;
         Count.Insert (Key      => User,
                       New_Item => 1);
      end if;
      List.Insert (Key      => Key,
                   New_Item => J);
   end Add_Job;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access procedure (J : SGE.Jobs.Job))
   is
      procedure Wrapper (Position : Job_Lists.Cursor);

      procedure Wrapper (Position : Job_Lists.Cursor) is
         use Ada.Text_IO;
      begin
         Process (Job_Lists.Element (Position));
      exception
         when E : others =>
         -- if we're here, something really bad happened:
         -- Process should have caught any exception that might have been raised
         -- deep inside, so proceed with utmost caution: print a static text first
         -- then something from a simple, local type
         -- proceed to SGElib types last
         Put_Line ("Wrapper caught error while iterating over users");
         Put_Line (Exception_Information (E));
         Put_Line (String (Job_Lists.Key (Position).User));
         Put_Line (SGE.Jobs.Get_ID (Job_Lists.Element (Position)));
      end Wrapper;

   begin
      List.Iterate (Wrapper'Access);
   end Iterate;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Index_Card) return Boolean is
   begin
      if Left.Counter < Right.Counter then
         return True;
      elsif Left.Counter > Right.Counter then
         return False;
      else
         return Left.User < Right.User;
      end if;
   end "<";


   function Total_Users return Natural is
   begin
      return Natural (Count.Length);
   end Total_Users;

   function Count_Jobs (For_User : String) return Natural is
   begin
      return Count.Element (Key => To_User_Name (For_User));
   end Count_Jobs;

end Users;

-------------------------------------------------
--                                             --
--         Parser generated by Coco/R          --
--                                             --
-- (assuming FileIO library will be available) --
-------------------------------------------------

with Jobs;
with Sanitiser; use Sanitiser;
with Utils;
with  -->scanner;

package body -->modulename is

   -->declarations


   --
   -- Report semantic error errNo
   --
   procedure Semantic_Error (errNo: in     Integer)
   is
   begin
      if  errDist >= minErrDist  then
         -->error
      end if;
      errDist := 0;
   end Semantic_Error;


   --
   -- Report syntax error errNo
   --
   procedure Syntax_Error (errNo: in     Integer)
   is
   begin
      if  errDist >= minErrDist  then
         -->error
      end if;
      errDist := 0;
   end Syntax_Error;


   procedure Get
   is
      S: FileIO.String_Ptr;
   begin
      loop
         -->scanner.Get (sym);
         if  sym <= maxT  then
            errDist := errDist + 1;
         else
            -->pragmas
         end if;
         exit when sym <= maxT;
      end loop;
   end Get;


   function Is_In
     (s: in     SymbolSet;
      x: in     CARDINAL)
     return Boolean
   is
   begin
      return  Sets.Is_In ((x mod setsize), s (x / setsize));
   end Is_In;

   procedure Is_In
     (s     : in out SymbolSet;
      x     : in     CARDINAL;
      Result:    out Boolean)
   is
   begin
      Result := Sets.Is_In ((x mod setsize), s (x / setsize));
   end Is_In;


   procedure Expect (n: in     CARDINAL)
   is
   begin
      if  sym = n  then
         Get;
      else
         Syntax_Error (n);
      end if;
   end Expect;


   procedure Expect_Weak
     (n     : CARDINAL;
      follow: CARDINAL)
   is
      Is_In_Result : Boolean;
   begin
      if  sym = n  then
         Get;
      else
         Syntax_Error (n);
   --       while  not Is_In (symSet (follow), sym)  loop
   --          Get;
   --       end loop;
         loop
            Is_In (symSet (follow), sym, Is_In_Result);
            exit when not Is_In_Result;
            Get;
         end loop;
      end if;
   end Expect_Weak;


   function Weak_Separator
     (n     : CARDINAL;
      syFol : CARDINAL;
      repFol: CARDINAL)
     return Boolean
   is
      use type Sets.BitSet;

      s           : SymbolSet;
      I           : CARDINAL;
      Is_In_Result: Boolean;
   begin
      if  sym = n  then
         Get;
         return TRUE;
      else
         Is_In (symSet (repFol), sym, Is_In_Result);
         if  Is_In_Result  then
            return FALSE;
         else
            I := 0;
            while  (I <= (maxT / setsize))  loop
               s (I) := symSet (0) (I) + symSet (syFol) (I) + symSet (repFol) (I);
               I := I + 1;
            end loop;
            Syntax_Error (n);
            loop
               Is_In (s, sym, Is_In_Result);
               exit when not Is_In_Result;
               Get;
            end loop;

            Is_In (symSet (syFol), sym, Is_In_Result);
            return Is_In_Result;
         end if;
      end if;
   end Weak_Separator;


   --
   -- Retrieves Lex as name of current token (capitalized if IGNORE case )
   --
   procedure Lex_Name (Lex:    out FileIO.String_Ptr)
   is
   begin
      -->scanner.Get_Name (-->scanner.pos, -->scanner.len, Lex);
   end Lex_Name;


   --
   -- Retrieves Lex as exact spelling of current token
   --
   procedure Lex_String (Lex:    out FileIO.String_Ptr)
   is
   begin
      -->scanner.Get_String (-->scanner.pos, -->scanner.len, Lex);
   end Lex_String;


   --
   -- Retrieves Lex as exact spelling of lookahead token
   --
   procedure Look_Ahead_Name (Lex:    out FileIO.String_Ptr)
   is
   begin
      -->scanner.Get_Name (-->scanner.nextPos, -->scanner.nextLen, Lex);
   end Look_Ahead_Name;


   --
   -- Retrieves Lex as name of lookahead token (capitalized if IGNORE case )
   --
   procedure Look_Ahead_String (Lex:    out FileIO.String_Ptr)
   is
   begin
      -->scanner.Get_String (-->scanner.nextPos, -->scanner.nextLen, Lex);
   end Look_Ahead_String;


   --
   -- Returns TRUE if no errors have been recorded while parsing
   --
   function Successful return Boolean
   is
   begin
      return -->scanner.errors = 0;
   end Successful;


   -->productions


   procedure Parse
   is
   begin
      -->parseRoot
   end Parse;


begin -- CRP
   errDist := minErrDist;

   -->initialization

end -->modulename;





--------------------------------
--                            --
-- Parser generated by Coco/R --
--                            --
--------------------------------

with  Sets;
with  FileIO;


package -->modulename is

   procedure Parse;

   --
   -- Returns TRUE if no errors have been recorded while parsing
   --
   function Successful return Boolean;

   --
   -- Report syntax error errNo
   --
   procedure Syntax_Error (errNo: in     Integer);

   --
   -- Report semantic error errNo
   --
   procedure Semantic_Error (errNo: in     Integer);

   --
   -- Retrieves Lex as exact spelling of current token
   --
   procedure Lex_String (Lex:    out FileIO.String_Ptr);

   --
   -- Retrieves Lex as name of current token (capitalized if IGNORE CASE)
   --
   procedure Lex_Name (Lex:    out FileIO.String_Ptr);

   --
   -- Retrieves Lex as exact spelling of lookahead token
   --
   procedure Look_Ahead_Name (Lex:    out FileIO.String_Ptr);

   --
   -- Retrieves Lex as name of lookahead token (capitalized if IGNORE CASE)
   --
   procedure Look_Ahead_String (Lex:    out FileIO.String_Ptr);


private

   subtype CARDINAL  is FileIO.CARDINAL;
   subtype INT32     is FileIO.INT32;

--CONST's
   -->constants
   minErrDist  : constant :=  2; -- minimal distance (good tokens) between two errors
   setsize     : constant := 16; -- sets are stored in 16 bits


--TYPE's
   subtype  SymbolSet   is Sets.BitArray (0 .. maxT / setsize);


--VAR's
  symSet    : array (Integer range 0 .. -->symSetSize) of SymbolSet;  -- symSet (0) = allSyncSyms
  errDist   : CARDINAL;    -- number of symbols recognized since last error
  sym       : CARDINAL;    -- current input symbol

end -->modulename;





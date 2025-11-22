-- CLI - Command Line Interface Package Specification
-- Handles command-line argument parsing

package CLI is

   -- Maximum string lengths
   Max_String_Length : constant := 1024;

   -- Bounded strings for options
   subtype Bounded_String is String (1 .. Max_String_Length);

   -- Options record
   type Options_Type is record
      Show_Help      : Boolean := False;
      Show_Version   : Boolean := False;
      Interactive    : Boolean := True;
      Config_File    : Bounded_String := (others => ' ');
      Config_Set     : Boolean := False;
      Repo_Name      : Bounded_String := (others => ' ');
      Repo_Set       : Boolean := False;
      File_Path      : Bounded_String := (others => ' ');
      File_Set       : Boolean := False;
      Description    : Bounded_String := (others => ' ');
      Desc_Set       : Boolean := False;
      License        : Bounded_String := (others => ' ');
      License_Set    : Boolean := False;
      No_Timestamp   : Boolean := False;
      No_Push        : Boolean := False;
   end record;

   -- Parse command-line arguments
   procedure Parse_Arguments (Options : out Options_Type);

   -- Exception for invalid arguments
   Invalid_Arguments : exception;

end CLI;

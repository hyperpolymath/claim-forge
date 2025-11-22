-- OpenTimestamps - OpenTimestamps Integration Body

with Ada.Text_IO;
with GNAT.OS_Lib;

package body OpenTimestamps is

   use Ada.Text_IO;

   function Execute_Command (Command : String) return Integer is
      use GNAT.OS_Lib;
      Args : Argument_List_Access;
      Exit_Code : Integer;
   begin
      Args := Argument_String_To_List (Command);
      Exit_Code := Spawn (Program_Name => Args (Args'First).all,
                          Args         => Args (Args'First + 1 .. Args'Last));
      Free (Args);
      return Exit_Code;
   end Execute_Command;

   procedure Stamp (File_Path : in String) is
      Command : constant String := "ots stamp " & File_Path;
      Exit_Code : Integer;
   begin
      Put_Line ("  Executing: " & Command);
      Exit_Code := Execute_Command (Command);

      if Exit_Code /= 0 then
         Put_Line ("  Warning: OpenTimestamps stamping returned exit code" & Exit_Code'Image);
         Put_Line ("  Note: Timestamp will be upgraded to blockchain over time");
      end if;

   end Stamp;

   procedure Verify (OTS_File : in String) is
      Command : constant String := "ots verify " & OTS_File;
      Exit_Code : Integer;
   begin
      Put_Line ("  Executing: " & Command);
      Exit_Code := Execute_Command (Command);

      if Exit_Code /= 0 then
         raise OTS_Error with "OpenTimestamps verification failed";
      end if;

   end Verify;

end OpenTimestamps;

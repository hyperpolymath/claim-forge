-- GPG - GPG Signing Integration Body

with Ada.Text_IO;
with GNAT.OS_Lib;

package body GPG is

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

   procedure Sign_File (File_Path : in String) is
      Command : constant String :=
         "gpg --detach-sign --armor --output " & File_Path & ".sig " & File_Path;
      Exit_Code : Integer;
   begin
      Put_Line ("  Executing: " & Command);
      Exit_Code := Execute_Command (Command);

      if Exit_Code /= 0 then
         raise GPG_Error with "GPG signing failed with exit code" & Exit_Code'Image;
      end if;

   end Sign_File;

   procedure Verify (File_Path : in String; Signature_Path : in String) is
      Command : constant String :=
         "gpg --verify " & Signature_Path & " " & File_Path;
      Exit_Code : Integer;
   begin
      Put_Line ("  Executing: " & Command);
      Exit_Code := Execute_Command (Command);

      if Exit_Code /= 0 then
         raise GPG_Error with "GPG verification failed with exit code" & Exit_Code'Image;
      end if;

   end Verify;

end GPG;

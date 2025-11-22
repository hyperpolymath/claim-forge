-- Git_Ops - Git Operations Integration Body

with Ada.Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;

package body Git_Ops is

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

   procedure Initialize (Repo_Name : in String) is
      Exit_Code : Integer;
   begin
      -- Check if .git exists
      if not Ada.Directories.Exists (".git") then
         Put_Line ("  Initializing new Git repository...");
         Exit_Code := Execute_Command ("git init");
         if Exit_Code /= 0 then
            raise Git_Error with "Failed to initialize Git repository";
         end if;
      else
         Put_Line ("  Git repository already exists");
      end if;

      -- Ensure Git config is set (use existing or prompt user to configure)
      Put_Line ("  Git repository: " & Repo_Name);

   end Initialize;

   procedure Commit_Claim (
      Claim_File  : in String;
      File_Path   : in String;
      Description : in String
   ) is
      Exit_Code : Integer;
      Commit_Message : constant String :=
         "feat: add IP claim for " & File_Path & ASCII.LF &
         ASCII.LF &
         Description;
   begin
      -- Add files to Git
      Put_Line ("  Adding files to Git...");
      Exit_Code := Execute_Command ("git add " & Claim_File);
      if Exit_Code /= 0 then
         raise Git_Error with "Failed to add claim file to Git";
      end if;

      -- Add signature file if it exists
      declare
         Sig_File : constant String := Claim_File & ".sig";
      begin
         if Ada.Directories.Exists (Sig_File) then
            Exit_Code := Execute_Command ("git add " & Sig_File);
         end if;
      end;

      -- Add OTS file if it exists
      declare
         OTS_File : constant String := Claim_File & ".ots";
      begin
         if Ada.Directories.Exists (OTS_File) then
            Exit_Code := Execute_Command ("git add " & OTS_File);
         end if;
      end;

      -- Commit with signed commit
      Put_Line ("  Committing changes...");
      Exit_Code := Execute_Command ("git commit -S -m """ & Commit_Message & """");
      if Exit_Code /= 0 then
         Put_Line ("  Warning: Signed commit failed, trying unsigned commit...");
         Exit_Code := Execute_Command ("git commit -m """ & Commit_Message & """");
         if Exit_Code /= 0 then
            raise Git_Error with "Failed to commit changes";
         end if;
      end if;

   end Commit_Claim;

   procedure Create_Tag (Tag_Name : in String) is
      Exit_Code : Integer;
      Tag_Message : constant String := "Claim registration: " & Tag_Name;
   begin
      Put_Line ("  Creating Git tag...");
      Exit_Code := Execute_Command ("git tag -s -m """ & Tag_Message & """ claim-" & Tag_Name);

      if Exit_Code /= 0 then
         Put_Line ("  Warning: Signed tag failed, creating unsigned tag...");
         Exit_Code := Execute_Command ("git tag -m """ & Tag_Message & """ claim-" & Tag_Name);
         if Exit_Code /= 0 then
            Put_Line ("  Warning: Tag creation failed");
         end if;
      end if;

   end Create_Tag;

end Git_Ops;

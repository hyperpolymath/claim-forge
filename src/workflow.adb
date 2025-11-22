-- Workflow - Main Workflow Orchestration Body

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Claim;
with GPG;
with OpenTimestamps;
with Git_Ops;
with Publisher;

package body Workflow is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   function Trim (S : CLI.Bounded_String) return String is
   begin
      return Ada.Strings.Fixed.Trim (String (S), Ada.Strings.Both);
   end Trim;

   procedure Run (
      Configuration : in Config.Config_Type;
      Options       : in CLI.Options_Type
   ) is
      Repo_Name   : constant String := Trim (Options.Repo_Name);
      File_Path   : constant String := Trim (Options.File_Path);
      Description : constant String := Trim (Options.Description);
      License     : constant String := Trim (Options.License);
      Claim_File  : constant String := "CLAIM.md";
   begin
      -- Validate inputs
      if not Options.Repo_Set or Repo_Name = "" then
         raise Workflow_Error with "Repository name is required";
      end if;

      if not Options.File_Set or File_Path = "" then
         raise Workflow_Error with "File path is required";
      end if;

      Put_Line ("Starting claim registration workflow...");
      New_Line;

      -- Step 1: Generate claim file
      Put_Line ("[1/7] Generating claim file...");
      Claim.Generate (
         Claim_File  => Claim_File,
         File_Path   => File_Path,
         Description => Description,
         License     => License
      );
      Put_Line ("  ✓ Claim file created: " & Claim_File);
      New_Line;

      -- Step 2: Initialize/configure Git repository
      Put_Line ("[2/7] Configuring Git repository...");
      Git_Ops.Initialize (Repo_Name);
      Put_Line ("  ✓ Git repository ready");
      New_Line;

      -- Step 3: Sign with GPG
      if Configuration.Auto_Sign then
         Put_Line ("[3/7] Signing claim with GPG...");
         GPG.Sign_File (Claim_File);
         Put_Line ("  ✓ GPG signature created");
      else
         Put_Line ("[3/7] Skipping GPG signing (disabled in config)");
      end if;
      New_Line;

      -- Step 4: Create OpenTimestamps proof
      if Configuration.OTS_Enabled and not Options.No_Timestamp then
         Put_Line ("[4/7] Creating blockchain timestamp...");
         OpenTimestamps.Stamp (Claim_File);
         Put_Line ("  ✓ OpenTimestamps proof created");
      else
         Put_Line ("[4/7] Skipping OpenTimestamps (disabled)");
      end if;
      New_Line;

      -- Step 5: Commit to Git
      Put_Line ("[5/7] Committing to Git...");
      Git_Ops.Commit_Claim (Claim_File, File_Path, Description);
      Put_Line ("  ✓ Changes committed");
      New_Line;

      -- Step 6: Create signed tag
      Put_Line ("[6/7] Creating signed Git tag...");
      Git_Ops.Create_Tag (Repo_Name);
      Put_Line ("  ✓ Signed tag created");
      New_Line;

      -- Step 7: Push to remote repositories
      if not Options.No_Push then
         Put_Line ("[7/7] Publishing to remote repositories...");
         Publisher.Push_All (
            Repo_Name,
            Configuration.GitLab_Enabled,
            Configuration.GitHub_Enabled
         );
         Put_Line ("  ✓ Published to remote repositories");
      else
         Put_Line ("[7/7] Skipping push to remote (--no-push)");
      end if;

      New_Line;
      Put_Line ("═══════════════════════════════════════");
      Put_Line ("Claim registration completed successfully!");
      Put_Line ("═══════════════════════════════════════");

   end Run;

end Workflow;

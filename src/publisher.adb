-- Publisher - Multi-Platform Publishing Body

with Ada.Text_IO;
with GNAT.OS_Lib;

package body Publisher is

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

   procedure Push_To_GitLab (Repo_Name : in String) is
      Remote_URL : constant String := "git@gitlab.com:user/" & Repo_Name & ".git";
      Exit_Code : Integer;
   begin
      Put_Line ("  Pushing to GitLab...");

      -- Add remote if not exists
      Exit_Code := Execute_Command ("git remote add gitlab " & Remote_URL);

      -- Push (ignore if remote already exists)
      Exit_Code := Execute_Command ("git push gitlab main --tags");

      if Exit_Code /= 0 then
         Put_Line ("  Warning: GitLab push failed. Check remote configuration.");
      else
         Put_Line ("  ✓ Pushed to GitLab");
      end if;
   end Push_To_GitLab;

   procedure Push_To_GitHub (Repo_Name : in String) is
      Remote_URL : constant String := "git@github.com:user/" & Repo_Name & ".git";
      Exit_Code : Integer;
   begin
      Put_Line ("  Pushing to GitHub...");

      -- Add remote if not exists
      Exit_Code := Execute_Command ("git remote add github " & Remote_URL);

      -- Push (ignore if remote already exists)
      Exit_Code := Execute_Command ("git push github main --tags");

      if Exit_Code /= 0 then
         Put_Line ("  Warning: GitHub push failed. Check remote configuration.");
      else
         Put_Line ("  ✓ Pushed to GitHub");
      end if;
   end Push_To_GitHub;

   procedure Push_All (
      Repo_Name      : in String;
      GitLab_Enabled : in Boolean;
      GitHub_Enabled : in Boolean
   ) is
   begin
      if GitLab_Enabled then
         Push_To_GitLab (Repo_Name);
      end if;

      if GitHub_Enabled then
         Push_To_GitHub (Repo_Name);
      end if;

      if not GitLab_Enabled and not GitHub_Enabled then
         Put_Line ("  No remote platforms enabled - skipping push");
      end if;

   end Push_All;

end Publisher;

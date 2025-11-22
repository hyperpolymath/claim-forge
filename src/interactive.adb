-- Interactive - Interactive Prompt System Body

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Workflow;

package body Interactive is

   use Ada.Text_IO;

   function Prompt (
      Message : String;
      Default : String := ""
   ) return String is
      Input : String (1 .. 1024);
      Last  : Natural;
   begin
      if Default /= "" then
         Put (Message & " [" & Default & "]: ");
      else
         Put (Message & ": ");
      end if;

      Get_Line (Input, Last);

      if Last = 0 and Default /= "" then
         return Default;
      else
         return Input (1 .. Last);
      end if;
   end Prompt;

   procedure Set_Option_String (
      Target : out CLI.Bounded_String;
      Source : in String;
      Is_Set : out Boolean
   ) is
      Len : constant Natural := Natural'Min (Source'Length, CLI.Max_String_Length);
   begin
      Target := (others => ' ');
      if Len > 0 then
         Target (1 .. Len) := Source (1 .. Len);
      end if;
      Is_Set := True;
   end Set_Option_String;

   procedure Run (
      Configuration : in Config.Config_Type;
      Options       : in out CLI.Options_Type
   ) is
      Repo_Name   : String := Prompt ("Repository name");
      File_Path   : String := Prompt ("File to claim");
      Description : String := Prompt ("Description");
      License_Str : String := Prompt ("License",
                                      Ada.Strings.Fixed.Trim (String (Configuration.Default_License), Ada.Strings.Both));
   begin
      Put_Line ("");
      Put_Line ("═══ Claim Configuration ═══");
      Put_Line ("Repository  : " & Repo_Name);
      Put_Line ("File        : " & File_Path);
      Put_Line ("Description : " & Description);
      Put_Line ("License     : " & License_Str);
      Put_Line ("═══════════════════════════");
      Put_Line ("");

      declare
         Confirm : String := Prompt ("Proceed with claim registration? (yes/no)", "yes");
      begin
         if Confirm /= "yes" and Confirm /= "y" and Confirm /= "Y" then
            Put_Line ("Claim registration cancelled.");
            return;
         end if;
      end;

      -- Set options from interactive input
      Set_Option_String (Options.Repo_Name, Repo_Name, Options.Repo_Set);
      Set_Option_String (Options.File_Path, File_Path, Options.File_Set);
      Set_Option_String (Options.Description, Description, Options.Desc_Set);
      Set_Option_String (Options.License, License_Str, Options.License_Set);

      -- Run the workflow
      Workflow.Run (Configuration, Options);

   end Run;

end Interactive;

-- Claim Forge - Main Entry Point
-- IP registration and timestamping system
-- Copyright (c) 2025 Hyperpolymath

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with CLI;
with Config;
with Interactive;
with Workflow;

procedure Claim_Forge is
   use Ada.Text_IO;
   use Ada.Command_Line;

   procedure Show_Banner is
   begin
      Put_Line ("╔═══════════════════════════════════════════════════════════╗");
      Put_Line ("║              Claim Forge v1.0.0                           ║");
      Put_Line ("║  IP Registration & Blockchain Timestamping System        ║");
      Put_Line ("╚═══════════════════════════════════════════════════════════╝");
      New_Line;
   end Show_Banner;

   procedure Show_Help is
   begin
      Put_Line ("Usage: claim-forge [OPTIONS]");
      New_Line;
      Put_Line ("Options:");
      Put_Line ("  -h, --help              Show this help message");
      Put_Line ("  -v, --version           Show version information");
      Put_Line ("  -i, --interactive       Run in interactive mode (default)");
      Put_Line ("  -c, --config FILE       Use custom configuration file");
      Put_Line ("  --repo NAME             Repository name");
      Put_Line ("  --file PATH             File to claim");
      Put_Line ("  --description TEXT      Claim description");
      Put_Line ("  --license TYPE          License type (default: Palimpsest)");
      Put_Line ("  --no-timestamp          Skip OpenTimestamps");
      Put_Line ("  --no-push               Skip git push");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  claim-forge --interactive");
      Put_Line ("  claim-forge --repo my-project --file paper.pdf --description 'Research paper'");
      New_Line;
      Put_Line ("For more information, visit: https://github.com/Hyperpolymath/claim-forge");
   end Show_Help;

   procedure Show_Version is
   begin
      Put_Line ("Claim Forge v1.0.0");
      Put_Line ("Built with GNAT Ada 2012");
      Put_Line ("Copyright (c) 2025 Hyperpolymath");
   end Show_Version;

   Configuration : Config.Config_Type;
   Options       : CLI.Options_Type;

begin
   -- Parse command line arguments
   CLI.Parse_Arguments (Options);

   -- Handle special flags
   if Options.Show_Help then
      Show_Banner;
      Show_Help;
      return;
   end if;

   if Options.Show_Version then
      Show_Version;
      return;
   end if;

   -- Show banner in interactive mode
   if Options.Interactive then
      Show_Banner;
   end if;

   -- Load configuration
   Config.Load (Configuration, Options.Config_File);

   -- Run workflow
   if Options.Interactive then
      -- Interactive mode: prompt for all required information
      Interactive.Run (Configuration, Options);
   else
      -- Non-interactive mode: use command-line options
      Workflow.Run (Configuration, Options);
   end if;

   Put_Line ("✓ Claim registration complete!");

exception
   when E : CLI.Invalid_Arguments =>
      Put_Line (Standard_Error, "Error: " & Ada.Exceptions.Exception_Message (E));
      New_Line (Standard_Error);
      Show_Help;
      Set_Exit_Status (Failure);

   when E : Config.Configuration_Error =>
      Put_Line (Standard_Error, "Configuration Error: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);

   when E : Workflow.Workflow_Error =>
      Put_Line (Standard_Error, "Workflow Error: " & Ada.Exceptions.Exception_Message (E));
      Set_Exit_Status (Failure);

   when E : others =>
      Put_Line (Standard_Error, "Unexpected Error: " & Ada.Exceptions.Exception_Message (E));
      Put_Line (Standard_Error, Ada.Exceptions.Exception_Information (E));
      Set_Exit_Status (Failure);

end Claim_Forge;

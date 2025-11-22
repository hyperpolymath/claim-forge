-- Workflow - Main Workflow Orchestration Specification

with CLI;
with Config;

package Workflow is

   -- Execute the claim registration workflow
   procedure Run (
      Configuration : in Config.Config_Type;
      Options       : in CLI.Options_Type
   );

   Workflow_Error : exception;

end Workflow;

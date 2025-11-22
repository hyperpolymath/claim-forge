-- Interactive - Interactive Prompt System Specification

with CLI;
with Config;

package Interactive is

   -- Run interactive workflow
   procedure Run (
      Configuration : in Config.Config_Type;
      Options       : in out CLI.Options_Type
   );

end Interactive;

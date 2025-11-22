-- Config - Configuration Management Package Specification

with CLI;

package Config is

   type Config_Type is record
      Default_License : CLI.Bounded_String := (others => ' ');
      GitLab_Enabled  : Boolean := True;
      GitHub_Enabled  : Boolean := True;
      OTS_Enabled     : Boolean := True;
      Auto_Sign       : Boolean := True;
   end record;

   -- Load configuration from file
   procedure Load (
      Config      : out Config_Type;
      Config_File : in CLI.Bounded_String
   );

   -- Get default configuration
   function Get_Default return Config_Type;

   Configuration_Error : exception;

end Config;

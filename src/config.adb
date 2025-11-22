-- Config - Configuration Management Package Body

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Directories;

package body Config is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   function Trim (S : CLI.Bounded_String) return String is
   begin
      return Ada.Strings.Fixed.Trim (String (S), Ada.Strings.Both);
   end Trim;

   procedure Load (
      Config      : out Config_Type;
      Config_File : in CLI.Bounded_String
   ) is
      File_Path : constant String := Trim (Config_File);
   begin
      -- Start with defaults
      Config := Get_Default;

      -- If no config file specified, use defaults
      if File_Path = "" then
         return;
      end if;

      -- Check if config file exists
      if not Ada.Directories.Exists (File_Path) then
         raise Configuration_Error with "Config file not found: " & File_Path;
      end if;

      -- TODO: Parse TOML configuration file
      -- For now, just use defaults
      Put_Line ("Note: Configuration file parsing not yet implemented");
      Put_Line ("Using default configuration");

   end Load;

   function Get_Default return Config_Type is
      Default : Config_Type;
   begin
      Default.Default_License (1 .. 11) := "Palimpsest ";
      Default.GitLab_Enabled := True;
      Default.GitHub_Enabled := True;
      Default.OTS_Enabled := True;
      Default.Auto_Sign := True;
      return Default;
   end Get_Default;

end Config;

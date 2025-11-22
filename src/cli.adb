-- CLI - Command Line Interface Package Body

with Ada.Command_Line;
with Ada.Strings.Fixed;

package body CLI is

   use Ada.Command_Line;
   use Ada.Strings.Fixed;

   procedure Set_String (
      Target : out Bounded_String;
      Source : in String;
      Is_Set : out Boolean
   ) is
      Len : constant Natural := Natural'Min (Source'Length, Max_String_Length);
   begin
      Target := (others => ' ');
      Target (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      Is_Set := True;
   end Set_String;

   function Trim_String (S : Bounded_String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Trim_String;

   procedure Parse_Arguments (Options : out Options_Type) is
      I : Positive := 1;
   begin
      Options := (others => <>);  -- Initialize with defaults

      -- If no arguments, default to interactive mode
      if Argument_Count = 0 then
         Options.Interactive := True;
         return;
      end if;

      -- Parse each argument
      while I <= Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg = "-h" or Arg = "--help" then
               Options.Show_Help := True;
               return;  -- Short-circuit: just show help

            elsif Arg = "-v" or Arg = "--version" then
               Options.Show_Version := True;
               return;  -- Short-circuit: just show version

            elsif Arg = "-i" or Arg = "--interactive" then
               Options.Interactive := True;

            elsif Arg = "-c" or Arg = "--config" then
               if I >= Argument_Count then
                  raise Invalid_Arguments with "Missing value for --config";
               end if;
               I := I + 1;
               Set_String (Options.Config_File, Argument (I), Options.Config_Set);

            elsif Arg = "--repo" then
               if I >= Argument_Count then
                  raise Invalid_Arguments with "Missing value for --repo";
               end if;
               I := I + 1;
               Set_String (Options.Repo_Name, Argument (I), Options.Repo_Set);
               Options.Interactive := False;  -- Non-interactive if options provided

            elsif Arg = "--file" then
               if I >= Argument_Count then
                  raise Invalid_Arguments with "Missing value for --file";
               end if;
               I := I + 1;
               Set_String (Options.File_Path, Argument (I), Options.File_Set);
               Options.Interactive := False;

            elsif Arg = "--description" then
               if I >= Argument_Count then
                  raise Invalid_Arguments with "Missing value for --description";
               end if;
               I := I + 1;
               Set_String (Options.Description, Argument (I), Options.Desc_Set);
               Options.Interactive := False;

            elsif Arg = "--license" then
               if I >= Argument_Count then
                  raise Invalid_Arguments with "Missing value for --license";
               end if;
               I := I + 1;
               Set_String (Options.License, Argument (I), Options.License_Set);

            elsif Arg = "--no-timestamp" then
               Options.No_Timestamp := True;

            elsif Arg = "--no-push" then
               Options.No_Push := True;

            else
               raise Invalid_Arguments with "Unknown argument: " & Arg;
            end if;
         end;

         I := I + 1;
      end loop;

   end Parse_Arguments;

end CLI;

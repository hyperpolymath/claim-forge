-- Publisher - Multi-Platform Publishing Specification

package Publisher is

   -- Push to all configured platforms
   procedure Push_All (
      Repo_Name      : in String;
      GitLab_Enabled : in Boolean;
      GitHub_Enabled : in Boolean
   );

   Publisher_Error : exception;

end Publisher;

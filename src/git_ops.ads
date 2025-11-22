-- Git_Ops - Git Operations Integration Specification

package Git_Ops is

   -- Initialize or configure Git repository
   procedure Initialize (Repo_Name : in String);

   -- Commit claim file and related files
   procedure Commit_Claim (
      Claim_File  : in String;
      File_Path   : in String;
      Description : in String
   );

   -- Create a signed Git tag
   procedure Create_Tag (Tag_Name : in String);

   Git_Error : exception;

end Git_Ops;

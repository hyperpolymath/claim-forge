-- Claim - Claim File Generation Specification

package Claim is

   -- Generate a claim file
   procedure Generate (
      Claim_File  : in String;
      File_Path   : in String;
      Description : in String;
      License     : in String
   );

   Claim_Error : exception;

end Claim;

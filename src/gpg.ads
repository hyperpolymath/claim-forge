-- GPG - GPG Signing Integration Specification

package GPG is

   -- Sign a file with GPG
   procedure Sign_File (File_Path : in String);

   -- Verify a GPG signature
   procedure Verify (File_Path : in String; Signature_Path : in String);

   GPG_Error : exception;

end GPG;

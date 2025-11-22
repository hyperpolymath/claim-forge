-- OpenTimestamps - OpenTimestamps Integration Specification

package OpenTimestamps is

   -- Create an OpenTimestamps proof for a file
   procedure Stamp (File_Path : in String);

   -- Verify an OpenTimestamps proof
   procedure Verify (OTS_File : in String);

   OTS_Error : exception;

end OpenTimestamps;

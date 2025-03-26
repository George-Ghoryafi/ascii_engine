-- filepath: c:\Users\georg\OneDrive\Documents\ascii_engine\ascii_engine\demos\src\input_test.adb
with Ada.Text_IO; use Ada.Text_IO;
with Input;       use Input;

procedure Input_Test is
begin
   Put_Line("Starting Input Test Demo");
   Put_Line("-------------------------");
   
   -- Call the Input procedure from the Input package
   Input.Simple;
   
   Put_Line("-------------------------");
   Put_Line("Input Test Demo Complete");
end Input_Test;
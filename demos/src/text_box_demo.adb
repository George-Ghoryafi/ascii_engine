with Ada.Text_IO; use Ada.Text_IO;
with Text_Box; use Text_Box;

procedure Text_Box_Demo is
   Welcome_Box : Text_Box_Type;
   Info_Box : Text_Box_Type;
begin
   -- Create and display a welcome message box
   Create(Welcome_Box, Width => 30, Height => 3, Title => "Welcome");
   Set_Content(Welcome_Box, "Welcome to the Grocery System!");
   Display(Welcome_Box);
   
   New_Line;
   
   -- Create and display an information box
   Create(Info_Box, Width => 40, Height => 4, Title => "Information");
   Set_Content(Info_Box, "Textbox Demo");
   Display(Info_Box);
end Text_Box_Demo;

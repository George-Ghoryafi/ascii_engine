with Ada.Text_IO; use Ada.Text_IO;
with Container; use Container;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


procedure Container_Test is
   -- Create a test container
   C1 : Container_T := Create("Hello, World!");
   C2 : Container_T := Create("Simple Box");
   C3 : Container_T := Create(""); -- Creating an empty container - this will be allowed to have children 
   str : Unbounded_String;
   
begin
   Put_Line("Container Test Program");
   Put_Line("=====================");
   New_Line;
   
   -- Display render_Rendered containers
   Put_Line("Container 1 Rendering:");
   Put_Line(Render(C1));
   New_Line;
   
   Put_Line("Container 2 render_Rendering:");
   Put_Line(Render(C2));
   New_Line;
   
   -- Modify container text
   Put_Line("Modifying container...");
   Set_Text(C1, "Updated text");
   New_Line;
   
   -- Display updated container
   Put_Line("Container 1 after update:");
   Put_Line(Render(C1));

   -- Can grab just the text that is inside the node without the container
   Put_Line("checking the node"); 
   str := To_Unbounded_String(Get_Text(C1));  
   Put_Line(To_String(str));
   New_Line;

   Put_Line("Trying out the nested contaienrs"); 
   Container.Add_Child(C3, C1);
   Add_Child(C3, C2);
   Put_Line(Render(C3));
   New_Line;





   
   Put_Line("Test completed successfully.");
end Container_Test;
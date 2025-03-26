with Ada.Text_IO;
with Container;

procedure Container_Test is
   -- Test case for a simple single-line container
   procedure Test_Simple_Container is
      Simple_Container : Container.Container_Type := Container.Create ("Hello, World!");
   begin
      Ada.Text_IO.Put_Line ("=== Simple Container Test ===");
      Container.Render (Simple_Container);
      Container.Free (Simple_Container);
      Ada.Text_IO.New_Line;
   end Test_Simple_Container;
   
   -- Test case for a multi-line container
   procedure Test_Multiline_Container is
      -- Create a multi-line string with line feeds
      Multi_Line_Text : constant String := 
         "This is a test" & ASCII.LF &
         "of a multi-line" & ASCII.LF &
         "container in our" & ASCII.LF &
         "ASCII Engine!";
      
      Multi_Container : Container.Container_Type := Container.Create (Multi_Line_Text);
   begin
      Ada.Text_IO.Put_Line ("=== Multi-line Container Test ===");
      Container.Render (Multi_Container);
      Container.Free (Multi_Container);
      Ada.Text_IO.New_Line;
   end Test_Multiline_Container;
   
   -- Test case for an empty container
   procedure Test_Empty_Container is
      Empty_Container : Container.Container_Type := Container.Create ("");
   begin
      Ada.Text_IO.Put_Line ("=== Empty Container Test ===");
      Container.Render (Empty_Container);
      Container.Free (Empty_Container);
      Ada.Text_IO.New_Line;
   end Test_Empty_Container;
   
   -- Test case for a container with varying line lengths
   procedure Test_Varying_Line_Lengths is
      Varying_Text : constant String :=
         "Short line" & ASCII.LF &
         "This is a much longer line that should extend the container width" &
         ASCII.LF &
         "Medium length line here" & ASCII.LF &
         "End";
      
      Varying_Container : Container.Container_Type := Container.Create (Varying_Text);
   begin
      Ada.Text_IO.Put_Line ("=== Varying Line Lengths Test ===");
      Container.Render (Varying_Container);
      Container.Free (Varying_Container);
      Ada.Text_IO.New_Line;
   end Test_Varying_Line_Lengths;
   
   -- Test case for containers with different background colors
   procedure Test_Colored_Containers is
      -- Red container
      Red_Text : constant String := "This container has a red background";
      Red_Container : Container.Container_Type := 
         Container.Create (Red_Text, Container.Hex_To_Color ("#FF0000"));
      
      -- Blue container
      Blue_Text : constant String := "This one is blue!";
      Blue_Container : Container.Container_Type := 
         Container.Create (Blue_Text, Container.Hex_To_Color ("#0000FF"));
      
      -- Green container
      Green_Text : constant String := "And this is green" & ASCII.LF & "with multiple lines!";
      Green_Container : Container.Container_Type := 
         Container.Create (Green_Text, Container.Hex_To_Color ("#00FF00"));
      
      -- Purple container
      Purple_Text : constant String := "Purple is a mix of red and blue";
      Purple_Container : Container.Container_Type := 
         Container.Create (Purple_Text, Container.Hex_To_Color ("#800080"));
      
      -- Orange container
      Orange_Text : constant String := "Orange is warm and friendly";
      Orange_Container : Container.Container_Type := 
         Container.Create (Orange_Text, Container.Hex_To_Color ("#FFA500"));
   begin
      Ada.Text_IO.Put_Line ("=== Colored Containers Test ===");
      
      -- Test direct ANSI color code to verify terminal support
      Ada.Text_IO.Put (ASCII.ESC & "[48;2;255;0;0m");
      Ada.Text_IO.Put ("This text should have a red background");
      Ada.Text_IO.Put (ASCII.ESC & "[0m");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      
      Ada.Text_IO.Put_Line ("Red Container:");
      Container.Render (Red_Container);
      Ada.Text_IO.New_Line;
      
      Ada.Text_IO.Put_Line ("Blue Container:");
      Container.Render (Blue_Container);
      Ada.Text_IO.New_Line;
      
      Ada.Text_IO.Put_Line ("Green Container:");
      Container.Render (Green_Container);
      Ada.Text_IO.New_Line;
      
      Ada.Text_IO.Put_Line ("Purple Container:");
      Container.Render (Purple_Container);
      Ada.Text_IO.New_Line;
      
      Ada.Text_IO.Put_Line ("Orange Container:");
      Container.Render (Orange_Container);
      
      -- Free all containers
      Container.Free (Red_Container);
      Container.Free (Blue_Container);
      Container.Free (Green_Container);
      Container.Free (Purple_Container);
      Container.Free (Orange_Container);
      
      Ada.Text_IO.New_Line;
   end Test_Colored_Containers;
   
   -- Test case for nested containers
      -- Test case for nested containers
         procedure Test_Nested_Containers is
            -- Parent container with light gray background
            Parent_Text : constant String := 
               "This is a parent container" & ASCII.LF &
               "It can contain other containers" & ASCII.LF &
               "as nested elements";
            Parent_Container : Container.Container_Type := 
               Container.Create (Parent_Text, Container.Hex_To_Color ("#DDDDDD"));
            
            -- Create a border container (like a card component)
            Border_Text : constant String :=
               "+----------------------------------------+" & ASCII.LF &
               "|                                        |" & ASCII.LF &
               "|                                        |" & ASCII.LF &
               "|                                        |" & ASCII.LF &
               "|                                        |" & ASCII.LF &
               "+----------------------------------------+";
            Border_Container : Container.Container_Type :=
               Container.Create (Border_Text, Container.Hex_To_Color ("#333333"));
            
            -- First child container with blue background
            Child1_Text : constant String := "I am a nested blue container";
            Child1_Container : Container.Container_Type := 
               Container.Create (Child1_Text, Container.Hex_To_Color ("#0000FF"));
            
            -- Second child container with green background
            Child2_Text : constant String := 
               "I am a nested green container" & ASCII.LF &
               "with multiple lines";
            Child2_Container : Container.Container_Type := 
               Container.Create (Child2_Text, Container.Hex_To_Color ("#00FF00"));
            
            -- Grandchild container (nested inside Child2)
            Grandchild_Text : constant String := "I am deeply nested!";
            Grandchild_Container : Container.Container_Type := 
               Container.Create (Grandchild_Text, Container.Hex_To_Color ("#FF00FF"));
         begin
            Ada.Text_IO.Put_Line ("=== Nested Containers Test ===");
            
            -- Add the border container to the parent
            Container.Add_Child (Parent_Container, Border_Container);
            
            -- Add the children to the border container (side by side layout)
            Container.Add_Child (Border_Container, Child1_Container);
            Container.Add_Child (Border_Container, Child2_Container);
            
            -- Add the grandchild to the second child
            Container.Add_Child (Child2_Container, Grandchild_Container);
            
            -- Render the parent container (which will also render its children)
            Container.Render (Parent_Container);
            
            -- Free the containers
            Container.Free (Parent_Container);
            Container.Free (Border_Container);
            Container.Free (Child1_Container);
            Container.Free (Child2_Container);
            Container.Free (Grandchild_Container);
            
            Ada.Text_IO.New_Line;
         end Test_Nested_Containers;
      
   begin
      Ada.Text_IO.Put_Line ("Container Test Suite");
      Ada.Text_IO.Put_Line ("===================");
      Ada.Text_IO.New_Line;
   
   -- Run all test cases
   Test_Simple_Container;
   Test_Multiline_Container;
   Test_Empty_Container;
   Test_Varying_Line_Lengths;
   Test_Colored_Containers;
   Test_Nested_Containers;  -- Add this line to run the nested containers test
   
   Ada.Text_IO.Put_Line ("All tests completed.");
end Container_Test;
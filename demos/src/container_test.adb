with Widget; use Widget; 
with Handler; use Handler;
with Widget.Text; use Widget.Text;
with Ada.Text_IO; use Ada.Text_IO;

procedure Container_Test is
   header : Widget.Any_Acc := 
      Widget.Create (id => "Header",
                     parent => Handler.main_widget, 
                     priority => 1,
                     bgd_color => (red => 255, green => 0, blue => 0, alpha => 255)   
                     );

   body_component : Widget.Any_Acc :=
      Widget.Create (id => "Body",
                     parent => Handler.main_widget, 
                     priority => 1,
                     bgd_color => (red => 0, green => 255, blue => 0, alpha => 255)   
                     );

   footer : Widget.Any_Acc :=
      Widget.Create (id => "Footer",
                     parent => Handler.main_widget, 
                     priority => 1,
                     bgd_color => (red => 0, green => 0, blue => 255, alpha => 255)   
                     );

   content : Widget.Any_Acc :=
      Widget.Create (id => "Content",
                     parent => body_component, 
                     priority => 1,
                     bgd_color => (red => 255, green => 255, blue => 0, alpha => 255)   
                     );
                     
   textbox : Widget.Any_Acc :=
      Widget.Text.Create (id => "TextBox",
                          parent => content, 
                          priority => 1,
                          bgd_color => (red => 255, green => 255, blue => 0, alpha => 255),
                          text => "Son of Content",
                          text_color => (red => 0, green => 0, blue => 0, alpha => 255),
                          overflow => default
                          );
   textbox2 : Widget.Any_Acc :=
      Widget.Text.Create (id => "TextBox2",
                          parent => content, 
                          priority => 0,
                          bgd_color => (red => 255, green => 0, blue => 255, alpha => 255),
                          text => "Second son of Content",
                          text_color => (red => 0, green => 0, blue => 0, alpha => 255),
                          overflow => default
                          );
                  
begin

   Put_Line("Rendering Nodes"); 

   header.Set_Width (50);
   header.Set_Height (20);

   body_component.Set_Width (50);
   body_component.Set_Height (20);

   footer.Set_Width (50);
   footer.Set_Height (20);

   content.Set_Width (40);
   content.Set_Height (10);
   content.Set_Flex_Direction (row);

   textbox.Set_Width (10);
   textbox.Set_Height (5);

   textbox2.Set_Width (15);
   textbox2.Set_Height (5);

   Handler.display_nodes; 



end Container_Test;
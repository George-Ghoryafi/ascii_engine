with Widget; use Widget; 
with Handler; use Handler;
with Widget.Text; use Widget.Text;
with Ada.Text_IO; use Ada.Text_IO;

procedure Container_Test is
   Multi_Line_String : constant String := 
   "This is a mutli-line string." & ASCII.LF &
   "It is used to test the text widget." & ASCII.LF &
   "It should be able to handle line breaks." & ASCII.LF &
   "This is the last line.";

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
                     bgd_color => Widget.default_color   
                     );

   footer : Widget.Any_Acc :=
      Widget.Create (id => "Footer",
                     parent => Handler.main_widget, 
                     priority => 1,
                     bgd_color => Widget.default_color   
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
                          bgd_color => Widget.default_color,
                          text => "This here is the content section",
                          text_color => Widget.default_color,
                          overflow => default
                          );
   textbox2 : Widget.Any_Acc :=
      Widget.Text.Create (id => "TextBox2",
                          parent => content, 
                          priority => 0,
                          bgd_color => Widget.default_color,
                          text => Multi_Line_String,
                          text_color => (red => 0, green => 0, blue => 0, alpha => 255),
                          overflow => default
                          );

   textbox3 : Widget.Any_Acc :=
      Widget.Text.Create (id => "TextBox3",
                          parent => footer, 
                          priority => 0,
                          bgd_color => Widget.default_color,
                          text => "First Son of Footer",
                          text_color => (red => 0, green => 0, blue => 0, alpha => 255),
                          overflow => default
                          );

   textbox4 : Widget.Any_Acc :=
      Widget.Text.Create (id => "TextBox4",
                          parent => header, 
                          priority => 0,
                          bgd_color => Widget.default_color,
                          text => "This is the header!",
                          text_color => (red => 0, green => 0, blue => 0, alpha => 255),
                          overflow => default
                          );
                  
begin

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
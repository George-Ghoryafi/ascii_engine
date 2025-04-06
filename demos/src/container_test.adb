with Widget; use Widget; 
with Handler; use Handler;
with Widget.Text; use Widget.Text;
with Ada.Text_IO; use Ada.Text_IO;

procedure Container_Test is
   header : Widget.Any_Acc := 
      Widget.Create (id => "Header",
                     parent => Handler.main_widget, 
                     priority => 1,
                     min_height => 50,
                     min_width => 50,
                     max_height => 50,
                     max_width => 50,
                     bgd_color => (red => 255, green => 0, blue => 0, alpha => 255)   
                     );

   body_component : Widget.Any_Acc :=
      Widget.Create (id => "Body",
                     parent => Handler.main_widget, 
                     priority => 1,
                     min_height => 50,
                     min_width => 50,
                     max_height => 50,
                     max_width => 50,
                     bgd_color => (red => 0, green => 255, blue => 0, alpha => 255)   
                     );

   footer : Widget.Any_Acc :=
      Widget.Create (id => "Footer",
                     parent => Handler.main_widget, 
                     priority => 1,
                     min_height => 50,
                     min_width => 50,
                     max_height => 50,
                     max_width => 50,
                     bgd_color => (red => 0, green => 0, blue => 255, alpha => 255)   
                     );

   content : Widget.Any_Acc :=
      Widget.Create (id => "Content",
                     parent => body_component, 
                     priority => 1,
                     min_height => 50,
                     min_width => 50,
                     max_height => 50,
                     max_width => 50,
                     bgd_color => (red => 255, green => 255, blue => 0, alpha => 255)   
                     );
                     
   textbox : Widget.Any_Acc :=
      Widget.Text.Create (id => "TextBox",
                          parent => content, 
                          priority => 1,
                          min_height => 50,
                          min_width => 50,
                          max_height => 50,
                          max_width => 50,
                          bgd_color => (red => 255, green => 255, blue => 0, alpha => 255),
                          text => "Son of Content",
                          text_color => (red => 0, green => 0, blue => 0, alpha => 255),
                          overflow => default
                          );
                  
begin

   Put_Line("Rendering Nodes"); 
   Handler.display_nodes; 







end Container_Test;
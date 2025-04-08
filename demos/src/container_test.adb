with Widget; use Widget; 
with Handler; use Handler;
with Widget.Text; use Widget.Text;
with Widget.Button; use Widget.Button;
with Ada.Text_IO; use Ada.Text_IO;
with Button_Callbacks; use Button_Callbacks;

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
                     bgd_color => (red => 255, green => 0, blue => 0, alpha => 255));

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
                  
   -- Add buttons to demonstrate navigation
   button1 : Widget.Any_Acc :=
      Widget.Button.Create (id => "Button1",
                           parent => header,
                           priority => 1,
                           bgd_color => Widget.default_color,
                           label => "Button 1",
                           on_click => Button1_Click'Access);

   button2 : Widget.Any_Acc :=
      Widget.Button.Create (id => "Button2",
                           parent => content,
                           priority => 2,
                           bgd_color => (red => 200, green => 200, blue => 200, alpha => 255),
                           label => "Content Button",
                           on_click => Button2_Click'Access
                           );
                           
   button3 : Widget.Any_Acc :=
      Widget.Button.Create (id => "Button3",
                           parent => footer,
                           priority => 1,
                           bgd_color => (red => 200, green => 200, blue => 200, alpha => 255),
                           label => "Footer Button",
                           on_click => Button3_Click'Access
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
   
   button1.Set_Width (15);
   button1.Set_Height (3);
   
   button2.Set_Width (15);
   button2.Set_Height (3);
   
   button3.Set_Width (15);
   button3.Set_Height (3);

   -- Initialize focus on the first button
   Handler.Initialize_Focus;
   
   -- Display the UI
   Handler.display_nodes;
   
   Put_Line("Navigation Instructions:");
   Put_Line("- Use arrow keys to navigate between buttons");
   Put_Line("- Press Enter or Space to activate a button");
   Put_Line("- Press 'q' to quit");
   
   -- Main event loop
   declare
      Key : Character;
      Available : Boolean;
      ESC_Detected : Boolean := False;
      ESC_Sequence : String(1..3);
      ESC_Count : Natural;
   begin
      loop
         begin
            -- Handle normal input or escape sequences
            if not ESC_Detected then
               Ada.Text_IO.Get_Immediate(Key, Available);
               
               if Available then
                  -- Check for quit command
                  if Key = 'q' or else Key = 'Q' then
                     exit;
                  elsif Key = ASCII.ESC then
                     -- Start of escape sequence
                     ESC_Detected := True;
                     ESC_Count := 1;
                     ESC_Sequence(ESC_Count) := Key;
                  elsif Key = ASCII.CR or else Key = ' ' then
                     -- Enter or Space to select the focused button
                     Handler.Select_Focused_Widget;
                     Handler.update_render := True;
                  elsif Key = 'w' or else Key = 'W' then
                     -- Alternative for up arrow
                     Handler.Navigate_Up;
                     Handler.update_render := True;
                  elsif Key = 's' or else Key = 'S' then
                     -- Alternative for down arrow
                     Handler.Navigate_Down;
                     Handler.update_render := True;
                  elsif Key = 'a' or else Key = 'A' then
                     -- Alternative for left arrow
                     Handler.Navigate_Left;
                     Handler.update_render := True;
                  elsif Key = 'd' or else Key = 'D' then
                     -- Alternative for right arrow
                     Handler.Navigate_Right;
                     Handler.update_render := True;
                  else
                     -- Other keys
                     Handler.Handle_Key_Press(Key);
                  end if;
               end if;
            else
               -- We're in the middle of an escape sequence
               Ada.Text_IO.Get_Immediate(Key, Available);
               
               if Available then
                  ESC_Count := ESC_Count + 1;
                  ESC_Sequence(ESC_Count) := Key;
                  
                  -- Check if we have a complete arrow key sequence
                  if ESC_Count = 3 and then ESC_Sequence(1) = ASCII.ESC and then 
                     ESC_Sequence(2) = '[' then
                     case ESC_Sequence(3) is
                        when 'A' => -- Up arrow
                           Handler.Navigate_Up;
                           Handler.update_render := True;
                        when 'B' => -- Down arrow
                           Handler.Navigate_Down;
                           Handler.update_render := True;
                        when 'C' => -- Right arrow
                           Handler.Navigate_Right;
                           Handler.update_render := True;
                        when 'D' => -- Left arrow
                           Handler.Navigate_Left;
                           Handler.update_render := True;
                        when others => null;
                     end case;
                     ESC_Detected := False;
                  elsif ESC_Count >= 3 then
                     -- Invalid or unsupported escape sequence
                     ESC_Detected := False;
                  end if;
               end if;
            end if;
            
            -- Update the display if needed
            if Handler.update_render then
               -- Clear screen completely
               Put(ASCII.ESC & "[2J");  -- Clear entire screen
               Put(ASCII.ESC & "[H");   -- Move cursor to home position
               
               -- Redraw UI
               Handler.display_nodes;
               
               Put_Line("Navigation Instructions:");
               Put_Line("- Use arrow keys or WASD to navigate between buttons");
               Put_Line("- Press Enter or Space to activate a button");
               Put_Line("- Press 'q' to quit");
               
               Handler.update_render := False;
            end if;
            
            -- Small delay to prevent CPU hogging
            delay 0.01;
         exception
            when Constraint_Error =>
               -- Reset state and continue
               Put_Line("Warning: Invalid character input detected");
               ESC_Detected := False;
               delay 0.1;  -- Longer delay after error
         end;
      end loop;
   end;

end Container_Test;
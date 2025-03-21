with Ada.Text_IO;
with Keyboard; use Keyboard;

procedure Input_Test is
   -- Test basic key detection
   procedure Test_Basic_Keys is
      Key : Key_Event;
   begin
      Ada.Text_IO.Put_Line("=== Basic Key Detection Test ===");
      Ada.Text_IO.Put_Line("Press any key (ESC to exit this test)...");
      Ada.Text_IO.Put_Line("Try arrow keys, shift, and typing with caps lock on/off");
      
      loop
         if Key_Available then
            Key := Get_Key;
            
            if Key.Special /= Key_None then
               Ada.Text_IO.Put("Special key: ");
               case Key.Special is
                  when Key_Up => Ada.Text_IO.Put_Line("UP ARROW");
                  when Key_Down => Ada.Text_IO.Put_Line("DOWN ARROW");
                  when Key_Left => Ada.Text_IO.Put_Line("LEFT ARROW");
                  when Key_Right => Ada.Text_IO.Put_Line("RIGHT ARROW");
                  when Key_Enter => Ada.Text_IO.Put_Line("ENTER");
                  when Key_Escape => Ada.Text_IO.Put_Line("ESCAPE");
                  when Key_Backspace => Ada.Text_IO.Put_Line("BACKSPACE");
                  when Key_Delete => Ada.Text_IO.Put_Line("DELETE");
                  when Key_Tab => Ada.Text_IO.Put_Line("TAB");
                  when Key_Home => Ada.Text_IO.Put_Line("HOME");
                  when Key_End => Ada.Text_IO.Put_Line("END");
                  when Key_Page_Up => Ada.Text_IO.Put_Line("PAGE UP");
                  when Key_Page_Down => Ada.Text_IO.Put_Line("PAGE DOWN");
                  when Key_None => Ada.Text_IO.Put_Line("NONE");
               end case;
            else
               Ada.Text_IO.Put_Line("Character: '" & Key.Char & "'");
            end if;
            
            -- Display modifier keys if any are pressed
            if Key.Shift then
               Ada.Text_IO.Put_Line("Modifier: SHIFT pressed");
            end if;
            
            -- Exit on ESC
            if Key.Special = Key_Escape then
               exit;
            end if;
         end if;
         
         delay 0.01; -- Small delay to prevent busy waiting
      end loop;
      
      Ada.Text_IO.New_Line;
   end Test_Basic_Keys;
   
   -- Test arrow key navigation
   procedure Test_Arrow_Navigation is
      Key : Key_Event;
      X, Y : Integer := 10;
      Max_X : constant Integer := 20;
      Max_Y : constant Integer := 5;
   begin
      Ada.Text_IO.Put_Line("=== Arrow Key Navigation Test ===");
      Ada.Text_IO.Put_Line("Use arrow keys to move the 'X' (ESC to exit)");
      Ada.Text_IO.New_Line;
      
      -- Initial display
      for I in 1..Max_Y loop
         for J in 1..Max_X loop
            if I = Y and J = X then
               Ada.Text_IO.Put("X");
            else
               Ada.Text_IO.Put(".");
            end if;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
      
      loop
         if Key_Available then
            Key := Get_Key;
            
            -- Update position based on arrow keys
            case Key.Special is
               when Key_Up =>
                  if Y > 1 then Y := Y - 1; end if;
               when Key_Down =>
                  if Y < Max_Y then Y := Y + 1; end if;
               when Key_Left =>
                  if X > 1 then X := X - 1; end if;
               when Key_Right =>
                  if X < Max_X then X := X + 1; end if;
               when Key_Escape =>
                  exit;
               when others =>
                  null;
            end case;
            
            -- Clear screen (ANSI escape sequence)
            Ada.Text_IO.Put(ASCII.ESC & "[2J");
            Ada.Text_IO.Put(ASCII.ESC & "[H");
            
            Ada.Text_IO.Put_Line("=== Arrow Key Navigation Test ===");
            Ada.Text_IO.Put_Line("Use arrow keys to move the 'X' (ESC to exit)");
            Ada.Text_IO.New_Line;
            
            -- Redraw
            for I in 1..Max_Y loop
               for J in 1..Max_X loop
                  if I = Y and J = X then
                     Ada.Text_IO.Put("X");
                  else
                     Ada.Text_IO.Put(".");
                  end if;
               end loop;
               Ada.Text_IO.New_Line;
            end loop;
         end if;
         
         delay 0.01; -- Small delay to prevent busy waiting
      end loop;
      
      Ada.Text_IO.New_Line;
   end Test_Arrow_Navigation;
   
   -- Test waiting for a key
   procedure Test_Wait_For_Key is
      Key : Key_Event;
   begin
      Ada.Text_IO.Put_Line("=== Wait For Key Test ===");
      Ada.Text_IO.Put_Line("Press any key to continue...");
      
      Key := Wait_For_Key;
      
      if Key.Special /= Key_None then
         Ada.Text_IO.Put_Line("You pressed a special key");
      else
         Ada.Text_IO.Put_Line("You pressed: '" & Key.Char & "'");
      end if;
      
      Ada.Text_IO.New_Line;
   end Test_Wait_For_Key;
   
begin
   Ada.Text_IO.Put_Line("Keyboard Input Test Suite");
   Ada.Text_IO.Put_Line("========================");
   Ada.Text_IO.New_Line;
   
   -- Start the keyboard handler
   Start;
   
   -- Run test cases
   Test_Wait_For_Key;
   Test_Basic_Keys;
   Test_Arrow_Navigation;
   
   -- Stop the keyboard handler
   Stop;
   
   Ada.Text_IO.Put_Line("All tests completed.");
end Input_Test;
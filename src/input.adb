with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Input is
   -- Global variable to store the current input
   Current_Input : Input_T := (ASCII.NUL, Undefined);
   
   function Get_Input return Input_T is
   begin
      return Current_Input;
   end Get_Input;
   
   procedure Reset_Input is
   begin
      Current_Input := (ASCII.NUL, Undefined);
   end Reset_Input;
   
   -- Safe procedure to display character information without risking encoding issues
   procedure Safe_Put_Character(Ch : Character) is
   begin
      if Is_Control(Ch) or else Character'Pos(Ch) > 127 then
         -- For control or non-ASCII characters, just show the code
         Put_Line("Key code: " & Integer'Image(Character'Pos(Ch)));
      else
         -- For printable ASCII characters, show both the character and code
         Put("Key pressed: '");
         Put(Ch);
         Put_Line("' (ASCII: " & Integer'Image(Character'Pos(Ch)) & ")");
      end if;
   end Safe_Put_Character;
   
   procedure Simple is
      Ch1, Ch2, Ch3 : Character;
      Available : Boolean;
   begin
      Put_Line("Press any key to see its value (Escape to exit)");
      
      loop
         begin
            -- Get key without waiting for Enter
            Ada.Text_IO.Get_Immediate(Ch1, Available);
            
            if Available then
               begin
                  if Character'Pos(Ch1) = 27 then -- ESC
                     -- Could be the start of an arrow key sequence or just ESC
                     
                     -- Wait briefly for more input (part of arrow key sequence)
                     delay 0.001;
                     
                     -- Try to get the next character
                     Ada.Text_IO.Get_Immediate(Ch2, Available);
                     
                     if Available and then Ch2 = '[' then
                        -- Get the direction character
                        Ada.Text_IO.Get_Immediate(Ch3, Available);
                        
                        if Available then
                           -- Process arrow key
                           case Ch3 is
                              when 'A' => 
                                 Put_Line("UP ARROW");
                                 Current_Input := (ASCII.NUL, Up);
                              when 'B' => 
                                 Put_Line("DOWN ARROW");
                                 Current_Input := (ASCII.NUL, Down);
                              when 'C' => 
                                 Put_Line("RIGHT ARROW");
                                 Current_Input := (ASCII.NUL, Right);
                              when 'D' => 
                                 Put_Line("LEFT ARROW");
                                 Current_Input := (ASCII.NUL, Left);
                              when others =>
                                 Put_Line("Unknown arrow key code: " & Integer'Image(Character'Pos(Ch3)));
                                 Reset_Input;
                           end case;
                        end if;
                     else
                        -- Just ESC by itself
                        Put_Line("Escape key detected. Exiting...");
                        exit;
                     end if;
                  else
                     -- Regular character - use safe character display
                     Safe_Put_Character(Ch1);
                     Current_Input := (Ch1, Undefined);
                  end if;
                  
                  -- Display current input state safely
                  if Current_Input.Key /= ASCII.NUL then
                     Put_Line("Current input key code: " & Integer'Image(Character'Pos(Current_Input.Key)));
                  elsif Current_Input.Arrow /= Undefined then
                     Put_Line("Current input: Arrow = " & Arrow_T'Image(Current_Input.Arrow));
                  end if;
               exception
                  when E : others =>
                     Put_Line("Error in inner block: " & Exception_Message(E));
                     Put_Line("Name: " & Exception_Name(E));
                     Reset_Input;
               end;
               
               -- Reset after processing
               delay 0.2;  -- Give time to see the result
               Reset_Input;
            end if;
            
            -- Small delay to prevent CPU overuse
            delay 0.01;
         exception
            when E : others =>
               Put_Line("Error in outer loop: " & Exception_Message(E));
               Put_Line("Name: " & Exception_Name(E));
               Reset_Input;
         end;
      end loop;
   end Simple;
end Input;
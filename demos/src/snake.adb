with Ascii_Engine;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;
with Snake_Core;
with Handler;

procedure Snake is
   Input_Task : Snake_Core.Input;
   Cmd : Character;
   
   -- For controlling game speed
   Last_Update : Ada.Calendar.Time;
   Update_Interval : constant Duration := 0.2; -- Snake speed (lower = faster)
begin
   -- Initialize the widget system
   Handler.Initialize_Focus;
   
   -- Initialize the snake game with widgets
   Snake_Core.Initialize_Game;
   
   -- Initial render
   Snake_Core.Render_Game;
   
   -- Initialize timing
   Last_Update := Ada.Calendar.Clock;
   
   -- Main game loop
   loop
      -- Get command from input task
      Cmd := Snake_Core.Get_Command;
      
      -- Check if we should quit
      if Snake_Core.Quit_Game(Cmd) then
         exit;
      end if;
      
      -- Update game at regular intervals
      declare
         use type Ada.Calendar.Time;
         Current_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         if Current_Time - Last_Update >= Update_Interval then
            -- Update game state
            Snake_Core.Update_Game(Cmd);
            
            -- Reset command after processing
            Snake_Core.Set_Command(' ');
            
            -- Update timing
            Last_Update := Current_Time;
            
            -- Render the updated game
            Snake_Core.Render_Game;
            
            -- Check if game is over
            if Snake_Core.Game_Over then
               Put_Line("Game Over! Press 'q' to quit.");
            end if;
         end if;
      end;
      
      -- Small delay to prevent CPU hogging
      delay 0.01;
   end loop;
   
   -- Stop the input task
   Input_Task.Stop;
   
   Put_Line("Thanks for playing!");
end Snake;

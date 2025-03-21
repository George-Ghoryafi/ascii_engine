with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

-- This package was used to handle arrow keys https://searchcode.com/codesearch/view/13693070/

package body Keyboard is
   -- Protected body for thread-safe queue access
   protected body Event_Queue_Manager is
      procedure Enqueue(Event : Key_Event) is
      begin
         Queue.Append(Event);
      end Enqueue;
      
      procedure Dequeue(Event : out Key_Event; Success : out Boolean) is
      begin
         if Queue.Is_Empty then
            Success := False;
            Event := (others => <>);
         else
            Event := Queue.First_Element;
            Queue.Delete_First;
            Success := True;
         end if;
      end Dequeue;
      
      function Is_Empty return Boolean is
      begin
         return Queue.Is_Empty;
      end Is_Empty;
   end Event_Queue_Manager;
   
   -- Start the keyboard input handler
   procedure Start is
   begin
      if not Running then
         Keyboard_Handler.Start;
      end if;
   end Start;
   
   -- Stop the keyboard input handler
   procedure Stop is
   begin
      if Running then
         Keyboard_Handler.Stop;
      end if;
   end Stop;
   
   -- Check if a key is available
   function Key_Available return Boolean is
   begin
      return not Event_Queue_Manager.Is_Empty;
   end Key_Available;
   
   -- Get the next key event (non-blocking)
   function Get_Key return Key_Event is
      Event : Key_Event;
      Success : Boolean;
   begin
      Event_Queue_Manager.Dequeue(Event, Success);
      return Event;
   end Get_Key;
   
   -- Wait for and get the next key event (blocking)
   function Wait_For_Key return Key_Event is
      Event : Key_Event;
      Success : Boolean;
   begin
      -- Wait until a key is available
      loop
         Event_Queue_Manager.Dequeue(Event, Success);
         if Success then
            return Event;
         end if;
         delay 0.01;  -- Small delay to prevent busy waiting
      end loop;
   end Wait_For_Key;
   
   -- Check if a specific key is currently pressed
   function Is_Key_Down(Key : Special_Key) return Boolean is
      -- Windows virtual key codes
      VK_LEFT     : constant := 37;
      VK_UP       : constant := 38;
      VK_RIGHT    : constant := 39;
      VK_DOWN     : constant := 40;
      VK_ESCAPE   : constant := 27;
      VK_RETURN   : constant := 13;
      VK_TAB      : constant := 9;
      
      -- Function to check if a key is pressed (Windows-specific)
      function Get_Async_Key_State(VKey : Integer) return Integer;
      pragma Import(StdCall, Get_Async_Key_State, "GetAsyncKeyState");
      
      Result : Integer;
      Mask : constant Unsigned_32 := 16#8000#;
      VKey : Integer;
   begin
      case Key is
         when Key_Up => VKey := VK_UP;
         when Key_Down => VKey := VK_DOWN;
         when Key_Left => VKey := VK_LEFT;
         when Key_Right => VKey := VK_RIGHT;
         when Key_Escape => VKey := VK_ESCAPE;
         when Key_Enter => VKey := VK_RETURN;
         when Key_Tab => VKey := VK_TAB;
         when others => return False;
      end case;
      
      Result := Get_Async_Key_State(VKey);
      return (Unsigned_32(Result) and Mask) /= 0;
   end Is_Key_Down;
   
   -- Simplified keyboard input handling using Ada.Text_IO
   procedure Process_Key(Event : out Key_Event) is
      C : Character;
      Available : Boolean;
      
      -- Windows virtual key codes
      VK_SHIFT    : constant := 16;
      VK_CAPITAL  : constant := 20;
      VK_LEFT     : constant := 37;
      VK_UP       : constant := 38;
      VK_RIGHT    : constant := 39;
      VK_DOWN     : constant := 40;
      
      -- Function to check if a key is pressed (Windows-specific)
      function Get_Async_Key_State(VKey : Integer) return Integer;
      pragma Import(StdCall, Get_Async_Key_State, "GetAsyncKeyState");
      
      -- Function to get the keyboard state
      type Byte_Array is array (0..255) of unsigned_char;
      function Get_Keyboard_State(KeyState : access Byte_Array) return Integer;
      pragma Import(StdCall, Get_Keyboard_State, "GetKeyboardState");
      
      -- Check if a specific key is pressed
      function Is_Key_Pressed(VKey : Integer) return Boolean is
         Result : Integer;
         -- Use both masks to check for both current state and transition state
         Mask_Down : constant Unsigned_32 := 16#0001#;  -- Key is down
         Mask_Pressed : constant Unsigned_32 := 16#8000#;  -- Key was pressed since last call
      begin
         Result := Get_Async_Key_State(VKey);
         -- Check if either the key is currently down or was pressed since last call
         return ((Unsigned_32(Result) and Mask_Down) /= 0) or 
                ((Unsigned_32(Result) and Mask_Pressed) /= 0);
      end Is_Key_Pressed;
      
      -- Get the state of all keys
      KeyState : aliased Byte_Array;
      KeyStateResult : Integer;
   begin
      -- Initialize event
      Event := (others => <>);
      
      -- Get the state of all keys
      KeyStateResult := Get_Keyboard_State(KeyState'Access);
      
      -- Check for arrow keys first with improved detection
      if Is_Key_Pressed(VK_UP) then
         Event.Special := Key_Up;
         -- Debug output to verify arrow key detection
         Ada.Text_IO.Put_Line("DEBUG: Up arrow detected");
         return;
      elsif Is_Key_Pressed(VK_DOWN) then
         Event.Special := Key_Down;
         Ada.Text_IO.Put_Line("DEBUG: Down arrow detected");
         return;
      elsif Is_Key_Pressed(VK_LEFT) then
         Event.Special := Key_Left;
         Ada.Text_IO.Put_Line("DEBUG: Left arrow detected");
         return;
      elsif Is_Key_Pressed(VK_RIGHT) then
         Event.Special := Key_Right;
         Ada.Text_IO.Put_Line("DEBUG: Right arrow detected");
         return;
      end if;
      
      -- Check for shift key
      Event.Shift := Is_Key_Pressed(VK_SHIFT);
      
      -- Check if a character key is available
      Ada.Text_IO.Get_Immediate(C, Available);
      
      if not Available then
         return;
      end if;
      
      -- Process the character
      if C = ASCII.ESC then
         -- Could be the start of an escape sequence
         delay 0.01;  -- Give time for the rest of the sequence to arrive
         
         Ada.Text_IO.Get_Immediate(C, Available);
         if Available and then C = '[' then
            -- Arrow keys and other special keys
            Ada.Text_IO.Get_Immediate(C, Available);
            if Available then
               case C is
                  when 'A' =>  -- Up arrow
                     Event.Special := Key_Up;
                  when 'B' =>  -- Down arrow
                     Event.Special := Key_Down;
                  when 'C' =>  -- Right arrow
                     Event.Special := Key_Right;
                  when 'D' =>  -- Left arrow
                     Event.Special := Key_Left;
                  when 'H' =>  -- Home
                     Event.Special := Key_Home;
                  when 'F' =>  -- End
                     Event.Special := Key_End;
                  when '5' =>  -- Page Up (needs additional character)
                     Ada.Text_IO.Get_Immediate(C, Available);
                     if Available and then C = '~' then
                        Event.Special := Key_Page_Up;
                     end if;
                  when '6' =>  -- Page Down (needs additional character)
                     Ada.Text_IO.Get_Immediate(C, Available);
                     if Available and then C = '~' then
                        Event.Special := Key_Page_Down;
                     end if;
                  when '3' =>  -- Delete (needs additional character)
                     Ada.Text_IO.Get_Immediate(C, Available);
                     if Available and then C = '~' then
                        Event.Special := Key_Delete;
                     end if;
                  when others =>
                     -- Other escape sequences
                     null;
               end case;
            end if;
         else
            -- Just ESC key
            Event.Special := Key_Escape;
         end if;
      elsif C = ASCII.BS or C = Character'Val(127) then  -- Backspace
         Event.Special := Key_Backspace;
      elsif C = ASCII.CR or C = ASCII.LF then  -- Enter
         Event.Special := Key_Enter;
      elsif C = ASCII.HT then  -- Tab
         Event.Special := Key_Tab;
      elsif C = Character'Val(127) then  -- Delete
         Event.Special := Key_Delete;
      else
         -- Regular character
         Event.Char := C;
         
         -- Apply caps lock effect if needed
         if (KeyState(VK_CAPITAL) and 1) /= 0 then
            if C in 'a'..'z' then
               Event.Char := Character'Val(Character'Pos(C) - Character'Pos('a') + Character'Pos('A'));
            elsif C in 'A'..'Z' then
               Event.Char := Character'Val(Character'Pos(C) - Character'Pos('A') + Character'Pos('a'));
            end if;
         end if;
      end if;
   end Process_Key;
   
   -- Task body for keyboard handler
   task body Keyboard_Task is
      Event : Key_Event;
   begin
      accept Start do
         Running := True;
      end Start;
      
      -- Main input loop
      while Running loop
         begin
            Process_Key(Event);
            
            -- Only enqueue valid events
            if Event.Special /= Key_None or else Event.Char /= ASCII.NUL then
               Event_Queue_Manager.Enqueue(Event);
            end if;
            
            -- Small delay to prevent busy waiting but allow repeated keys
            delay 0.005;
         exception
            when others =>
               -- Log error and continue
               null;
         end;
         
         -- Check for stop request
         select
            accept Stop do
               Running := False;
            end Stop;
         else
            null;  -- Continue if no stop request
         end select;
      end loop;
   end Keyboard_Task;
   
end Keyboard;
with Ada.Task_Identification;
with Ada.Containers.Vectors;

package Keyboard is
   -- Key codes for special keys
   type Special_Key is (
      Key_None,
      Key_Up,
      Key_Down,
      Key_Left,
      Key_Right,
      Key_Enter,
      Key_Escape,
      Key_Backspace,
      Key_Delete,
      Key_Tab,
      Key_Home,
      Key_End,
      Key_Page_Up,
      Key_Page_Down
   );
   
   -- Represents a keyboard event
   type Key_Event is record
      Special : Special_Key := Key_None;
      Char : Character := ASCII.NUL;
      Alt : Boolean := False;
      Control : Boolean := False;
      Shift : Boolean := False;
   end record;
   
   -- Vector for key events
   package Key_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Key_Event);
   
   -- Start the keyboard input handler
   procedure Start;
   
   -- Stop the keyboard input handler
   procedure Stop;
   
   -- Check if a key is available
   function Key_Available return Boolean;
   
   -- Get the next key event (non-blocking)
   function Get_Key return Key_Event;
   
   -- Wait for and get the next key event (blocking)
   function Wait_For_Key return Key_Event;
   
private
   -- Task for handling keyboard input
   task type Keyboard_Task is
      entry Start;
      entry Stop;
   end Keyboard_Task;
   
   -- The keyboard input task
   Keyboard_Handler : Keyboard_Task;
   
   -- Queue for key events
   Event_Queue : Key_Vectors.Vector;
   
   -- Flag to indicate if the handler is running
   Running : Boolean := False;
   
   -- Protected type for thread-safe access to the event queue
   protected Event_Queue_Manager is
      procedure Enqueue(Event : Key_Event);
      procedure Dequeue(Event : out Key_Event; Success : out Boolean);
      function Is_Empty return Boolean;
   private
      Queue : Key_Vectors.Vector;
   end Event_Queue_Manager;
   
   -- Check if a specific key is currently pressed
   function Is_Key_Down(Key : Special_Key) return Boolean;
end Keyboard;
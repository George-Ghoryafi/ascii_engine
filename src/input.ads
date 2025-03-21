with Ada.Unchecked_Deallocation;
with Container;

package Input is
   
   -- Input element type
   type Input_Type is tagged private;
   
   -- Create a new input element with optional placeholder text and background color
   function Create (Placeholder : String := "";
                   Width : Positive := 20;
                   Background_Color : Container.Color_Type := Container.Default_Color) 
                   return Input_Type;
   
   -- Render the input element to the terminal
   procedure Render (Input_Element : Input_Type);
   
   -- Set the text content of the input element
   procedure Set_Text (Input_Element : in out Input_Type; Text : String);
   
   -- Get the current text content of the input element
   function Get_Text (Input_Element : Input_Type) return String;
   
   -- Free the memory used by the input element
   procedure Free (Input_Element : in out Input_Type);
   
private
   -- Define our own string access type
   type String_Access is access String;
   
   type Input_Type is tagged record
      Container_Element : Container.Container_Type;
      Text_Content : String_Access := null;
      Width : Positive;
      Cursor_Position : Natural := 0;
      Is_Focused : Boolean := False;
   end record;
   
   procedure Free_String is new Ada.Unchecked_Deallocation(String, String_Access);
   
end Input;
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Input is
   
   -- Create a new input element
   function Create (Placeholder : String := "";
                   Width : Positive := 20;
                   Background_Color : Container.Color_Type := Container.Default_Color) 
                   return Input_Type is
      Result : Input_Type;
      Initial_Text : String := (if Placeholder'Length > 0 then Placeholder else "");
   begin
      -- Create the underlying container
      Result.Container_Element := Container.Create(Initial_Text, Background_Color);
      
      -- Store the width
      Result.Width := Width;
      
      -- Allocate memory for the text content
      Result.Text_Content := new String'(Initial_Text);
      
      -- Set cursor position at the end of the text
      Result.Cursor_Position := Initial_Text'Length;
      
      return Result;
   end Create;
   
   -- Render the input element to the terminal
   procedure Render (Input_Element : Input_Type) is
      Display_Text : String := Get_Text(Input_Element);
      
      -- Create a temporary container with the current text and cursor
      Temp_Container : Container.Container_Type;
      
      -- If the input is focused, show a cursor
      Cursor_Text : String := Display_Text;
   begin
      if Input_Element.Is_Focused then
         -- Insert cursor character at the cursor position
         if Input_Element.Cursor_Position < Cursor_Text'Length then
            Cursor_Text := Display_Text(1..Input_Element.Cursor_Position) & 
                          "|" & 
                          Display_Text(Input_Element.Cursor_Position+1..Display_Text'Length);
         else
            Cursor_Text := Display_Text & "|";
         end if;
      end if;
      
      -- Create a temporary container with the current text
      Temp_Container := Container.Create(
         Cursor_Text,
         Input_Element.Container_Element.Background
      );
      
      -- Render the container
      Container.Render(Temp_Container);
      
      -- Free the temporary container
      Container.Free(Temp_Container);
   end Render;
   
   -- Set the text content of the input element
   procedure Set_Text (Input_Element : in out Input_Type; Text : String) is
   begin
      -- Free the old text content
      if Input_Element.Text_Content /= null then
         Free_String(Input_Element.Text_Content);
      end if;
      
      -- Allocate memory for the new text content
      Input_Element.Text_Content := new String'(Text);
      
      -- Update the cursor position if needed
      if Input_Element.Cursor_Position > Text'Length then
         Input_Element.Cursor_Position := Text'Length;
      end if;
      
      -- Update the underlying container
      Container.Free(Input_Element.Container_Element);
      Input_Element.Container_Element := Container.Create(
         Text,
         Input_Element.Container_Element.Background
      );
   end Set_Text;
   
   -- Get the current text content of the input element
   function Get_Text (Input_Element : Input_Type) return String is
   begin
      if Input_Element.Text_Content = null then
         return "";
      else
         return Input_Element.Text_Content.all;
      end if;
   end Get_Text;
   
   -- Set focus state of the input element
   procedure Set_Focus (Input_Element : in out Input_Type; Focused : Boolean) is
   begin
      Input_Element.Is_Focused := Focused;
   end Set_Focus;
   
   -- Move cursor left
   procedure Move_Cursor_Left (Input_Element : in out Input_Type) is
   begin
      if Input_Element.Cursor_Position > 0 then
         Input_Element.Cursor_Position := Input_Element.Cursor_Position - 1;
      end if;
   end Move_Cursor_Left;
   
   -- Move cursor right
   procedure Move_Cursor_Right (Input_Element : in out Input_Type) is
   begin
      if Input_Element.Cursor_Position < Get_Text(Input_Element)'Length then
         Input_Element.Cursor_Position := Input_Element.Cursor_Position + 1;
      end if;
   end Move_Cursor_Right;
   
   -- Free the memory used by the input element
   procedure Free (Input_Element : in out Input_Type) is
   begin
      -- Free the text content
      if Input_Element.Text_Content /= null then
         Free_String(Input_Element.Text_Content);
      end if;
      
      -- Free the container
      Container.Free(Input_Element.Container_Element);
   end Free;
   
end Input;
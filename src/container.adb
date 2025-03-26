with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Container is

   -- Split a string by line feeds
   function Split_By_LF (Text : String) return String_Array_Access is
      -- Count the number of lines
      Line_Count : Natural := 1;
      Pos : Natural := Text'First;
   begin
      -- Count line feeds to determine array size
      for I in Text'Range loop
         if Text(I) = ASCII.LF then
            Line_Count := Line_Count + 1;
         end if;
      end loop;
      
      -- Create the array
      declare
         Result : String_Array_Access := new String_Array(1..Line_Count);
         Line_Start : Natural := Text'First;
         Line_Index : Natural := 1;
      begin
         -- Split the text by line feeds
         for I in Text'Range loop
            if Text(I) = ASCII.LF or I = Text'Last then
               declare
                  Line_End : Natural;
               begin
                  if Text(I) = ASCII.LF then
                     Line_End := I - 1;
                  else
                     Line_End := I;
                  end if;
                  
                  -- Create a new string for this line
                  Result(Line_Index) := new String'(Text(Line_Start..Line_End));
                  Line_Index := Line_Index + 1;
                  Line_Start := I + 1;
               end;
            end if;
         end loop;
         
         return Result;
      end;
   end Split_By_LF;
   
   -- Create a new container with the given text and optional background color
   function Create (Text : String; Background_Color : Color_Type := Default_Color) 
                   return Container_Type is
      Lines : String_Array_Access;
      Max_Width : Natural := 0;
   begin
      -- Handle empty text case
      if Text'Length = 0 then
         Lines := new String_Array(1..1);
         Lines(1) := new String'("");
         return (Width => 0, Height => 1, Content => Lines, 
                 Background => Background_Color, 
                 Children => null, Child_Count => 0);
      end if;
      
      -- Split the text by line feeds
      Lines := Split_By_LF(Text);
      
      -- Calculate the maximum line width
      for I in Lines'Range loop
         Max_Width := Natural'Max(Max_Width, Lines(I)'Length);
      end loop;
      
      -- Create and return the container
      return (Width => Max_Width, 
              Height => Lines'Length, 
              Content => Lines, 
              Background => Background_Color,
              Children => null,
              Child_Count => 0);
   end Create;
   
   -- Add a child container to a parent container
   procedure Add_Child (Parent : in out Container_Type; Child : Container_Type) is
      New_Children : Container_Array_Access;
      procedure Free_Container is new Ada.Unchecked_Deallocation
        (Container_Type, Container_Access);
   begin
      -- Create a new child container array if needed
      if Parent.Children = null then
         Parent.Children := new Container_Array(1..10);  -- Initial capacity of 10
         Parent.Child_Count := 0;
      elsif Parent.Child_Count = Parent.Children'Length then
         -- Double the capacity if needed
         New_Children := new Container_Array(1..Parent.Children'Length * 2);
         for I in 1..Parent.Child_Count loop
            New_Children(I) := Parent.Children(I);
         end loop;
         
         -- Free the old array
         Parent.Children.all := (others => null);  -- Clear references
         Parent.Children := New_Children;
      end if;
      
      -- Add the child
      Parent.Child_Count := Parent.Child_Count + 1;
      Parent.Children(Parent.Child_Count) := new Container_Type'(Child);
   end Add_Child;
   
   -- Convert hex color code to Color_Type
   function Hex_To_Color (Hex : String) return Color_Type is
      Result : Color_Type;
      
      -- Convert a hex digit to its decimal value
      function Hex_Digit_Value (Digit : Character) return Natural is
      begin
         if Digit in '0'..'9' then
            return Character'Pos(Digit) - Character'Pos('0');
         elsif Digit in 'A'..'F' then
            return Character'Pos(Digit) - Character'Pos('A') + 10;
         elsif Digit in 'a'..'f' then
            return Character'Pos(Digit) - Character'Pos('a') + 10;
         else
            return 0;  -- Invalid hex digit
         end if;
      end Hex_Digit_Value;
      
      -- Convert two hex digits to a decimal value
      function Hex_Byte_Value (MSB, LSB : Character) return Natural is
      begin
         return Hex_Digit_Value(MSB) * 16 + Hex_Digit_Value(LSB);
      end Hex_Byte_Value;
   begin
      -- Default to black if the hex string is invalid
      if Hex'Length /= 7 or Hex(Hex'First) /= '#' then
         return (0, 0, 0);
      end if;
      
      -- Parse the hex color
      Result.Red := Hex_Byte_Value(Hex(Hex'First + 1), Hex(Hex'First + 2));
      Result.Green := Hex_Byte_Value(Hex(Hex'First + 3), Hex(Hex'First + 4));
      Result.Blue := Hex_Byte_Value(Hex(Hex'First + 5), Hex(Hex'First + 6));
      
      return Result;
   end Hex_To_Color;
   
   -- Set terminal background color
   procedure Set_Background_Color (Color : Color_Type) is
   begin
      -- Skip if the color is the default (transparent)
      if Color = Default_Color then
         return;
      end if;
      
      -- Set the background color using ANSI escape codes
      Ada.Text_IO.Put(ASCII.ESC & "[48;2;" & 
                     Ada.Strings.Fixed.Trim(Color.Red'Image, Ada.Strings.Left) & ";" &
                     Ada.Strings.Fixed.Trim(Color.Green'Image, Ada.Strings.Left) & ";" &
                     Ada.Strings.Fixed.Trim(Color.Blue'Image, Ada.Strings.Left) & "m");
   end Set_Background_Color;
   
   -- Reset terminal colors to default
   procedure Reset_Colors is
   begin
      Ada.Text_IO.Put(ASCII.ESC & "[0m");
   end Reset_Colors;
   
   -- Render the container to the terminal
   procedure Render (Container : Container_Type) is
   begin
      -- Render the container content
      for I in 1..Container.Height loop
         Set_Background_Color(Container.Background);
         
         -- Print the line content
         Ada.Text_IO.Put(Container.Content(I).all);
         
         -- Pad with spaces to the container width
         if Container.Content(I)'Length < Container.Width then
            Ada.Text_IO.Put((1..Container.Width - Container.Content(I)'Length => ' '));
         end if;
         
         Reset_Colors;
         Ada.Text_IO.New_Line;
      end loop;
      
      -- Render child containers if any
      if Container.Children /= null then
         for I in 1..Container.Child_Count loop
            Render(Container.Children(I).all);
         end loop;
      end if;
   end Render;
   
   -- Free the memory used by the container
   procedure Free (Container : in out Container_Type) is
      procedure Free_String is new Ada.Unchecked_Deallocation
        (String, String_Access);
      procedure Free_String_Array is new Ada.Unchecked_Deallocation
        (String_Array, String_Array_Access);
      procedure Free_Container is new Ada.Unchecked_Deallocation
        (Container_Type, Container_Access);
      procedure Free_Container_Array is new Ada.Unchecked_Deallocation
        (Container_Array, Container_Array_Access);
   begin
      -- Free the content strings
      if Container.Content /= null then
         for I in Container.Content'Range loop
            if Container.Content(I) /= null then
               Free_String(Container.Content(I));
            end if;
         end loop;
         Free_String_Array(Container.Content);
      end if;
      
      -- Free child containers if any
      if Container.Children /= null then
         for I in 1..Container.Child_Count loop
            if Container.Children(I) /= null then
               Free(Container.Children(I).all);
               Free_Container(Container.Children(I));
            end if;
         end loop;
         Free_Container_Array(Container.Children);
      end if;
      
      -- Reset container fields
      Container.Width := 0;
      Container.Height := 0;
      Container.Content := null;
      Container.Children := null;
      Container.Child_Count := 0;
   end Free;
   
end Container;
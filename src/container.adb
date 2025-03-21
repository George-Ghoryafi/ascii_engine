with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Container is
   
   -- Create a new container with the given text
   function Create (Text : String; Background_Color : Color_Type := Default_Color) return Container_Type is
      Lines : String_Array_Access;
      Max_Width : Natural := 0;
      Line_Count : Natural := 0;
      Current_Line : Ada.Strings.Unbounded.Unbounded_String;
      
   begin
      -- Count the number of lines and find the maximum width
      for I in Text'Range loop
         if Text(I) = ASCII.LF then
            Line_Count := Line_Count + 1;
            Max_Width := Natural'Max(Max_Width, 
                                    Ada.Strings.Unbounded.Length(Current_Line));
            Current_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
         else
            Ada.Strings.Unbounded.Append(Current_Line, Text(I));
         end if;
      end loop;
      
      -- Handle the last line if it doesn't end with a newline
      if Ada.Strings.Unbounded.Length(Current_Line) > 0 then
         Line_Count := Line_Count + 1;
         Max_Width := Natural'Max(Max_Width, 
                                 Ada.Strings.Unbounded.Length(Current_Line));
      end if;
      
      -- Allocate the array for lines
      Lines := new String_Array(1..Line_Count);
      
      -- Fill the array with the lines
      Line_Count := 0;
      Current_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
      
      for I in Text'Range loop
         if Text(I) = ASCII.LF then
            Line_Count := Line_Count + 1;
            Lines(Line_Count) := new String'(Ada.Strings.Unbounded.To_String(Current_Line));
            Current_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
         else
            Ada.Strings.Unbounded.Append(Current_Line, Text(I));
         end if;
      end loop;
      
      -- Handle the last line if it doesn't end with a newline
      if Ada.Strings.Unbounded.Length(Current_Line) > 0 then
         Line_Count := Line_Count + 1;
         Lines(Line_Count) := new String'(Ada.Strings.Unbounded.To_String(Current_Line));
      end if;
      
      -- Create and return the container
      return Container_Type'(
         Width => Max_Width,
         Height => Line_Count,
         Content => Lines,
         Background => Background_Color
      );
   end Create;
   
   -- Render the container to the terminal
   procedure Render (Container : Container_Type) is
   begin
      -- Draw the top border with color if specified
      if Container.Background /= Default_Color then
         Set_Background_Color(Container.Background);
      end if;
      Ada.Text_IO.Put("+");
      Ada.Text_IO.Put((Container.Width + 2) * "-");
      Ada.Text_IO.Put("+");
      Reset_Colors;
      Ada.Text_IO.New_Line;
      
      -- Draw each line of content with side borders
      for I in 1..Container.Height loop
         if Container.Background /= Default_Color then
            Set_Background_Color(Container.Background);
         end if;
         Ada.Text_IO.Put("| ");
         Ada.Text_IO.Put(Container.Content(I).all);
         
         -- Pad with spaces to align the right border
         declare
            Padding : constant Natural := Container.Width - Container.Content(I)'Length;
         begin
            if Padding > 0 then
               Ada.Text_IO.Put(Padding * " ");
            end if;
         end;
         
         Ada.Text_IO.Put(" |");
         Reset_Colors;
         Ada.Text_IO.New_Line;
      end loop;
      
      -- Draw the bottom border with color if specified
      if Container.Background /= Default_Color then
         Set_Background_Color(Container.Background);
      end if;
      Ada.Text_IO.Put("+");
      Ada.Text_IO.Put((Container.Width + 2) * "-");
      Ada.Text_IO.Put("+");
      Reset_Colors;
      Ada.Text_IO.New_Line;
   end Render;
   
   -- Convert hex color code to Color_Type
   function Hex_To_Color (Hex : String) return Color_Type is
      R, G, B : Natural;
   begin
      -- Check if the hex code is valid (should be "#RRGGBB" format)
      if Hex'Length /= 7 or else Hex(Hex'First) /= '#' then
         return Default_Color;
      end if;
      
      -- Convert hex to RGB components
      R := Integer'Value("16#" & Hex(Hex'First + 1 .. Hex'First + 2) & "#");
      G := Integer'Value("16#" & Hex(Hex'First + 3 .. Hex'First + 4) & "#");
      B := Integer'Value("16#" & Hex(Hex'First + 5 .. Hex'First + 6) & "#");
      
      return (R, G, B);
   exception
      when others =>
         -- Return default color if conversion fails
         return Default_Color;
   end Hex_To_Color;
   
   -- Set terminal background color
   procedure Set_Background_Color (Color : Color_Type) is
      R_Str : String := Trim(Color.Red'Image, Ada.Strings.Left);
      G_Str : String := Trim(Color.Green'Image, Ada.Strings.Left);
      B_Str : String := Trim(Color.Blue'Image, Ada.Strings.Left);
   begin
      -- ANSI escape sequence for setting background color
      Ada.Text_IO.Put(ASCII.ESC & "[48;2;" & 
                     R_Str & ";" & 
                     G_Str & ";" & 
                     B_Str & "m");
   end Set_Background_Color;
   
   -- Reset terminal colors to default
   procedure Reset_Colors is
   begin
      Ada.Text_IO.Put(ASCII.ESC & "[0m");
   end Reset_Colors;
   
   -- Free the memory used by the container
   procedure Free (Container : in out Container_Type) is
      procedure Free_String is new Ada.Unchecked_Deallocation(
         String, String_Access);
      procedure Free_String_Array is new Ada.Unchecked_Deallocation(
         String_Array, String_Array_Access);
   begin
      -- Free each string in the array
      for I in Container.Content'Range loop
         Free_String(Container.Content(I));
      end loop;
      
      -- Free the array itself
      Free_String_Array(Container.Content);
   end Free;
   
end Container;
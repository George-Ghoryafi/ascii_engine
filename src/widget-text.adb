with handler; 
with Widget; use Widget;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Widget.Text is 
   function Create 
   (
      id : String; 
      parent : Widget.Any_Acc; 
      priority : Natural := 0; 
      min_height, min_width : Natural := 0;
      max_height, max_width : Natural := Natural'Last;
      bgd_color : Widget.color_t := Widget.default_color;
      text : string; 
      text_color : Widget.color_t := Widget.white;
      overflow : text_overflow := default
   ) return Widget.Any_Acc is
      This : Widget.Any_Acc; 
   begin 
      This := new Instance'(Ada.Finalization.Controlled with
                            id => +id, 
                            priority => priority,
                            min_height => min_height,
                            min_width => min_width,
                            max_height => max_height,
                            max_width => max_width,
                            bgd_color => bgd_color, 
                            text => +text,
                            text_color => text_color,
                            overflow => overflow, 
                            others => <>);

      Handler.add_to_LOT (This, parent);
      return This;
   end Create; 


   procedure render (This : in out Instance) is
      procedure find_adjusted_dimensions (This : in out Instance) is
         Lines : String_Array_Access; 
         Line_Count : Natural := 0;
         New_Width : Natural := 0;
         Current_Line : Ada.Strings.Unbounded.Unbounded_String;
         widget_text : String := To_String(This.text);
      begin 
         -- Find the adjusted width of the text widget
         for I in widget_text'Range loop 
            if widget_text(I) = ASCII.LF then 
               Line_Count := Line_Count + 1; 
               New_Width := Natural'Max (New_Width, Ada.Strings.Unbounded.Length(Current_Line)); 
               Current_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
            else 
               Ada.Strings.Unbounded.Append(Current_Line, widget_text(I));
            end if;
         end loop;

         if Ada.Strings.Unbounded.Length(Current_Line) > 0 then 
            Line_Count := Line_Count + 1; 
            New_Width := Natural'Max (New_Width, Ada.Strings.Unbounded.Length(Current_Line)); 
         end if;

         Lines := new String_Array(1..Line_Count);
         Line_Count := 0;
         Current_Line := Ada.Strings.Unbounded.Null_Unbounded_String;

         for I in widget_text'Range loop 
            if widget_text(I) = ASCII.LF then 
               Line_Count := Line_Count + 1; 
               Lines(Line_Count) := new String'(Ada.Strings.Unbounded.To_String(Current_Line));
               Current_Line := Ada.Strings.Unbounded.Null_Unbounded_String;
            else 
               Ada.Strings.Unbounded.Append(Current_Line, widget_text(I));
            end if;
         end loop;

         if Ada.Strings.Unbounded.Length(Current_Line) > 0 then 
            Line_Count := Line_Count + 1; 
            Lines(Line_Count) := new String'(To_String(Current_Line));
         end if;

         This.width := New_Width + 2;  -- Add 2 for the borders
         This.height := Line_Count;
      end find_adjusted_dimensions;

      procedure horizontal_border (width : Natural) is 
      wide : Natural := width + 2; 
      line : String := To_String(wide * "-"); 
      begin 
         -- Draw the top border of the text widget 
         Put_Line ("+" & line & "+"); 
      end horizontal_border;

      procedure vertical_handling (height : Natural; width: Natural; text: String) is
         Line_Count : Natural := 1;  -- Start at 1 for first line
         wide : Natural := width + 2;
         Current_Pos : Natural := 1;
         Next_LF : Natural;
      begin
         -- Count actual number of lines in text
         for I in text'Range loop
            if text(I) = ASCII.LF then
               Line_Count := Line_Count + 1;
            end if;
         end loop;

         -- Draw the lines of text with borders
         for I in 1 .. Line_Count loop
            Put ("| ");
            
            -- Find next line break or end of string
            Next_LF := Current_Pos;
            while Next_LF <= text'Last and then text(Next_LF) /= ASCII.LF loop
               Next_LF := Next_LF + 1;
            end loop;
            
            -- Extract current line
            declare
               Line_Text : String := text(Current_Pos .. Next_LF - 1);
               Padding : constant Natural := wide - Line_Text'Length - 2;
            begin
               Put (" " & Line_Text);
               if Padding > 0 then
                  Put ((1 .. Padding => ' '));
               end if;
            end;
            
            Put_Line ("|");
            
            -- Move to next line start (skip the LF)
            if Next_LF < text'Last then
               Current_Pos := Next_LF + 1;
            end if;
         end loop;

         -- Add empty lines if widget height is greater than text lines
         for I in Line_Count + 1 .. height loop
            Put ("|");
            Put ((1 .. wide - 2 => ' '));
            Put_Line ("|");
         end loop;
      end vertical_handling;

      
      begin 
         -- Find the adjusted width of the text widget
         find_adjusted_dimensions(This);
         
         if This.bgd_color /= Widget.default_color then
            handler.Set_Background_Color (This.bgd_color);
         end if;
         -- Draw the top border of the text widget 
         horizontal_border (This.width);
         vertical_handling (This.height, This.width, To_String(This.text)); 
         horizontal_border (This.width);
         


         handler.Reset_Color; 
   end render;


begin 
   Put_Line ("Widget.Text package body loaded");
      
end Widget.Text;
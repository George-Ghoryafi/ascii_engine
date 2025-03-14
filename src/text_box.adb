with Ada.Text_IO; use Ada.Text_IO;

package body Text_Box is
   procedure Create (Box : in out Text_Box_Type;
                    Width : Positive;
                    Height : Positive;
                    Title : String := "") is
   begin
      Box.Width := Width;
      Box.Height := Height;
      if Title'Length > 0 then
         Box.Title(1..Title'Length) := Title;
         Box.Title_Length := Title'Length;
      end if;
   end Create;
   
   procedure Display (Box : Text_Box_Type) is
      procedure Draw_Horizontal_Line is
      begin
         Put ("+");
         for I in 1..Box.Width loop
            Put ("-");
         end loop;
         Put_Line ("+");
      end Draw_Horizontal_Line;
   begin
      -- Draw top border with title if exists
      if Box.Title_Length > 0 then
         Put ("+");
         declare
            Title_Start : Integer := (Box.Width - Box.Title_Length) / 2;
         begin
            for I in 1..Title_Start-1 loop
               Put ("-");
            end loop;
            Put (" " & Box.Title(1..Box.Title_Length) & " ");
            for I in Title_Start + Box.Title_Length + 2..Box.Width loop
               Put ("-");
            end loop;
         end;
         Put_Line ("+");
      else
         Draw_Horizontal_Line;
      end if;
      
      -- Draw content
      for I in 1..Box.Height loop
         Put ("|");
         if I <= (Box.Content_Length + Box.Width - 1) / Box.Width then
            declare
               Start_Idx : Integer := (I-1) * Box.Width + 1;
               End_Idx : Integer := Integer'Min(Start_Idx + Box.Width - 1, 
                                              Box.Content_Length);
            begin
               Put (Box.Content(Start_Idx..End_Idx));
               for J in End_Idx - Start_Idx + 2..Box.Width loop
                  Put (" ");
               end loop;
            end;
         else
            for J in 1..Box.Width loop
               Put (" ");
            end loop;
         end if;
         Put_Line ("|");
      end loop;
      
      -- Draw bottom border
      Draw_Horizontal_Line;
   end Display;
   
   procedure Set_Content (Box : in out Text_Box_Type;
                         Content : String) is
   begin
      if Content'Length <= Max_Content_Length then
         Box.Content(1..Content'Length) := Content;
         Box.Content_Length := Content'Length;
      end if;
   end Set_Content;
end Text_Box;
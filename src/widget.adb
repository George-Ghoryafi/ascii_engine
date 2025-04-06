with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Handler; use Handler; 

with System; use System;  -- Add this line

package body Widget is
   function Create 
   (
      id : String; 
      parent : Widget.Any_Acc; 
      priority : Natural := 0; 
      min_height, min_width : Natural := 0;
      max_height, max_width : Natural := Natural'Last;
      bgd_color : color_t := default_color
   ) return Widget.Any_Acc 
   
   is
      This : Widget.Any_Acc; 
   begin 
      This := 
         new Instance'
            (Ada.Finalization.Controlled with id => +id, 
               priority      => priority,
               min_height    => min_height,
               min_width     => min_width,
               max_height    => max_height,
               max_width     => max_width,
               bgd_color     => bgd_color, 
               others        => <>); 
      
      Handler.add_to_LOT (This, parent);
      return This; 
   end Create;



   function Get_Id (This : in out Instance) return Ada.Strings.Unbounded.Unbounded_String is
      begin 
         return This.id; 
      end Get_Id;


   procedure Set_Width (This : in out Instance; calculated_width : Natural) is
      begin 
         if calculated_width < This.min_width then
            This.width := This.min_width;
         elsif calculated_width > This.max_width then
            This.width := This.max_width;
         else
            This.width := calculated_width;
         end if;
      end Set_Width;


   procedure Set_Height (This : in out Instance; calculated_height : Natural) is
      begin 
         if calculated_height < This.min_height then
            This.height := This.min_height;
         elsif calculated_height > This.max_height then
            This.height := This.max_height;
         else
            This.height := calculated_height;
         end if;
      end Set_Height;

   procedure Click (This : in out Instance) is
      begin 
         Put_Line ("Widget " & To_String (This.id) & " clicked!");
      end Click;
   
   function Is_Clickable (This : in out Instance) return Boolean is
      begin 
         return True; -- For now, all widgets are clickable
      end Is_Clickable;



   function Set_Event_Override_Width (This: in out Instance; Parent : Widget.Any_Acc; new_width: Natural) return Natural is
      begin 
         if new_width > Parent.width then 
            This.width := Parent.width;
         elsif new_width < 1 then 
            This.width := 1;
         else
            This.width := new_width;
         end if;
         return This.width;
      end Set_Event_Override_Width;
   
   function Set_Event_Override_Height (This: in out Instance; Parent : Widget.Any_Acc; new_height: Natural) return Natural is
      begin 
         if new_height > Parent.height then 
            This.height := Parent.height;
         elsif new_height < 1 then 
            This.height := 1;
         else
            This.height := new_height;
         end if;
         return This.height;
      end Set_Event_Override_Height;


   procedure render (This : in out Instance) is 
      name : String := To_String (This.id); 
      -- We need to convert the id to a string, as it is an unbounded string
      begin
         -- Now we print the properties of the widget to the screen
         Put_Line ("Widget ID: " & name);
         Put_Line ("Width" & Integer'Image (This.width));
         Put_Line ("Height" & Integer'Image (This.height));
   end render;

end Widget; 
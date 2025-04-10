with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; 
with Ada.Finalization; use Ada.Finalization;
with Ada.Numerics; 
with Ada.Numerics.Elementary_Functions; 
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags; use Ada.Tags;
with Ada.Unchecked_Deallocation;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Widget; use Widget; 
with Widget.Button;

 package body Handler is 
   procedure add_to_LOT (This : Widget.Any_Acc; Parent : Widget.Any_Acc) is
      parent_cursor : Layout_Object_Tree.Cursor := Layout_Object_Tree.Find (Container => LOT, Item => Parent);
      cc : Natural := Natural (Layout_Object_Tree.Child_Count (parent_cursor));
   begin 
      -- Check if the parent has any children, if not, add the new widget as the first child
      if cc = 0 then 
         handler.LOT.Append_Child (parent_cursor, This); 
      else
         -- Otherwise we want to add the new widget based on its priority 
         declare 
            current_sibling : Layout_Object_Tree.Cursor := Layout_Object_Tree.First_Child (parent_cursor);
            last_sibling : Layout_Object_Tree.Cursor := Layout_Object_Tree.Last_Child (parent_cursor);

            Inserted  : Boolean   := False;
            Increment : Natural   := 1;
         begin 
            while Increment < cc and Inserted = False loop
               if This.priority > Layout_Object_Tree.Element (current_sibling).priority then
                  handler.LOT.Insert_Child (Parent => parent_cursor, 
                                     Before => current_sibling,
                                     New_Item => This
                                     ); 
                  Inserted := True; 

               else
                  current_sibling := Layout_Object_Tree.Next_Sibling (current_sibling);
                  Increment := Increment + 1;
               end if;
            end loop;
            if Inserted = False then
               if This.priority > Layout_Object_Tree.Element (last_sibling).priority then
                  handler.LOT.Insert_Child(Parent => parent_cursor, 
                                     Before => last_sibling,
                                     New_Item => This
                                     );
                  Inserted := True;
               else
                  handler.LOT.Append_Child (parent_cursor, This); 
               end if;
            end if;
         end; -- end declare 
      end if; -- end if cc = 0
   end add_to_LOT;

  
  
  procedure Calculate_Dimensions (Node_Cursor : Layout_Object_Tree.Cursor) is 
   total_height : Natural := 0; 
   max_width : Natural := 0; 
   Child_Cursor : Layout_Object_Tree.Cursor; 
   Child_Widget : Widget.Any_Acc;
   Node_Widget : Widget.Any_Acc;
  begin 

   if not LOT.Has_Element (Node_Cursor) then 
      return; 
   end if;

   Node_Widget := LOT.Element (Node_Cursor);

   if Node_Widget = null then 
      return; 
   end if;

   if LOT.Child_Count(Node_Cursor > 0) then 
      Child_Cursor := LOT.First_Child (Node_Cursor);
      while LOT.Has_Element (Child_Cusor) loop 
         Calculate_Layout (Child_Cursor);


         Child_Widget := LOT.Element (Child_Cursor);

         if Child_Widget /= null then 
            total_height := total_height + Child_Widget.Height;

            if Child_Widget.Width > max_width then 
               max_width := Child_Widget.Width; 
            end if;  
         else 
            -- Handle the case where the child widget is null
            -- This could be a placeholder or an empty space in the layout
            return;
         end if;  

         Child_Cursor := LOT.Next_Sibling (Child_Cursor);
      end loop; -- end while LOT.Has_Element (Child_Cursor)
   else 
      declare 
         Leaf_Instance : Widget.Instance := Widget.Instance(Node_Widget.all);
      begin 
         if Leaf_Instance /= null then 
            total_height := Leaf_Instance.Height; 
            max_width := Leaf_Instance.Width; 
         end if; 
      end; -- end declare
      return; 
   end if; 

   -- Now update the current node's dimensions
   declare 
      Node_Instance_Ref : access Widget.Instance := Widget.Instance(Node_Widget.all);
   begin 
      if Node_Instance_Ref /= null then 
         Node_Instance_Ref.all.Set_Width (max_width); 
         Node_Instance_Ref.all.Set_Height (total_height); 
      end if;
   end; -- end declare
  end Calculate_Dimensions; -- end procedure Calculate_Dimensions
  
  
  
  
  
  
   procedure display_nodes is
      w : Widget.Any_Acc; 
      current_id : Ada.Strings.Unbounded.Unbounded_String;
   begin 
      for c in LOT.Iterate loop 
         w := Layout_Object_Tree.Element (c);
         Widget.render(w.all); 
      end loop; -- end for c in LOT.Iterate
   end display_nodes; -- end procedure display_node
   
   
   procedure Free_Buffer is new Ada.Unchecked_Deallocation(
      Object => Buffer_Array,
      Name => Buffer_Access);


   procedure Initialize_Buffer is
   begin
      if buffer /= null then
         Free_Buffer(buffer);
      end if;
      
      if main_widget /= null then
         buffer := new Buffer_Array(0..main_widget.Height-1, 0..main_widget.Width-1);
      else
         -- Default size if no main widget exists
         buffer := new Buffer_Array(0..24, 0..79);
      end if;
   end Initialize_Buffer;

   procedure Set_Background_Color (Color: Widget.color_t) is
      R_Str : String := Ada.Strings.Fixed.Trim (Color.red'Image, Ada.Strings.Left); 
      G_Str : String := Ada.Strings.Fixed.Trim (Color.green'Image, Ada.Strings.Left);
      B_Str : String := Ada.Strings.Fixed.Trim (Color.blue'Image, Ada.Strings.Left); 
   begin 
      Ada.Text_IO.Put (ASCII.ESC & "[48;2;" & R_Str & ";" & G_Str & ";" & B_Str & "m");
   end Set_Background_Color; -- end procedure Set_Background_Color

   procedure Reset_Color is
   begin
      Ada.Text_IO.Put(ASCII.ESC & "[0m");
   end Reset_Color;

   procedure Clear_Terminal is
   begin
      Ada.Text_IO.Put(ASCII.ESC & "[2J" & ASCII.ESC & "[H");
      Ada.Text_IO.Flush;
   end Clear_Terminal;
   
   -- Navigation functions implementation
   function Is_Navigable(W : Widget.Any_Acc) return Boolean is
      use type Ada.Tags.Tag;
   begin
      -- Check if the widget is a button
      return W.all'Tag = Widget.Button.Instance'Tag;
   end Is_Navigable;
   
   procedure Initialize_Focus is
      procedure Find_First_Navigable(Position : Layout_Object_Tree.Cursor) is
         Child : Layout_Object_Tree.Cursor := Layout_Object_Tree.First_Child(Position);
      begin
         while Layout_Object_Tree.Has_Element(Child) loop
            declare
               Widget_Ptr : Widget.Any_Acc := Layout_Object_Tree.Element(Child);
            begin
               if Is_Navigable(Widget_Ptr) then
                  focused_widget := Widget_Ptr;
                  
                  -- Set focus on the button
                  if Widget_Ptr.all'Tag = Widget.Button.Instance'Tag then
                     Widget.Button.Set_Focus(Widget.Button.Instance(Widget_Ptr.all), True);
                  end if;
                  
                  return;
               end if;
            end;
            
            -- Recursively check children
            Find_First_Navigable(Child);
            
            -- If we found a navigable widget in the children, exit
            if focused_widget /= null then
               return;
            end if;
            
            -- Move to next sibling
            Child := Layout_Object_Tree.Next_Sibling(Child);
         end loop;
      end Find_First_Navigable;
   begin
      -- Start from the root
      Find_First_Navigable(LOT_Root);
   end Initialize_Focus;
   
   function Find_Next_Navigable_Widget(Current : Widget.Any_Acc; Direction : String) return Widget.Any_Acc is
      -- This is a simplified implementation - in a real application, you would
      -- want to consider the spatial layout of widgets
      
      procedure Find_Next(Position : Layout_Object_Tree.Cursor; Found_Current : in out Boolean; Result : in out Widget.Any_Acc) is
         Child : Layout_Object_Tree.Cursor := Layout_Object_Tree.First_Child(Position);
      begin
         while Layout_Object_Tree.Has_Element(Child) and Result = null loop
            declare
               Widget_Ptr : Widget.Any_Acc := Layout_Object_Tree.Element(Child);
            begin
               if not Found_Current and Widget_Ptr = Current then
                  Found_Current := True;
               elsif Found_Current and Is_Navigable(Widget_Ptr) then
                  Result := Widget_Ptr;
                  return;
               end if;
            end;
            
            -- Recursively check children
            Find_Next(Child, Found_Current, Result);
            
            -- Move to next sibling
            Child := Layout_Object_Tree.Next_Sibling(Child);
         end loop;
      end Find_Next;
      
      Found : Boolean := False;
      Result : Widget.Any_Acc := null;
   begin
      Find_Next(LOT_Root, Found, Result);
      
      -- If we didn't find a next widget, wrap around to the first
      if Result = null and Found then
         Found := False;
         Find_Next(LOT_Root, Found, Result);
      end if;
      
      return Result;
   end Find_Next_Navigable_Widget;
   
   procedure Navigate_Up is
   begin
      if focused_widget /= null then
         -- Clear focus on current widget
         if focused_widget.all'Tag = Widget.Button.Instance'Tag then
            Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), False);
         end if;
         
         -- Find next widget
         declare
            Next_Widget : Widget.Any_Acc := Find_Next_Navigable_Widget(focused_widget, "up");
         begin
            if Next_Widget /= null then
               focused_widget := Next_Widget;
               
               -- Set focus on new widget
               if focused_widget.all'Tag = Widget.Button.Instance'Tag then
                  Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), True);
               end if;
            end if;
         end;
      else
         Initialize_Focus;
      end if;
      
      update_render := True;
   end Navigate_Up;
   
   procedure Navigate_Down is
   begin
      if focused_widget /= null then
         -- Clear focus on current widget
         if focused_widget.all'Tag = Widget.Button.Instance'Tag then
            Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), False);
         end if;
         
         -- Find next widget
         declare
            Next_Widget : Widget.Any_Acc := Find_Next_Navigable_Widget(focused_widget, "down");
         begin
            if Next_Widget /= null then
               focused_widget := Next_Widget;
               
               -- Set focus on new widget
               if focused_widget.all'Tag = Widget.Button.Instance'Tag then
                  Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), True);
               end if;
            end if;
         end;
      else
         Initialize_Focus;
      end if;
      
      update_render := True;
   end Navigate_Down;
   
   procedure Navigate_Left is
   begin
      if focused_widget /= null then
         -- Clear focus on current widget
         if focused_widget.all'Tag = Widget.Button.Instance'Tag then
            Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), False);
         end if;
         
         -- Find next widget
         declare
            Next_Widget : Widget.Any_Acc := Find_Next_Navigable_Widget(focused_widget, "left");
         begin
            if Next_Widget /= null then
               focused_widget := Next_Widget;
               
               -- Set focus on new widget
               if focused_widget.all'Tag = Widget.Button.Instance'Tag then
                  Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), True);
               end if;
            end if;
         end;
      else
         Initialize_Focus;
      end if;
      
      update_render := True;
   end Navigate_Left;
   
   procedure Navigate_Right is
   begin
      if focused_widget /= null then
         -- Clear focus on current widget
         if focused_widget.all'Tag = Widget.Button.Instance'Tag then
            Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), False);
         end if;
         
         -- Find next widget
         declare
            Next_Widget : Widget.Any_Acc := Find_Next_Navigable_Widget(focused_widget, "right");
         begin
            if Next_Widget /= null then
               focused_widget := Next_Widget;
               
               -- Set focus on new widget
               if focused_widget.all'Tag = Widget.Button.Instance'Tag then
                  Widget.Button.Set_Focus(Widget.Button.Instance(focused_widget.all), True);
               end if;
            end if;
         end;
      else
         Initialize_Focus;
      end if;
      
      update_render := True;
   end Navigate_Right;
   
   procedure Select_Focused_Widget is
   begin
      if focused_widget /= null then
         if focused_widget.all'Tag = Widget.Button.Instance'Tag then
            Widget.Button.Press(Widget.Button.Instance(focused_widget.all));
            Widget.Button.Release(Widget.Button.Instance(focused_widget.all));
         end if;
      end if;
      
      update_render := True;
   end Select_Focused_Widget;
   
   procedure Handle_Key_Press(Key : Character) is
   begin
      case Key is
         when ASCII.ESC =>
            -- Handle escape key (special case for arrow keys)
            declare
               Next_Char : Character;
               Arrow_Char : Character;
            begin
               -- Read the next two characters to determine the arrow key
               Get_Immediate(Next_Char);
               if Next_Char = '[' then
                  Get_Immediate(Arrow_Char);
                  case Arrow_Char is
                     when 'A' => Navigate_Up;    -- Up arrow
                     when 'B' => Navigate_Down;  -- Down arrow
                     when 'C' => Navigate_Right; -- Right arrow
                     when 'D' => Navigate_Left;  -- Left arrow
                     when others => null;
                  end case;
               end if;
            end;
         when ' ' | ASCII.CR =>
            -- Space or Enter key - select the focused widget
            Select_Focused_Widget;
         when others =>
            null;
      end case;
   end Handle_Key_Press;

begin
   -- Reminder :: Come back and initialize the main_widget here
   main_widget := 
      new Widget.Instance' (
         Controlled with id => +"main", 
         others => <>
      ); 
   
   LOT.Append_Child (Parent => LOT_Root, New_Item => main_widget); 
   LOT_Root := Layout_Object_Tree.First_Child (LOT_Root); 

end Handler;
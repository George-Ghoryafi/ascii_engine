with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; 
with Ada.Finalization; use Ada.Finalization;
with Ada.Numerics; 
with Ada.Numerics.Elementary_Functions; 
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags; use Ada.Tags;
with Ada.Calendar;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Widget; use Widget; 
with Widget.Button;

 package body Handler is 
   -- Add this variable to track the last render time
   Last_Render_Time : Ada.Calendar.Time := Ada.Calendar.Time_Of(1901, 1, 1); -- Initialize to a past time
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

   procedure display_nodes is
      w : Widget.Any_Acc; 
      current_id : Ada.Strings.Unbounded.Unbounded_String;
   begin 
      for c in LOT.Iterate loop 
         w := Layout_Object_Tree.Element (c);
         Widget.render(w.all); 
      end loop; -- end for c in LOT.Iterate
   end display_nodes; -- end procedure display_node

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
      -- Improved implementation that better handles navigation between widgets
      All_Navigable : array(1..100) of Widget.Any_Acc := (others => null);
      Count : Natural := 0;
      Current_Index : Natural := 0;
      
      procedure Collect_Navigable(Position : Layout_Object_Tree.Cursor) is
         Child : Layout_Object_Tree.Cursor := Layout_Object_Tree.First_Child(Position);
      begin
         while Layout_Object_Tree.Has_Element(Child) loop
            declare
               Widget_Ptr : Widget.Any_Acc := Layout_Object_Tree.Element(Child);
            begin
               if Is_Navigable(Widget_Ptr) then
                  Count := Count + 1;
                  All_Navigable(Count) := Widget_Ptr;
                  
                  if Widget_Ptr = Current then
                     Current_Index := Count;
                  end if;
               end if;
            end;
            
            -- Recursively check children
            Collect_Navigable(Child);
            
            -- Move to next sibling
            Child := Layout_Object_Tree.Next_Sibling(Child);
         end loop;
      end Collect_Navigable;
      
      Next_Index : Natural;
   begin
      -- First collect all navigable widgets
      Collect_Navigable(LOT_Root);
      
      -- If no navigable widgets or current widget not found, return null
      if Count = 0 or Current_Index = 0 then
         return null;
      end if;
      
      -- Determine next index based on direction
      if Direction = "up" or Direction = "left" then
         if Current_Index > 1 then
            Next_Index := Current_Index - 1;
         else
            Next_Index := Count; -- Wrap around to last
         end if;
      else -- "down" or "right"
         if Current_Index < Count then
            Next_Index := Current_Index + 1;
         else
            Next_Index := 1; -- Wrap around to first
         end if;
      end if;
      
      return All_Navigable(Next_Index);
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
            -- Use Press instead of Set_Pressed
            Widget.Button.Press(Widget.Button.Instance(focused_widget.all));
            
            -- Force a render to show the pressed state
            update_render := True;
            Update_Display(Show_Instructions => True);
            
            -- Small delay to show the pressed state
            delay 0.2;
            
            -- Release the button
            Widget.Button.Release(Widget.Button.Instance(focused_widget.all));
            
            -- Force another render to show the released state
            update_render := True;
         end if;
      end if;
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

   procedure Update_Display(Show_Instructions : Boolean := True) is
      use type Ada.Calendar.Time;
      Current_Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      Min_Render_Interval : constant Duration := 0.1; -- 100ms between renders
   begin
      -- Only render if update_render is true and enough time has passed since last render
      if update_render and then (Last_Render_Time = Ada.Calendar.Time_Of(1901, 1, 1) or else 
                                Current_Time - Last_Render_Time >= Min_Render_Interval) then
         -- Clear screen completely - using more explicit Windows console commands
         Ada.Text_IO.Put(ESC & "[2J");        -- Clear entire screen
         Ada.Text_IO.Put(ESC & "[3J");        -- Clear scrollback buffer
         Ada.Text_IO.Put(ESC & "[1;1H");      -- Move cursor to top-left corner (1,1)
         Ada.Text_IO.New_Line;                -- Ensure we start with a clean line
         
         -- Redraw UI
         display_nodes;
         
         -- Show navigation instructions if requested
         if Show_Instructions then
            Ada.Text_IO.Put_Line("Navigation Instructions:");
            Ada.Text_IO.Put_Line("- Use arrow keys or WASD to navigate between buttons");
            Ada.Text_IO.Put_Line("- Press Enter or Space to activate a button");
            Ada.Text_IO.Put_Line("- Press 'q' to quit");
         end if;
         
         update_render := False;
         Last_Render_Time := Current_Time;
      end if;
   end Update_Display;

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
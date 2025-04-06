with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; 
with Ada.Finalization; use Ada.Finalization;
with Ada.Numerics; 
with Ada.Numerics.Elementary_Functions; 
with Ada.Text_IO; use Ada.Text_IO;

with Widget; use Widget; 


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
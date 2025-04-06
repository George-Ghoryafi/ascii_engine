with Ada.Containers.Multiway_Trees;
with Ada.Strings.Unbounded;

with Widget; use Widget;

package Handler is 
   pragma Elaborate_Body; 

   -- Every node (cursor) in the tree will be of type Widget.Any_Acc
   package Layout_Object_Tree is new Ada.Containers.Multiway_Trees (Widget.Any_Acc); 

   LOT : Layout_Object_Tree.Tree;
   LOT_Root : Layout_Object_Tree.Cursor := LOT.Root; 
   main_widget : Widget.Any_Acc; 

   type event_states is (idle, press, drag, resize); -- Probably just implementing idle and press

   event_state : event_states := idle;
   update_render : Boolean := False; 
   start_dist : Float; 
   start_w, start_h : Natural;
   event_target : Widget.Any_Acc;


   procedure add_to_LOT (This : Widget.Any_Acc; Parent : Widget.Any_Acc);
   procedure display_nodes;
   procedure Set_Background_Color (Color: Widget.color_t);
   procedure Reset_Color; 

end Handler;
with handler; 
with Widget; use Widget;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
      begin 
         -- Render the text in the widget
         Put_Line (To_String(This.text)); 
   end render;


begin 
   Put_Line ("Widget.Text package body loaded");
      
end Widget.Text;
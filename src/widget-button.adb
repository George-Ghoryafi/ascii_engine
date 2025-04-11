with Handler;
with Widget; use Widget;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Widget.Button is
   function Create
   (
      id : String;
      parent : Widget.Any_Acc;
      priority : Natural := 0;
      min_height, min_width : Natural := 0;
      max_height, max_width : Natural := Natural'Last;
      bgd_color : Widget.color_t := Widget.default_color;
      label : String;
      on_click : Button_Callback := null
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
                           label => +label,
                           is_focused => False,
                           is_pressed => False,
                           on_click => on_click,
                           others => <>);
      
      Handler.add_to_LOT(This, parent);
      return This;
   end Create;
   
   procedure render (This : in out Instance) is
      procedure horizontal_border (width : Natural) is
         wide : Natural := width;
         line : String := (1 .. wide => '-');
      begin
         -- Draw the top border of the button
         if This.is_focused then
            Put_Line ("*" & line & "*");
         else
            Put_Line ("+" & line & "+");
         end if;
      end horizontal_border;
      
      label_text : String := To_String(This.label);
      button_width : Natural := Natural'Max(This.width, label_text'Length + 4);
      padding : Natural;
   begin
      -- Set background color
      if This.is_focused then
         Handler.Set_Background_Color((red => 100, green => 100, blue => 255, alpha => 255));
      elsif This.is_pressed then
         Handler.Set_Background_Color((red => 150, green => 150, blue => 150, alpha => 255));
      elsif This.bgd_color /= Widget.default_color then
         Handler.Set_Background_Color(This.bgd_color);
      end if;
      
      -- Adjust width if needed
      This.width := button_width;
      padding := (button_width - label_text'Length) / 2;
      
      -- Draw button
      horizontal_border(button_width);
      
      -- Draw button content
      Put("|");
      Put((1 .. padding => ' '));
      Put(label_text);
      Put((1 .. button_width - label_text'Length - padding => ' '));
      Put_Line("|");
      
      horizontal_border(button_width);
      
      Handler.Reset_Color;
   end render;
   
   procedure Set_Focus (This : in out Instance; focused : Boolean) is
   begin
      This.is_focused := focused;
   end Set_Focus;
   
   procedure Press (This : in out Instance) is
   begin
      This.is_pressed := True;
      -- Remove the callback execution from here
   end Press;
   
   procedure Release (This : in out Instance) is
   begin
      This.is_pressed := False;
   end Release;
   
   procedure Execute_Action (This : in out Instance) is
   begin
      if This.on_click /= null then
         This.on_click.all;
      end if;
   end Execute_Action;
   
begin
   Put_Line("Widget.Button package body loaded");
end Widget.Button;
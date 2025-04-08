with Ada.Strings.Unbounded;

package Widget.Button is
   subtype Parent is Widget.Instance;
   
   -- Define a named access type for the callback
   type Button_Callback is access procedure;
   
   type Instance is new Parent with record
      label : Ada.Strings.Unbounded.Unbounded_String;
      is_focused : Boolean := False;
      is_pressed : Boolean := False;
      on_click : Button_Callback := null;
   end record;
   
   subtype Class is Instance'Class;
   type Acc is access all Instance;
   type Any_Acc is access all Class;
   
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
   ) return Widget.Any_Acc;
   
   overriding
   procedure render (This : in out Instance);
   
   procedure Set_Focus (This : in out Instance; focused : Boolean);
   procedure Press (This : in out Instance);
   procedure Release (This : in out Instance);
   
private
   subtype Dispatch is Instance'Class;
end Widget.Button;
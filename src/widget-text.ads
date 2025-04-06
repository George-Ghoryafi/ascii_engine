with Ada.Strings.Unbounded; 


package Widget.Text is 
   subtype Parent is Widget.Instance; 
   type text_overflow is (default, truncate, wrap); 

   type Instance is new Parent 
   with record
      text : Ada.Strings.Unbounded.Unbounded_String; 
      text_color : Widget.color_t := Widget.white;
      overflow : text_overflow := default;
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
      text : string; 
      text_color : Widget.color_t := Widget.white;
      overflow : text_overflow := default
   ) return Widget.Any_Acc; 

   overriding 
   procedure render (This : in out Instance);
   --  overriding
   --  function Set_Event_Override_Width (This: in out Instance; Parent : Widget.Any_Acc; new_width: Natural) return Natural;
   --  overriding
   --  function Set_Event_Override_Height (This: in out Instance; Parent : Widget.Any_Acc; new_height: Natural) return Natural;

private 
   subtype Dispatch is Instance'Class;

end Widget.Text;
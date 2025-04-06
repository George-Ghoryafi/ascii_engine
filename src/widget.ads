with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Containers.Multiway_Trees; 

package Widget is 

   package SU renames Ada.Strings.Unbounded;

   function "+" (Source : in String) return SU.Unbounded_String is 
      (SU.To_Unbounded_String (Source));

   function "+" (Source : in SU.Unbounded_String) return String is
    (SU.To_String (source));


    type percent_t is new Float range 0.0 .. 1.0;

   type dir_t          is (left_right, right_left, top_bottom, bottom_top, front_back, back_front); 
   type align_t        is (left, right, top, bottom, center, stretch, none);
   type buoy_t         is (space_between, space_around, space_evenly, space_nothing); 
   type behaviour_t    is (content, portion, pixel, percent, max); 

   type color_t is record 
      red   : Natural range 0 .. 255; 
      green : Natural range 0 .. 255; 
      blue  : Natural range 0 .. 255; 
      alpha : Natural range 0 .. 255; 
   end record;

   -- Default color for the widget, fully transparent 
   default_color : constant color_t := (red => 0, green => 0, blue => 0, alpha => 0);
   white : constant color_t := (red => 255, green => 255, blue => 255, alpha => 255);


   -- Setting the requirements for the behaviour of the widget 
   type expand_t (behaviour : behaviour_t := max) is record 
      case behaviour is 
         when portion =>
            portion : Positive; 
         when pixel =>
            pixel   : Positive; 
         when percent =>
            percent : percent_t; 
         when others => 
            null;
      end case;
   end record;


   type Instance is new Ada.Finalization.Controlled with 
   record
      id : SU.Unbounded_String; 
      x, y : Natural := 0; 
      width, height : Natural := 0;
      min_height, min_width : Natural := 0; 
      max_height, max_width : Natural := Natural'Last;
      priority : Natural := 0; 
      bgd_color : color_t := default_color;
   end record;
   -- Allowing us to use Class to either contain an object of type Class, or of any derived type of Instance, including Instance itself
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
      bgd_color : color_t := default_color
   ) return Widget.Any_Acc; 

   procedure render (This : in out Instance);
   procedure Set_Width (This : in out Instance; calculated_width : Natural);
   procedure Set_Height (This : in out Instance; calculated_height : Natural);
   function Get_Id (This : in out Instance) return SU.Unbounded_String;
   procedure Click (This : in out Instance); 
   function Is_Clickable (This : in out Instance) return Boolean; 
   function Set_Event_Override_Height (This: in out Instance; Parent : Widget.Any_Acc; new_height: Natural) return Natural;
   function Set_Event_Override_Width (This: in out Instance; Parent : Widget.Any_Acc; new_width: Natural) return Natural;
   


end Widget;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Container is
   
   type Color_T is (Red, Green, Blue);
   
   -- Container type declaration
   type Container_T is tagged private;
   
   -- Constructor
   function Create (Text : String := ""; Background: Color_T := Red; Width : Natural := 5) return Container_T;
   
   -- Accessor functions
   function Get_Text (Self : Container_T) return String;

   -- Child Management 
   procedure Add_Child (Self : in out Container_T; Child : Container_T); 
   --  function Get_Child (Self : Container_T; Index : Positive) return Container_T;
   --  function Get_Number_Of_Children (Self : Container_T) return Natural;
   
   -- Modifier procedures
   procedure Set_Text (Self : in out Container_T; Text : String);
   
   -- Rendering function
   function Render (Self : Container_T) return String;
   
private
   -- Need to clarify - Why do I have to do this?
   type Container_Access is access Container_T;


   package Container_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Container_Access);
   
   type Container_T is tagged record
      Text       : Unbounded_String;
      Background : Color_T; 
      Children   : Container_Vectors.Vector; -- Vector containing the container's Children
      Width      : Natural; 
   end record;
   
end Container;
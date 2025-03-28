with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Container is
   
   type Color_T is (Red, Green, Blue);
   
   -- Container type declaration
   type Container_T is tagged private;
   type Container_Access is private;
   
   -- Constructor
   function Create (Text : String := ""; Background: Color_T := Red; Width : Natural := 5) return Container_T;
   function Create (Text : String := ""; Background: Color_T := Red; Width : Natural := 5; Parent : Container_T) return Container_T;
   function Create (Text : String := ""; Background: Color_T := Red; Width : Natural := 5; Parent : Container_T; First_Child : Container_T) return Container_T;
   
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
   type Container_Access is access all Container_T;


   type Container_T is tagged record
      Text             : Unbounded_String;
      Width            : Natural;
      Background       : Color_T; 
      Parent           : Container_Access := null; -- Pointer to the parent container
      First_Child      : Container_Access := null; -- Vector containing the container's Children
      Next_Sibling     : Container_Access := null; -- Pointer to the next sibling container
      Previous_Sibling : Container_Access := null; -- Pointer to the previous sibling container
   end record;
   
end Container;
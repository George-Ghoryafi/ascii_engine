with Ada.Unchecked_Deallocation;

package Container is
   
   -- Color type definition
   type Color_Type is record
      Red   : Natural range 0..255 := 0;
      Green : Natural range 0..255 := 0;
      Blue  : Natural range 0..255 := 0;
   end record;
   
   -- Default color (transparent/terminal default)
   Default_Color : constant Color_Type := (0, 0, 0);
   
   -- Access type for strings
   type String_Access is access String;
   
   -- Array of strings and its access type
   type String_Array is array (Positive range <>) of String_Access;
   type String_Array_Access is access String_Array;
   
   -- Container type definition
   type Container_Type is record
      Width      : Natural;
      Height     : Natural;
      Content    : String_Array_Access;
      Background : Color_Type := Default_Color;
   end record;
   
   -- Create a new container with the given text and optional background color
   function Create (Text : String; Background_Color : Color_Type := Default_Color) 
                   return Container_Type;
   
   -- Convert hex color code to Color_Type
   function Hex_To_Color (Hex : String) return Color_Type;
   
   -- Set terminal background color
   procedure Set_Background_Color (Color : Color_Type);
   
   -- Reset terminal colors to default
   procedure Reset_Colors;
   
   -- Render the container to the terminal
   procedure Render (Container : Container_Type);
   
   -- Free the memory used by the container
   procedure Free (Container : in out Container_Type);
   
end Container;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Container is

   -- Constructor implementation
   function Create (Text : String := ""; Background : Color_T := Red) return Container_T is
   begin
      return Container_T'(
         Text => To_Unbounded_String(Text), 
         Background => Background, 
         Children => Container_Vectors.Empty_Vector
      );
   end Create;
   
   -- Accessor implementations
   function Get_Text (Self : Container_T) return String is
   begin
      return To_String(Self.Text);
   end Get_Text;
   
   -- Modifier implementations
   procedure Set_Text (Self : in out Container_T; Text : String) is
   begin
      Self.Text := To_Unbounded_String(Text);
   end Set_Text;

   -- Handling Children 
   procedure Add_Child (Self : in out Container_T; Child : Container_T) is
      Child_Access : constant Container_Access := new Container_T'(Child);
   begin
      Self.Children.Append(Child_Access);
   end Add_Child;

   --  function Get_Child( Self : Container_T; Index : Positive ) return Container_T is
   --  begin
   --     return Self.Children.Element(Index);
   --  end Get_Child;

   --  function Get_Children_Count (Self : Container_T) return Natural is
   --  begin
   --     return Self.Children.Length;
   --  end Get_Children_Count;

   
   
   -- Rendering implementation
   function Render (Self : Container_T) return String is
      Text : constant String := To_String(Self.Text);
      Width : constant Natural := Text'Length + 4; -- Add padding
      Top_Bottom_Border : constant String := "+" & (Width-2) * "-" & "+";
      Empty_Line : constant String := "|" & (Width-2) * " " & "|";
      Text_Line : constant String := "| " & Text & " |";
      Result : Unbounded_String;
   begin
      -- Build the box with the text centered
      Append(Result, Top_Bottom_Border & ASCII.LF);
      Append(Result, Empty_Line & ASCII.LF);

      -- Conditionally render either text if it exists, or the children of the container
      -- We need to render the last possible child first, in order to determine the size of the container
      if Text /= "" then
         Append(Result, Text_Line & ASCII.LF);
      else
         for I in reverse 1..Integer(Self.Children.Length) loop
            Append(Result, Self.Children.Element(I).all.Render & ASCII.LF);
         end loop;  
      end if;    
      
      Append(Result, Empty_Line & ASCII.LF);
      Append(Result, Top_Bottom_Border);
      
      return To_String(Result);
   end Render;





end Container;
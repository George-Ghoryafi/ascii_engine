package Text_Box is
   type Text_Box_Type is tagged private;
   
   procedure Create (Box : in out Text_Box_Type;
                    Width : Positive;
                    Height : Positive;
                    Title : String := "");
                    
   procedure Display (Box : Text_Box_Type);
   
   procedure Set_Content (Box : in out Text_Box_Type;
                         Content : String);
                         
private
   Max_Content_Length : constant := 1000;
   
   type Text_Box_Type is tagged record
      Width : Positive := 20;
      Height : Positive := 3;
      Title : String(1..50) := (others => ' ');
      Title_Length : Natural := 0;
      Content : String(1..Max_Content_Length) := (others => ' ');
      Content_Length : Natural := 0;
   end record;
end Text_Box;
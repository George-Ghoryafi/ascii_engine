with Ada.Text_IO; use Ada.Text_IO;

package body Button_Callbacks is
   procedure Button1_Click is
   begin
      Put_Line("Button 1 clicked!");
   end Button1_Click;

   procedure Button2_Click is
   begin
      Put_Line("Button 2 clicked!");
   end Button2_Click;

   procedure Button3_Click is
   begin
      Put_Line("Button 3 clicked!");
   end Button3_Click;
end Button_Callbacks;
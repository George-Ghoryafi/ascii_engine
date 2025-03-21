with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body Ascii_Engine is

   protected type Input_Data is
      procedure Set_Input (Cmd : Input_T);
      function Get_Input return Input_T;
   private
      Input : Input_T;
   end Input_Data;

   protected body Input_Data is
      procedure Set_Input (Cmd : Input_T) is
      begin
         Input := Cmd;
      end Set_Input;
      function Get_Input return Input_T is
         (Input);
   end Input_Data;

   ID : Input_Data;

   function Get_Input return Input_T is
   begin
      return ID.Get_Input;
   end Get_Input;

   task body Input is
      N1 : Character;
      N2 : Character;
      N3 : Character;
      Avail  : Boolean;
   begin
      loop
         select
            accept Stop do
               null;
            end Stop;
            exit;
         else
            Get_Immediate (N1, Avail);
            if Avail then
            if N1 = ASCII.ESC then
               Get_Immediate (N2, Avail);
               Get_Immediate (N3, Avail);
               if N2 = Character'Val (91) then
                  case N3 is
                     when Character'Val (65) => ID.Set_Input (Input_T'(ASCII.NUL, Up));
                     when Character'Val (66) => ID.Set_Input (Input_T'(ASCII.NUL, Down));
                     when Character'Val (67) => ID.Set_Input (Input_T'(ASCII.NUL, Right));
                     when Character'Val (68) => ID.Set_Input (Input_T'(ASCII.NUL, Left));
                     when others => null;
                  end case;
               end if;
            elsif N1 /= ASCII.NUL then
               ID.Set_Input (Input_T'(N1, Undefined));
            end if;
            end if;
         end select;
      end loop;
   end Input;

end Ascii_Engine;

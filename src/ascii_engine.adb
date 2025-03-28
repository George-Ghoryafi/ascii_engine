with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body Ascii_Engine is

   Cmd : Command_T := Up;
   pragma Atomic (Cmd);

   task body Input_T is
      Run : Boolean := False;
      C : Character;
   begin
      loop
         select
            accept Stop do
               Put_Line ("Input Task Stoped");
            end Stop;
            exit;
         or
            accept Start do
               Put_Line ("Input Task Started");
               Run := True;
            end Start;
         else
            if Run then
               Get_Immediate (C);
               if C = ASCII.Esc then
                     Get_Immediate (C);
                  if C = '[' then
                     Get_Immediate (C);
                     case C is
                        when 'A'    => Cmd := Up;
                        when 'B'    => Cmd := Down;
                        when 'C'    => Cmd := Right;
                        when 'D'    => Cmd := Left;
                        when others => null;
                     end case;
                  end if;
               else
                  case C is
                     when 'w'    => Cmd := Up;
                     when 's'    => Cmd := Down;
                     when 'd'    => Cmd := Right;
                     when 'a'    => Cmd := Left;
                     when 'q'    => 
                        Run := False;
                        Cmd := Quit;
                     when others => null;
                  end case;
               end if;
            end if;
         end select;
      end loop;
   end Input_T;

   function Command return Command_T is
   begin
      return Cmd;
   end;

end Ascii_Engine;

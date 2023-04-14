open DMPF;;
open Aef;;
open Aef_proprietes;;
open Aef_operations;;

let () = print_endline "Hello, World!"


(* reconnait L(exemple1) = a(a^n)b 
   deterministe
   incomplet*)
   let exemple1:aef = { alphabet = ['a'; 'b'];
   etats_Q = [1;2;3];
   transitions = [ (1, 'a', 2);
                   (2, 'a', 2);
                   (2, 'b', 3)];
   initial = 1;
   etats_F = [3]}

(* L(exemple2)=a*{a,b}*b*
detreministe
complet *)
let exemple2 = { alphabet = ['a'; 'b'];
   etats_Q = [4;5];
   transitions = [(4,'a',4);
                  (4,'b',5);
                  (5,'a',4);
                  (5,'b',5)];
   initial = 4;
   etats_F = [5];}

let union_ex1_ex2 = {alphabet = ['a';'b'];
        etats_Q = [0;1;2;3;4;5];
        transitions = [(4,'a',4);
                       (4,'b',5);
                       (5,'a',4);
                       (5,'b',5);
                       (1, 'a', 2);
                       (2, 'a', 2);
                       (2, 'b', 3);
                       (0, 'a', 4);
                       (0, 'b', 5);
                       (0, 'a', 2)];
        etats_F = [5;3];
        initial=0};;

let concat_ex1_ex2 = {alphabet = ['a'; 'b'];
   initial = 1;
   etats_F = [5];
   etats_Q = [1;2;3;4;5];
   transitions =[(4,'a',4);
   (4,'b',5);
   (5,'a',4);
   (5,'b',5);
   (1, 'a', 2);
   (2, 'a', 2);
   (2, 'b', 3);
   (2, 'a', 4);
   (2, 'b', 5)]};;

afficher (union exemple1 exemple2);;
print_endline "";;
afficher union_ex1_ex2;;
print_endline "";;
afficher concat_ex1_ex2;;
print_endline "";;
print_endline (string_of_bool (est_complet exemple1))

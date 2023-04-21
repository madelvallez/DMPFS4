open Aef;;


(* reconnait L(exemple1) = a(a^n)b 
   deterministe
   incomplet*)
let exemple1:aef = { alphabet = ['a'; 'b'];
   etats_Q = [1;2;3];
   transitions = [ (1, 'a', 2);
                   (2, 'a', 2);
                   (2, 'b', 3)];
   initial = 1;
   etats_F = [3]};;

(* L(exemple2)={a,b}*b
detreministe
complet *)
let exemple2 = { alphabet = ['a'; 'b'];
   etats_Q = [4;5];
   transitions = [(4,'a',4);
                  (4,'b',5);
                  (5,'a',4);
                  (5,'b',5)];
   initial = 4;
   etats_F = [5];};;

(* reconnait L(exemple3)= (a*.(bb)* )* b 
   deterministe incomplet*)
let exemple3 : aef = {
  alphabet = ['a';'b'];
  etats_Q= [1;2];
  transitions = [(1, 'a', 1); (1, 'b', 2); (2, 'b', 1)]; 
  initial = 1; 
  etats_F= [2]};;

  (* reconnait le langage vide
     deterministe , complet*)
let exemple4 :aef = {
  alphabet = ['a';'b'];
  etats_Q = [4];
  etats_F = [];
  transitions = [(4,'a', 4)];
  initial = 4; 
};;

(*reconnait le langage vide 
   deterministe, incomplet*)
let exemple5 : aef = {
  alphabet = ['a'; 'b'];
  etats_Q = [1; 2];
  etats_F = [2];
  transitions = [];
  initial = 1;
};;

(*reconnait L(exemple6) = a(a²)* 
   deterministe incomplet*)
let exemple6 :aef = {
   alphabet = ['a';'b'];
   etats_Q = [0; 4];
   etats_F = [4];
   transitions = [(0,'a',4); (4,'a',0)];
   initial = 0;
   };;

(*reconnait L(exemple7) = (ab)* 
   deterministe incomplet*)
let exemple7:aef = {
   alphabet = ['a';'b'];
   etats_Q = [6;7];
   etats_F = [6];
   initial = 6;
   transitions = [(6,'a',7); (7,'b',6)];

};;

let exemple8:aef = {
   alphabet = ['a';'b'];
   etats_Q = [6;7];
   etats_F = [6];
   initial = 6;
   transitions = [(6,'a',7); (6,'a',7); (7,'b',6)];

};;

let exemple9:aef = {
   alphabet = ['a';'b'];
   etats_Q = [6;7];
   etats_F = [6;2];
   initial = 6;
   transitions = [(6,'a',7); (6,'a',7); (7,'b',6)];

};;

let affiche (a:aef) : unit =
      let rec affiche_alpha l = match l with
        |[] -> ""
        |[c] -> String.make 1 c
        |h::q -> (String.make 1 h )^", "^(affiche_alpha q)
      in
      let rec affiche_etats l = match l with
        |[] -> ""
        |[e] -> string_of_int e
        | h::q -> (string_of_int h )^", "^(affiche_etats q)
      in 
      let rec affiche_transitions l = match l with
        |[] -> ""
        |(e,c,e')::q -> "\t ("^(string_of_int e)^", "^(String.make 1 c)^", "^(string_of_int e')^") \n " ^(affiche_transitions q)
      in print_endline (( "alphabet : " ^ (affiche_alpha a.alphabet) ^ "\n"^
      "etats_Q : " ^ (affiche_etats a.etats_Q) ^"\n"^
      "transitions : \n" ^ (affiche_transitions a.transitions)^"\n"^
      "initial : "^ (string_of_int a.initial) ^"\n" ^
      "etats_F : "^(affiche_etats a.etats_F))^"\n");;
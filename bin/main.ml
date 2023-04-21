open DMPF ;;
open Aef_proprietes;;
open Aef_operations;;
open Aef_exemples;;
open Aef_lecture;;



print_endline (string_of_bool (est_deterministe exemple5));;

print_endline "union exemple1 exemple2";;
afficher (union exemple1 exemple2);;
print_endline "union exemple1 exemple4 :" ;;
afficher (union exemple1 exemple4);;
print_endline "union exemple1 exemple7(test etat initial et terminal) :" ;;
afficher (union exemple1 exemple7);;
print_endline "union exemple1 exemple6 (test de premier_entier_abs) :" ;;
afficher (union exemple1 exemple6);;
print_endline "concat exemple1 exemple7 (test cas etat initial et final) :" ;;
afficher (concat exemple1 exemple7);;
print_endline "concat exemple1 exemple4 :" ;;
afficher (concat exemple1 exemple4);;


print_endline("exemple1 = {
  alphabet = ['a'; 'b'];
  etats_Q = [1;2;3];
  transitions = [(1, 'a', 2);(2, 'a', 2);(2, 'b', 3)];
  initial = 1;
  etats_F = [3]
  }");;

print_endline("est_correct: " ^ string_of_bool(est_correct exemple1)) ;;
print_endline("est_complet: " ^ string_of_bool(est_complet exemple1));;
print_endline("on va donc le completer");;
affiche(completer exemple1);;
print_endline"on va voir si le nouveau aef est bien completé";;
print_endline("est_complet (nouveau aef): " ^ string_of_bool(est_complet (completer exemple1)));;

print_endline("langage_vide: " ^ string_of_bool(langage_vide exemple1));;
print_endline("est_deterministe: " ^ string_of_bool(est_deterministe exemple1));;
print_endline" ";;


print_endline ("Le mot aab est il accepté dans l'aef exemple1: " ^ string_of_bool(accepter_mot ['a';'a';'b'] exemple1));;
print_endline ("Le mot aa est il accepté dans l'aef exemple1: " ^ string_of_bool(accepter_mot ['a';'a'] exemple1));;


print_endline" ";;
print_endline"exemple5 : aef = {
  alphabet = ['a'; 'b'];
  etats_Q = [1; 2];
  etats_F = [2];
  transitions = [];
  initial = 1;
}";;
print_endline("langage_vide: " ^ string_of_bool(langage_vide exemple5));;

print_endline"";;
  
print_endline"exemple8:aef = {
  alphabet = ['a';'b'];
  etats_Q = [6;7];
  etats_F = [6];
  initial = 6;
  transitions = [(6,'a',7); (6,'a',7); (7,'b',6)];

}";;
print_endline("est_deterministe: " ^ string_of_bool(est_deterministe exemple8));;

print_endline"";;

print_endline"exemple9:aef = {
  alphabet = ['a';'b'];
  etats_Q = [6;7];
  etats_F = [6;2];
  initial = 6;
  transitions = [(6,'a',7); (6,'a',7); (7,'b',6)];

}";;
print_endline("est_correct: " ^ string_of_bool(est_correct exemple9));;
  
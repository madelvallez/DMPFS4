open DMPF;;
open Aef_proprietes;;
open Aef_operations;;
open Aef_exemples;;


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
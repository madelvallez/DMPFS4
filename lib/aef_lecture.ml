open Aef;;

(*2. Reconnaissance d’un mot par un AEF déterministe*)

(*ex 7 lecture_car*)
(* renvoie l'etat qui correspond à la transition d'un caractère*)


let lecture_car : etat -> char -> transition list -> etat option = fun q x les_transitions->
  let transitions = List.filter (fun (q', x', _) -> q' = q && x' = x) les_transitions in
  match transitions with 
  | [(_, _, q'')] -> Some q'' (* si etat et le char en paramètre correspondent à une transition dans les_transitions alors, renvoie l'état cible de cette transition*)
  | _ -> None;;


(*ex 8 lecture_mot*)
(* renvoie l'etat qui correspond à la transition d'une list de caractères*)

let rec lecture_mot : etat -> char list -> transition list -> etat option = fun etat mot les_transitions ->
  match mot with
  | [] -> Some etat (* si le mot est vide retourne l'état en paramètre *)
  | h::t ->
      match (lecture_car etat h les_transitions) with
      | Some etat' -> lecture_mot etat' t les_transitions (* si il y a une transition pour le char actuel, on continue avec le char suivant *)
      | None -> None;; (* si il n'y a pas de transition pour le char actuel, le mot n'est pas reconnu par l'aef *)


(*ex 9 accepter_mot*) 
(* renvoie true si le dernier état cible du mot est dans la liste des états finaux sinon false*)
let accepter_mot : char list -> aef -> bool = fun mot aef ->
  match (lecture_mot aef.initial mot aef.transitions) with
  | Some etat -> List.mem etat aef.etats_F (* si l'état final du mot est dans la liste etat_F alors renvoie true *)
  | None -> false ;; (* si le mot n'est pas reconnu par l'aef, retourne false *)
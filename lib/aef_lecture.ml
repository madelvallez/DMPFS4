open Aef;;

(*2. Reconnaissance d’un mot par un AEF déterministe*)

(*ex 7 lecture_car*)
(** La fonction lecture_car prend en entrée un état q, 
    un caractère x et une liste de transitions les_transitions,
    et retourne l'état cible correspondant à la transition qui part de l'état q et qui est étiquetée par le caractère x
    @author Gilles, Marwan 
    @param q état
    @param x caractère
    @param les_transitions liste de transitions 
    @return a type option 
*)
let lecture_car : etat -> char -> transition list -> etat option = fun q x les_transitions->
  let transitions = List.filter (fun (q', x', _) -> q' = q && x' = x) les_transitions in
  match transitions with 
  | [(_, _, q'')] -> Some q'' (* si etat et le char en paramètre correspondent à une transition dans les_transitions alors, renvoie l'état cible de cette transition*)
  | _ -> None;;


(*ex 8 lecture_mot*)
(**  la fonction lecture_car vérifier s'il existe une transition pour le premier caractère du mot à lire et 
     renvoie l'etat qui correspond à la transition d'une list de caractères
    @author Gilles, Marwan 
    @param etat état
    @param mot char list
    @param les_transitions liste de transitions 
    @return a type option 
*)
let rec lecture_mot : etat -> char list -> transition list -> etat option = fun etat mot les_transitions ->
  match mot with
  | [] -> Some etat (* si le mot est vide retourne l'état en paramètre *)
  | h::t ->
      match (lecture_car etat h les_transitions) with
      | Some etat' -> lecture_mot etat' t les_transitions (* si il y a une transition pour le char actuel, on continue avec le char suivant *)
      | None -> None;; (* si il n'y a pas de transition pour le char actuel, le mot n'est pas reconnu par l'aef *)


(*ex 9 accepter_mot*) 
(**  La fonction accepter_mot utilise la fonction lecture_mot pour parcourir le mot dans l'AEF 
     et renvoie true si le dernier état atteint après avoir parcouru le mot appartient à l'ensemble des états finaux de l'AEF
    @author Gilles , Marwan 
    @param mot char list
    @param aef AEF
    @return a boolean
*)
let accepter_mot : char list -> aef -> bool = fun mot aef ->
  match (lecture_mot aef.initial mot aef.transitions) with
  | Some etat -> List.mem etat aef.etats_F (* si l'état final du mot est dans la liste etat_F alors renvoie true *)
  | None -> false ;; (* si le mot n'est pas reconnu par l'aef, retourne false *)
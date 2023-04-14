open Aef;;

(*1. Quelques fonctions sur les AEF*)
(*1.2 est_correct*) 
(* renvoie true si l'aef en argument est correct *)
let est_correct = fun a ->
  let etats_non_utilises = List.filter (fun q -> not (List.exists (fun (q', _, q'') -> q = q' || q = q'') a.transitions)) a.etats_Q in
  a.etats_Q <> [] &&
  a.transitions <> [] &&
  List.for_all (fun etat -> List.mem etat a.etats_Q) a.etats_F &&
  List.for_all (fun (e, _, e') -> List.mem e a.etats_Q && List.mem e' a.etats_Q) a.transitions &&
  etats_non_utilises = []


(*1.3 est_complet*) 
(* renvoie true si l'aef en argument est complet *)

let est_complet : aef -> bool = fun a ->
  if not (est_correct a) then false else
    let pour_tout_char = fun q ->
      List.for_all (fun x -> List.exists (fun (q', c, _) -> q = q' && c = x) a.transitions) a.alphabet
    in
    let pour_tout_etat = fun etats ->
      List.for_all (fun q -> pour_tout_char q) etats
    in
    pour_tout_etat a.etats_Q


(*1.4 completer*) 
(* renvoie le completé de l'aef en argument *)

let completer (a:aef) : aef =
  if est_complet a then a
  else 
    let rec nouveau_etat n =
      if List.mem n a.etats_Q then nouveau_etat (n+1)
      else n
    in
    let etat_puits = nouveau_etat 1 in
    let transitions_manquantes = 
      List.concat (List.map (fun q -> 
          List.map (fun c -> (q,c,etat_puits)) 
            (List.filter (fun x -> 
                 not (List.exists (fun (q',y,_) -> y = x && q = q') a.transitions)) 
                a.alphabet)) 
          a.etats_Q)
    in
    let transitions_puits = 
      List.map (fun c -> (etat_puits, c, etat_puits)) a.alphabet
    in
    {
      alphabet = a.alphabet;
      etats_Q = etat_puits :: a.etats_Q;
      transitions = a.transitions @ transitions_manquantes @ transitions_puits;
      initial = a.initial;
      etats_F = a.etats_F
    }

let langage_vide = fun a ->
      a.transitions = [] || a.etats_F = []

let est_deterministe = fun a -> 
  let transition_pour_etat_et_char q x =
    List.filter (fun (q', x', _) -> q' = q && x' = x) a.transitions
  in
  List.for_all (fun q ->
      List.for_all (fun x ->
          List.length (transition_pour_etat_et_char q x) <= 1
        ) a.alphabet
    ) a.etats_Q
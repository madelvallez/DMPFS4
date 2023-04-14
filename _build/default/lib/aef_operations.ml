open Aef ;;

(*UNION________________________________________________________________*)
(* renvoie le premier entier naturel qui n'est dans aucune des listes données *)
let  premier_entier_abs l1 l2 = 
    let rec aux_premier_entier_abs l n = 
        if List.mem n l then
        (aux_premier_entier_abs l (n+1))
        else n
    in aux_premier_entier_abs (l1@l2) 0 ;;
        
 
(*renvoie la partie de delta supplementaire pour union (issu de la presnece de q0) *)
let delta_union_q0 (a1:aef) (a2:aef) (q0:etat) : transition list =
  let q1 = a1.initial in
  let q2 = a2.initial in
  let rec extraire l qa = match l with
    |[] -> []
    |(qi, x, p)::t when(qi=qa)-> (q0, x, p)::(extraire t qa)
    | _::t -> extraire t qa
  in (extraire a1.transitions q1)@(extraire a2.transitions q2)

exception PasMemeAlphabet

let ont_meme_alphabet (a1:aef) (a2:aef) : bool=
  let egalite_listes l1 l2 = 
    let rec liste_incluse l1 l2 = match l1 with
      |[] -> true
      |t::q -> (List.mem t l2)&&(liste_incluse q l2)
    in (liste_incluse l1 l2)&&(liste_incluse l2 l1)
  in egalite_listes a1.alphabet a2.alphabet



(*fonction qui prend deuc AEF et renvoie l'AEF qui reconnait l'union des deux langagues *)
let union (a1:aef) (a2:aef) :aef = 
  if not (ont_meme_alphabet a1 a2) then raise PasMemeAlphabet else  
  let q0 = premier_entier_abs a1.etats_Q a2.etats_Q in
  let etats_union = q0::(a1.etats_Q @ a2.etats_Q) in
  let finaux_union = 
    if (List.mem a1.initial a1.etats_F)||(List.mem a2.initial a2.etats_F) 
    then q0::(a1.etats_F @ a2.etats_F) 
    else (a1.etats_F @ a2.etats_F) in
  let delta_union = (delta_union_q0 a1 a2 q0)@(a1.transitions)@(a2.transitions) in
  {alphabet = a1.alphabet; 
  etats_Q = etats_union; 
  transitions = delta_union; 
  initial = q0;
  etats_F = finaux_union}


(*CONCAT___________________________________________________________________________________*)
let liaisons (a1:aef) (a2:aef) : transition list = 
  let rec attacher_aux_finaux x  q lf = match lf with 
    |[] -> []
    |hf::qf -> (hf, x, q)::(attacher_aux_finaux x q qf)
  in
  let rec traiter_suivant_initial q2 lq = match lq with
    |[] -> []
    |(qi,x,q)::qq when(qi=q2)-> (attacher_aux_finaux x q a1.etats_F)@(traiter_suivant_initial q2 qq)
    |_::qq -> traiter_suivant_initial q2 qq
  in traiter_suivant_initial a2.initial a2.transitions


let concat (a1: aef) (a2: aef) = 
  if not(ont_meme_alphabet a1 a2) then raise  PasMemeAlphabet else 
    (*on verifie que les automates ont le même alphabet*)
  let etats_QQ = a1.etats_Q @ a2.etats_Q in
  let etats_FF = if List.mem a2.initial a2.etats_F 
                  then (a1.etats_F)@(a2.etats_F) 
                  else a2.etats_F in
  let delta_concat = (liaisons a1 a2)@((a1.transitions)@(a2.transitions)) in 
  let initial_concat = a1.initial in
  {alphabet = a1.alphabet;
  etats_Q = etats_QQ;
  etats_F = etats_FF;
  transitions = delta_concat;
  initial = initial_concat}

(*AFFICHER___________________________________________________________________________________*)
(*3.12 affichage*)
let afficher (a:aef) =
  let rec produire_texte_transition lt = match lt with
    |[] -> " "
    |(q,x,p)::qt -> (string_of_int q)^ " -> ("^(String.make 1 x)^") "^(string_of_int p)^"\n"^(produire_texte_transition qt)
  in print_endline (produire_texte_transition a.transitions);;


(*ITERE__________________________________________________________________________*)

let complement_delta_debut (a:aef) (q0:etat) = 
  let q1 = a.initial in
  let rec traitement lt = match lt with 
    |[] -> []
    |(q,x,p)::qt when(q==q1) -> (q0,x,p)::(traitement qt)
    |_::qt -> traitement qt
  in traitement a.transitions

let complement_delta_fin (a:aef) =
  let rec creation_transitions y q lf = match lf with 
    |[] -> []
    |hf::qf -> (hf, y, q)::(creation_transitions y q qf)
  in
  let rec trouver_q lt = match lt with 
    |[] -> []
    |(qi, y ,q)::qt when(qi=a.initial)->(creation_transitions y q a.etats_F)@(trouver_q qt)
    |_::qt -> trouver_q qt
  in trouver_q a.transitions 

let itere (a:aef) = 
  let q0 = premier_entier_abs a.etats_Q [] in
  let etats_QQ = q0::(a.etats_Q) in 
  let etats_FF = q0::(a.etats_F) in 
  let delta =(complement_delta_debut a q0)@((complement_delta_fin a)@(a.transitions)) in
  {alphabet = a.alphabet;
  initial = q0;
  etats_Q = etats_QQ;
  etats_F = etats_FF;
  transitions = delta}


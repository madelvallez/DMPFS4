(* type etat = int
type transition = etat * char * etat
type aef = {
  alphabet : char list;
  etats_Q : etat list;
  transitions : transition list;
  initial : etat;
  etats_F : etat list;
}*)
exception PasMemeAlphabet
val union : Aef.aef -> Aef.aef -> Aef.aef
val concat : Aef.aef -> Aef.aef -> Aef.aef
val afficher : Aef.aef -> unit
val itere : Aef.aef -> Aef.aef
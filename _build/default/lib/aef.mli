type etat = int
type transition = etat * char * etat
type aef = {
  alphabet : char list;
  etats_Q : etat list;
  transitions : transition list;
  initial : etat;
  etats_F : etat list;
}
val lecture_car : Aef.etat -> char -> Aef.transition list -> Aef.etat option
val lecture_mot :
  Aef.etat -> char list -> Aef.transition list -> Aef.etat option
val accepter_mot : char list -> Aef.aef -> bool
# Manuel d'utilisation

Le main se trouve dans `DMPFS4/bin/main.ml` 

L'execution se fait avec `dune exec DMPF`

Les fonctions du sujet sont définies dans `DMPFS4/lib` dans 4 fichiers: 
- `aef.ml` pour la définition du type (question 1)
- `aef_propriete.ml` pour les fonctions de la partie 1
- `aef_lecture.ml` pour les fonctions de la partie 2
- `aef_operations.ml` pour les fonctions de la partie 3 

Des aef exemples sont données dans `DMPFS4/lib/aef_exemples.ml`. Ce sont ceux utilisés dans `main.ml`. Une illustration de ces derniers est disponible dans `DFPFS4/schema_exemples_aef`.

# Repartition du travail
| Question | Marine Delvallez | Gilles Gachiniard | Marwan Laaguid |
|:---- | :---: | :---: | :---: |
| 1: `aef` |x|x|x| 
| 2: `est_correct` | |x| | 
| 3: `est_complet` | |x| | 
| 4: `completer` | |x|x| 
| 5: `langage_vide` | |x| | 
| 6: `est_deterministe` | |x| | 
| 7: `lecture_car` | |x|x| 
| 8: `lecture_mot` | |x|x| 
| 9: `accepter_mot` | |x|x| 
| 10: `union` |x| | | 
| 11: `concat` |x| | | 
| 12: `afficher` |x| | | 
| 13: `itere` |x| | | 




# Descriptions des fonctions
Voir la documentation à `DMPFS4/lib/doc/index.html`
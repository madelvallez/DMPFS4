#!/bin/sh

# 1. compiler la base :
ocamlc -c aef.mli aef.ml

# 2. compiler le reste :
ocamlc aef.cmo aef_lecture.mli aef_lecture.ml
ocamlc aef.cmo aef_proprietes.mli aef_proprietes.ml
ocamlc aef.cmo aef_operations.mli aef_operations.ml

# 3. recuperer la doc :
ocamldoc -html aef.ml aef_lecture.ml aef_proprietes.ml aef_operations.ml

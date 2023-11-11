open Formule_Syllogisme
(* open Formule_Log_Prop *)

module Predicate_set = Set.Make (String)
(** Module des ensembles de prédicats représentés par des chaines de caractères *)

(** Type des remplissages possibles d'un diagramme de Venn *)
type fill = Vide | NonVide

module Diag = Map.Make (Predicate_set)
(** Module des Maps dont les clés sont des ensembles de chaines de caractères *)

type diagramme = fill Diag.t
(** Type des diagrammes de Venn *)

(** string_of_diag d : conversion d'un diagramme d en une chaine de caractères *)
let string_of_diag (_ : diagramme) : string = failwith "à faire"

(** diag_from_formule alpha f : construit le diagramme de Venn associé à la formule f sur
      les prédicats issus de f ou de alpha *)
let diag_from_formule (_ : string list) (_ : formule_syllogisme) :
    diagramme list =
  failwith "à faire"

(** conj_diag d1 d2 : Calcule la combinaison/conjonction de deux diagrammes, renvoyant None si incompatibilité *)
let conj_diag (_ : diagramme) (_ : diagramme) : diagramme option =
  failwith "à faire"

(** est_compatible_diag_diag dp dc : teste si le diagramme dp d'une prémisse est compatible
    avec le diagramme dc d'une conclusion *)
let est_compatible_diag_diag (_ : diagramme) (_ : diagramme) : bool =
  failwith "à faire"

(** est_compatible_diag_list dp dcs : teste si un diagramme dp d'une prémisse est compatible
    avec un des diagrammes de la liste dcs,
    diagrammes issus d'une conclusion *)
let est_compatible_diag_list (_ : diagramme) (_ : diagramme list) : bool =
  failwith "à faire"

(** est_compatible_list_list dps dcs : teste si chacun des diagrammes de dps, diagrammes issus de prémisses,
    est compatible avec au moins un des diagrammes de dcs, diagrammes issus d'une conclusion *)
let est_compatible_list_list (_ : diagramme list) (_ : diagramme list) : bool =
  failwith "à faire"

(** est_compatible_premisses_conc ps c : teste si une liste de prémisses ps est compatible avec une conclusion c *)
let est_compatible_premisses_conc (_ : formule_syllogisme list)
    (_ : formule_syllogisme) : bool =
  failwith "à faire"

(** temoin_incompatibilite_premisses_conc_opt ps c : renvoie un diagramme de la combinaison des 
    prémisses ps incompatible avec la conclusion c s'il existe, None sinon *)
let temoin_incompatibilite_premisses_conc_opt (_ : formule_syllogisme list)
    (_ : formule_syllogisme) : diagramme option =
  failwith "à faire"

(** temoins_incompatibilite_premisses_conc ps c : renvoie les diagrammes de la combinaison
    des prémisses ps incompatibles avec la conclusion c *)
let temoins_incompatibilite_premisses_conc (_ : formule_syllogisme list)
    (_ : formule_syllogisme) : diagramme list =
  failwith "à faire"

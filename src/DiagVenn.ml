open Formule_Syllogisme
open Formule_Log_Prop

module Predicate_set = Set.Make (String)
(** Module des ensembles de prédicats représentés par des chaines de caractères 
    *)

type predicats = Predicate_set.t
(** Type d'ensemble des prédicats représentés par des chaines de caractères *)

(** Type des remplissages possibles d'un diagramme de Venn *)
type fill = Vide | NonVide

module Diag = Map.Make (Predicate_set)
(** Module des Maps dont les clés sont des ensembles de chaines de caractères *)

type diagramme = fill Diag.t
(** Type des diagrammes de Venn *)

(** string_of_fill f : conversion d'un remplisage f sous forme de chaine de 
    caractères *)
let string_of_fill : fill -> string = function
  | Vide -> "Vide"
  | NonVide -> "Non Vide"

(** string_of_predicate k : conversion d'une liste de prédicats k en un ensemble
    représenter sous forme de chaine de caractère. *)
let string_of_predicate : predicats -> string = function
  | p when Predicate_set.cardinal p = 0 -> "∅"
  | p ->
      String.concat ""
        [ "{"; String.concat ", " (Predicate_set.elements p); "}" ]

(** list_string_of_diag d : conversion d'un diagramme d en une liste de chaine 
  de caractères *)
let list_string_of_diag (d : diagramme) : string list =
  Diag.fold
    (fun k v a ->
      String.concat "" [ string_of_predicate k; " -> "; string_of_fill v ] :: a)
    d []

(** string_of_diag d : conversion d'un diagramme d en une chaine de caractères*)
let string_of_diag (d : diagramme) : string =
  String.concat "\n" (list_string_of_diag d)

(** diag_from_formule alpha f : construit le diagramme de Venn associé à la 
      formule f sur les prédicats issus de f ou de alpha *)
let diag_from_formule (l : string list) : formule_syllogisme -> diagramme list =
  function
  | PourTout f' ->
      [
        List.fold_left
          (fun acc v ->
            match v with
            | a, false -> Diag.add (Predicate_set.of_list a) Vide acc
            | _ -> acc)
          Diag.empty (table_verite l f');
      ]
  | IlExiste f' ->
      List.fold_left
        (fun acc v ->
          match v with
          | _, false -> acc
          | a, true -> Diag.singleton (Predicate_set.of_list a) NonVide :: acc)
        [] (table_verite l f')

(** conj_diag d1 d2 : Calcule la combinaison/conjonction de deux diagrammes, 
    renvoyant None si incompatibilité *)
let conj_diag (d1 : diagramme) (d2 : diagramme) : diagramme option =
  let acc =
    Diag.fold
      (fun k v acc ->
        match Diag.find_opt k d2 with
        | _ when Option.is_none acc -> None
        | Some v' when v' <> v -> None
        | None -> Some (Diag.add k v (Option.get acc))
        | Some _ -> Some (Diag.add k v (Option.get acc)))
      d1 (Some Diag.empty)
  in
  match acc with
  | None -> None
  | _ -> Diag.fold (fun k v acc -> Some (Diag.add k v (Option.get acc))) d2 acc

(** conj_diag_to_diag_list d1 d2l : Fais la conjonction de d1 avec tous ceux 
    présent dans d2l. Si la conjonction entre d1 et l'un de ces diagrammes n'est 
    pas possible alors elle n'appaît pas dans la list final des combinaison. *)
let conj_diag_to_diag_list (d1 : diagramme) (d2l : diagramme list) :
    diagramme list =
  List.fold_left
    (fun acc d ->
      match conj_diag d1 d with None -> acc | Some d' -> acc @ [ d' ])
    [] d2l

(** conj_diag_list d1l d2l : Fais la conjonction des diagramme de d1l avec ceux 
    de d2l. Si une telle conjonction n'est pas possible pour deux d'entre eux 
    alors, cette conjonction n'apparaitra pas dans le résultat final. *)
let conj_diag_list (d1l : diagramme list) (d2l : diagramme list) :
    diagramme list =
  List.fold_left (fun acc d -> acc @ conj_diag_to_diag_list d d2l) [] d1l

(** est_compatible_diag_diag dp dc : teste si le diagramme dp d'une prémisse est 
    compatible avec le diagramme dc d'une conclusion *)
let est_compatible_diag_diag (d1 : diagramme) (d2 : diagramme) : bool =
  match
    Diag.fold
      (fun k v acc ->
        match (v, acc) with
        | _, (false, _) -> (false, -1)
        | Vide, _ -> (
            match Diag.find_opt k d1 with Some Vide -> acc | _ -> (false, -1))
        | NonVide, (_, 1) -> acc
        | NonVide, _ -> (
            match Diag.find_opt k d1 with
            | Some NonVide -> (true, 1)
            | _ -> (true, -1)))
      d2 (true, 0)
  with
  | true, n when n >= 0 -> true
  | _ -> false

(** est_compatible_diag_list dp dcs : teste si un diagramme dp d'une prémisse 
    est compatibl avec un des diagrammes de la liste dcs, diagrammes issus d'une 
    conclusion *)
let est_compatible_diag_list (d1 : diagramme) (dl : diagramme list) : bool =
  List.exists (est_compatible_diag_diag d1) dl

(** est_compatible_list_list dps dcs : teste si chacun des diagrammes de dps, 
    diagrammes issus de prémisses, est compatible avec au moins un des 
    diagrammes de dcs, diagrammes issus d'une conclusion *)
let est_compatible_list_list (d1 : diagramme list) (d2 : diagramme list) : bool
    =
  List.for_all (Fun.flip est_compatible_diag_list d2) d1

(** est_compatible_premisses_conc ps c : teste si une liste de prémisses ps est 
    compatible avec une conclusion c *)
let est_compatible_premisses_conc (fl : formule_syllogisme list)
    (c : formule_syllogisme) : bool =
  match fl with
  | [] -> false
  | _ ->
    let a =
      List.sort_uniq compare
        (List.concat (List.map atomes_of_formule fl) @ atomes_of_formule c)
    in
    let comb_dl = List.fold_left 
      (fun acc f -> conj_diag_list acc (diag_from_formule a f)) 
      (diag_from_formule a (List.hd fl)) 
      (List.tl fl) in
    est_compatible_list_list comb_dl (diag_from_formule a c)

(** temoin_incompatibilite_premisses_conc_opt ps c : renvoie un diagramme de la 
    combinaison des prémisses ps incompatible avec la conclusion c s'il existe, 
    None sinon *)
let temoin_incompatibilite_premisses_conc_opt (fl : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme option =
    let a =
      List.sort_uniq compare
        (List.concat (List.map atomes_of_formule fl) @ atomes_of_formule c)
    in
  let dc = diag_from_formule a c in
  let comb_dl = List.fold_left 
    (fun acc f -> conj_diag_list acc (diag_from_formule a f)) 
    (diag_from_formule a (List.hd fl)) 
    (List.tl fl) in
  List.find_opt (fun comb -> not (est_compatible_diag_list comb dc)) comb_dl

(** temoins_incompatibilite_premisses_conc_diag combl cdl : renvoie les 
    diagrammes de la combinaison combl incompatibles avec la conclusion cdl *)
let temoins_incompatibilite_premisses_conc_diag (combl : diagramme list)
    (cdl : diagramme list) : diagramme list =
  List.fold_left
    (fun acc comb ->
      match est_compatible_diag_list comb cdl with
      | true -> acc
      | false -> acc @ [ comb ])
    [] combl

(** temoins_incompatibilite_premisses_conc ps c : renvoie les diagrammes de la 
    combinaison des prémisses ps incompatibles avec la conclusion c *)
let temoins_incompatibilite_premisses_conc (fl : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme list =
  let a =
    List.sort_uniq compare
      (List.concat (List.map atomes_of_formule fl) @ atomes_of_formule c)
  in
  let dc = diag_from_formule a c in
  let dpl = List.map (diag_from_formule a) fl in
  let comb_dl =
    List.fold_left
      (fun acc dl -> conj_diag_list acc dl)
      (List.hd dpl) (List.tl dpl)
  in
  temoins_incompatibilite_premisses_conc_diag comb_dl dc

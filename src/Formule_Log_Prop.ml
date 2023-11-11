(** Type des formules de la logique propositionnelle *)
type formule_log_prop =
  | Bot
  | Top
  | Atome of string
  | Et of formule_log_prop * formule_log_prop
  | Ou of formule_log_prop * formule_log_prop
  | Imp of formule_log_prop * formule_log_prop
  | Non of formule_log_prop
  | Xor of formule_log_prop * formule_log_prop
  | Equiv of formule_log_prop * formule_log_prop
  | Aucun of formule_log_prop list
  | Tous of formule_log_prop list
  | AuMoins of int * formule_log_prop list
  | AuPlus of int * formule_log_prop list

type interpretation = string -> bool
(** Type des interprétations. *)

(** eval i f : renvoie l'évaluation d'une formule f en fonction d'une 
    interprétation i. *)
let rec eval (i : interpretation) : formule_log_prop -> bool = function
  | Bot -> false
  | Top -> true
  | Atome a -> i a
  | Et (f, g) -> eval i f && eval i g
  | Ou (f, g) -> eval i f || eval i g
  | Imp (f, g) -> (not (eval i f)) || eval i g
  | Non f -> not (eval i f)
  | Xor (f, g) ->
      let a = eval i f and b = eval i g in
      (a || b) && not (a && b)
  | Equiv (f, g) -> eval i f = eval i g
  | Aucun l -> List.for_all (function f -> not (eval i f)) l
  | Tous l -> List.for_all (eval i) l
  | AuMoins (n, l) ->
      let rec existn n l =
        match (n, l) with
        | 0, _ -> true
        | _, [] -> false
        | n, h :: t -> (
            match eval i h with true -> existn (n - 1) t | false -> existn n t)
      in
      existn n l
  | AuPlus (n, l) ->
      let rec atmost n l =
        match (n, l) with
        | 0, l -> not (List.for_all (eval i) l)
        | _, [] -> true
        | n, h :: t -> (
            match eval i h with true -> atmost (n - 1) t | false -> atmost n t)
      in
      atmost n l

(** interpretation_of_list s : Transforme une liste de string s en une 
    interprétation (valant vrai pour les string contenu dans la liste de string, 
    et faux sinon). *)
let interpretation_of_list : string list -> interpretation = Fun.flip List.mem

(** all_sublists l : Calcule la liste de toutes les listes de Booléens d'une 
    longueur donnée. *)
let rec all_sublists : 'a list -> 'a list list = function
  | [] -> [ [] ]
  | h :: t ->
      let a = all_sublists t in
      List.map (function a -> h :: a) a @ a

(** atomes f : Calcule la liste (triée et sans doublon) des atomes d'une 
    formule de la logique propositionnelle.*)
let atomes (f : formule_log_prop) : string list =
  let rec aux acc = function
    | Atome s -> s :: acc
    | Non f -> aux acc f
    | Ou (f, g) | Et (f, g) | Imp (f, g) | Xor (f, g) | Equiv (f, g) ->
        aux [] f @ aux [] g @ acc
    | Aucun l | Tous l | AuMoins (_, l) | AuPlus (_, l) ->
        List.fold_left (fun a f -> aux [] f @ a) [] l
    | Bot | Top -> acc
  in
  List.sort_uniq compare (aux [] f)

(** verity_lines l f : renvoie toute les conbinaison possible des atomes 
    issue de f ou de l sans aucun doublon. *)
let verity_lines (l : string list) (f : formule_log_prop) : string list list =
  all_sublists (List.sort_uniq compare (l @ atomes f))

(** table_verite alpha f : renvoie la table de vérité de f sur les atomes issus 
    de f ou de alpha. *)
let table_verite (l : string list) (f : formule_log_prop) :
    (string list * bool) list =
  List.map
    (function l' -> (l', eval (interpretation_of_list l') f))
    (verity_lines l f)

(** string_of_formule_log_prop_var s f : conversion d'une formule f en chaîne de 
    caractères, en les représentant comme des prédicats unaires appliqués sur 
    des occurrences de la variable s. *)
let rec string_of_formule_log_prop_var (s : string) : formule_log_prop -> string
    = function
  | Bot -> "⊥"
  | Top -> "T"
  | Atome a -> String.concat "" [ a; "("; s; ")" ]
  | Et (f, g) ->
      String.concat ""
        [
          "(";
          string_of_formule_log_prop_var s f;
          " ∧ ";
          string_of_formule_log_prop_var s g;
          ")";
        ]
  | Ou (f, g) ->
      String.concat ""
        [
          "(";
          string_of_formule_log_prop_var s f;
          " v ";
          string_of_formule_log_prop_var s g;
          ")";
        ]
  | Imp (f, g) ->
      String.concat ""
        [
          "(";
          string_of_formule_log_prop_var s f;
          " → ";
          string_of_formule_log_prop_var s g;
          ")";
        ]
  | Non f -> String.concat "" [ "¬"; string_of_formule_log_prop_var s f ]
  | Xor (f, g) ->
      String.concat ""
        [
          "(";
          string_of_formule_log_prop_var s f;
          " ⊕ ";
          string_of_formule_log_prop_var s g;
          ")";
        ]
  | Equiv (f, g) ->
      String.concat ""
        [
          "(";
          string_of_formule_log_prop_var s f;
          " <=> ";
          string_of_formule_log_prop_var s g;
          ")";
        ]
  | Aucun l ->
      String.concat ""
        [
          "Aucun(";
          String.concat ", " (List.map (string_of_formule_log_prop_var s) l);
          ")";
          "";
        ]
  | Tous l ->
      String.concat ""
        [
          "Tous(";
          String.concat ", " (List.map (string_of_formule_log_prop_var s) l);
          ")";
        ]
  | AuMoins (n, l) ->
      String.concat ""
        [
          "AuMoins";
          string_of_int n;
          "(";
          String.concat ", " (List.map (string_of_formule_log_prop_var s) l);
          ")";
        ]
  | AuPlus (n, l) ->
      String.concat ""
        [
          "AuPlus";
          string_of_int n;
          "(";
          String.concat ", " (List.map (string_of_formule_log_prop_var s) l);
          ")";
        ]

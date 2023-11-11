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

(** table_verite alpha f : renvoie la table de vérité de f sur les atomes issus de f ou de alpha *)
let table_verite (_ : string list) (_ : formule_log_prop) :
    (string list * bool) list =
  failwith "à faire"

(** string_of_formule_log_prop_var s f : conversion d'une formule f en chaîne de caractères,
    en les représentant comme des prédicats unaires appliqués sur des 
    occurrences de la variable s. *)
let string_of_formule_log_prop_var (_ : string) (_ : formule_log_prop) : string
    =
  failwith "à faire"

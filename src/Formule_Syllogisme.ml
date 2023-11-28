open Formule_Log_Prop

(** Type des formules utilisées pour les syllogismes *)
type formule_syllogisme =
  | PourTout of formule_log_prop
  | IlExiste of formule_log_prop

(** string_of_formule f : conversion d'une formule f en chaîne de caractères,
    en considérant des prédicats unaires appliqués sur des 
    occurrences de la variable s. *)
let string_of_formule : formule_syllogisme -> string = function
  | PourTout f ->
      String.concat "" [ "∀x "; string_of_formule_log_prop_var "x" f ]
  | IlExiste f ->
      String.concat "" [ "∃x "; string_of_formule_log_prop_var "x" f ]

(** atomes_of_formule f : renvoie la liste (triée et sans doublon) des atomes 
    présent dans une formulle de syllogisme.*)
let atomes_of_formule : formule_syllogisme -> string list = function
  | PourTout f | IlExiste f -> atomes f

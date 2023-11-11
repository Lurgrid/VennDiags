open Formule_Log_Prop

(** Type des formules utilisées pour les syllogismes *)
type formule_syllogisme =
  | PourTout of formule_log_prop
  | IlExiste of formule_log_prop

(** string_of_formule f : conversion d'une formule f en chaîne de caractères,
    en considérant des prédicats unaires appliqués sur des 
    occurrences de la variable s. *)
let string_of_formule (_ : formule_syllogisme) : string = failwith "à faire"

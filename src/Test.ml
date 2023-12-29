open Formule_Syllogisme
open Formule_Log_Prop
open DiagVenn

let a = Atome "a"
let b = Atome "b"
let c = Atome "c"
let d = Atome "d"

(* Définition d'opérateur liée à la création de syllogismes, pour rendre leurs
   création plus concise. *)
let ( |+| ) a b = Ou (a, b)
let ( |*| ) a b = Et (a, b)
let ( |->| ) a b = Imp (a, b)
let ( |++| ) a b = Xor (a, b)
let ( |<->| ) a b = Equiv (a, b)
let ( |&| ) n a = AuMoins (n, a)
let ( |!&| ) n a = AuPlus (n, a)
let p1 = PourTout (Imp (Ou (a, b), c))
let p2 = PourTout (Imp (c, Ou (a, b)))
let p3 = IlExiste a
let p4 = IlExiste (Imp (a, Non b))
let p5 = PourTout (Imp (Xor (a, b), c))
let p6 = PourTout (Imp (Non c, a))
let c1 = IlExiste b
let c2 = IlExiste c
let c3 = IlExiste d

exception BadCounter

(** print_premisses n fl : Affiche sur la sortie standard, "Prémisses :\n" avec 
    un décalage à gauche de n espace. Puis, toutes les formules présentes dans 
    la liste des prémises fl avec un décalage de (n+3) espace à gauche. 
    (l'affichage des formules s'éffectue à l'aide de la fonction 
    string_of_formule)
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_premisses (n : int) (fl : formule_syllogisme list) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ ->
      Printf.printf "%*sPrémisses :\n" n "";
      List.iter
        (fun f -> Printf.printf "%*s%s\n" (n + 3) "" (string_of_formule f))
        fl

(** print_conclusion n c : Affiche sur la sortie standard, "Conclusion :\n" 
    avec un décalage à gauche de n espace. Puis, la formule c avec un décalage 
    de (n+3) espace à gauche. (l'affichage de la formule s'éffectue à l'aide de 
    la fonction string_of_formule)
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_conclusion (n : int) (c : formule_syllogisme) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ ->
      Printf.printf "%*sConclusion :\n" n "";
      Printf.printf "%*s%s\n" (n + 3) "" (string_of_formule c)

(** print_diagramme n i d : Affiche sur la sortie standard, 
    "Diagramme " + (i + 1) + ":\n" avec un décalage à gauche de n espace. Puis, 
    le diagramme d avec un décalage de (n+3) espace à gauche. (l'affichage du 
    diagramme s'éffectue à l'aide de la fonction List.iter sur 
    list_string_of_diag d)
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_diagramme (n : int) (i : int) (d : diagramme) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ ->
      Printf.printf "%*sDiagramme %d:\n" n "" (i + 1);
      List.iter
        (fun l -> Printf.printf "%*s%s\n" (n + 3) "" l)
        (list_string_of_diag d)

(** print_diagrammes n dl : Affiche sur la sortie standard, la liste des 
    diagrammes présent dans dl avec un décalage de n espace à gauche.
    (l'affichage des diagrammes s'éffectue à l'aide de la fonction List.iter sur 
    print_diagramme n de dl)
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_diagrammes (n : int) (dl : diagramme list) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ -> List.iteri (print_diagramme n) dl

(** print_diagrames_premisses n dll : Affiche sur la sortie standard, 
    "Diagrammes de la combinaison :\n" avec un décalage à gauche de n espace.
    Puis, pour chaque liste de diagramme présente de dll, 
    "Diagrammes de la prémisse " + (i + 1) + ":\n" (avec un décalage de (n + 3) 
    espace à gauche) suivit de l'affichages des diagrammes présent dans cette 
    liste avec un décalge de (n + 6). (affichage à l'aide de la fonction 
    print_diagrammes sur la liste)
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_diagrames_premisses (n : int) (dll : diagramme list list) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ ->
      Printf.printf "%*sDiagrammes de chaque prémisse :\n" n "";
      List.iteri
        (fun i dl ->
          Printf.printf "%*sDiagrammes de la prémisse %d:\n" (n + 3) "" (i + 1);
          print_diagrammes (n + 6) dl)
        dll

(** print_diagrames_combinaison n comb_dl : Affiche sur la sortie standard, 
    "Diagrammes de la combinaison :\n" avec un décalage à gauche de n espace. 
    Puis, les diagrammes présent dans la liste comb_dl (à l'aide de la fonction
    print_diagrammes avec un décalage a gauche de (n + 3) espace).
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_diagrames_combinaison (n : int) (comb_dl : diagramme list) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ ->
      Printf.printf "%*sDiagrammes de la combinaison :\n" n "";
      print_diagrammes (n + 3) comb_dl

(** print_diagrames_conclusion n dl : Affiche sur la sortie standard, 
    "Diagrammes de la conclusion :\n" avec un décalage à gauche de n espace. 
    Puis, les diagrammes présent dans la liste dl (à l'aide de la fonction
    print_diagrammes avec un décalage a gauche de (n + 3) espace).
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_diagrames_conclusion (n : int) (dl : diagramme list) : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ ->
      Printf.printf "%*sDiagrammes de la conclusion:\n" n "";
      print_diagrammes (n + 3) dl

(** print_comp_conclusion n comb c : Affiche sur la sortie standard, 
    "Conclusion " suivie de "compatible avec les diagrammes.\n" dans le cas où 
    comb est compatible avec c sinon 
    "incompatible avec les diagrammes, contre-exemples :\n". Ce méssage 
    s'affiche avec un décalage à gauche de n espace. Aussi dans le cas 
    d'incompatibilité, les contre-exemples sont affiché à l'aide de la fonction 
    print_diagrammes.(d'un décalage de (n+3) espace a gauche)
    Dans le cas où n n'est pas un naturel, l'exception BadCounter est levé et le 
    traitement ci-dessus n'est pas éxecuter.*)
let print_comp_conclusion (n : int) (comb : diagramme list) (c : diagramme list)
    : unit =
  match n with
  | n' when n' < 0 -> raise BadCounter
  | _ -> (
      Printf.printf "%*sConclusion " n "";
      match temoins_incompatibilite_premisses_conc_diag comb c with
      | [] -> Printf.printf "compatible avec les diagrammes.\n"
      | dl ->
          Printf.printf "incompatible avec les diagrammes, contre-exemples :\n";
          print_diagrammes (n + 3) dl)

(** test premisses conclusion : teste si chacun des diagrammes de la combinaison
    de la liste prémisses est compatible avec au moins un des diagrammes de 
    conclusion, tout en traçant les calculs réalisés et les diagrammes calculés,
    et en affichant tous les contre-exemples le cas échéant. *)
let test (fl : formule_syllogisme list) (c : formule_syllogisme) : unit =
  let a =
    List.sort_uniq compare
      (List.concat (List.map atomes_of_formule fl) @ atomes_of_formule c)
  in
  print_premisses 0 fl;
  print_conclusion 0 c;
  let dpl = List.map (diag_from_formule a) fl in
  print_diagrames_premisses 0 dpl;
  let comb_dl = (match fl with
    | [] -> []
    | _ -> List.fold_left
          (fun acc dl -> conj_diag_list acc dl)
          (List.hd dpl) (List.tl dpl)) in
  print_diagrames_combinaison 0 comb_dl;
  let dc = diag_from_formule a c in
  print_diagrames_conclusion 0 dc;
  print_comp_conclusion 0 comb_dl dc;;

(* Syllogisme du td *)

exception AssertionError

let test_unit (fl : formule_syllogisme list) (c : formule_syllogisme) (t : bool) : unit =
  match est_compatible_premisses_conc fl c with
  | b when b <> t -> raise AssertionError
  | _ -> print_endline "Test unit pass";;

(* q1 *)
print_endline "Question 1\n";;

let p1 = PourTout (a |*| b) in
let c = PourTout a in
test_unit [ p1 ] c true
;;

print_newline ();;

(* q2 *)
print_endline "Question 2\n";;

let p1 = IlExiste (a |*| b) in
let c = IlExiste a in
test_unit [ p1 ] c true
;;

print_newline ();;

(* q3 *)
print_endline "Question 3\n";;

let p1 = PourTout a in
let p2 = PourTout b in
let c = PourTout (a |*| b) in
test_unit [ p1; p2 ] c true
;;

print_newline ();;

(* q4 *)
print_endline "Question 4\n";;

let p1 = IlExiste a in
let p2 = IlExiste b in
let c = IlExiste (a |*| b) in
test_unit [ p1; p2 ] c false
;;

print_newline ();;

(* q5 *)
print_endline "Question 5\n";;

let p1 = IlExiste b in
let c = IlExiste (a |+| b) in
test_unit [ p1 ] c true
;;

print_newline ();;

(* q6 *)
print_endline "Question 6\n";;

let p1 = PourTout a in
let c = PourTout (a |+| b) in
test_unit [ p1 ] c true
;;

print_newline ();;

(* q7 *)
print_endline "Question 7\n";;

let p1 = PourTout (a |+| b) in
let c = PourTout a in
test_unit [ p1 ] c false
;;

print_newline ();;

(* q8 *)
print_endline "Question 8\n";;

let p1 = PourTout (a |->| b) in
let p2 = IlExiste a in
let c = IlExiste b in
test_unit [ p1; p2 ] c true
;;

print_newline ();;

(* q9 *)
print_endline "Question 9\n";;

let p1 = PourTout (a |->| b) in
let p2 = IlExiste (Non b) in
let c = IlExiste (Non a) in
test_unit [ p1; p2 ] c true
;;

print_newline ();;

(* q10 *)
print_endline "Question 10\n";;

let p1 = PourTout (a |->| b) in
let p2 = PourTout a in
let c = PourTout b in
test_unit [ p1; p2 ] c true
;;

print_newline ();;

(* q11 *)
print_endline "Question 11\n";;

let p1 = IlExiste (a |->| b) in
let p2 = PourTout a in
let c = PourTout b in
test_unit [ p1; p2 ] c false
;;

print_newline ();;

(* q12 *)
print_endline "Question 12\n";;

let p1 = IlExiste (a |->| b) in
let p2 = PourTout a in
let c = IlExiste b in
test_unit [ p1; p2 ] c true
;;

print_newline ();;

(* q13 *)
print_endline "Question 13\n";;

let p1 = PourTout (a |++| b) in
let p2 = PourTout a in
let c = PourTout (Non b) in
test_unit [ p1; p2 ] c true
;;

print_newline ();;

(* q14 *)
print_endline "Question 14\n";;

let p1 = IlExiste (a |++| b) in
let p2 = IlExiste a in
let c = IlExiste (Non b) in
test_unit [ p1; p2 ] c false
;;

print_newline ();;

(* q15 *)
print_endline "Question 15\n";;

let p1 = PourTout (a |->| (b |->| c)) in
let p2 = PourTout (a |->| b) in
let p3 = PourTout a in
let c = PourTout c in
test_unit [ p1; p2; p3 ] c true
;;

print_newline ();

test [] (PourTout c)

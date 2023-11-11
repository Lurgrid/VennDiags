open Formule_Syllogisme
open Formule_Log_Prop
(* open DiagVenn *)

let a = Atome "a"
let b = Atome "b"
let c = Atome "c"
let d = Atome "d"
let p1 = PourTout (Imp (Ou (a, b), c))
let p2 = PourTout (Imp (c, Ou (a, b)))
let p3 = IlExiste a
let p4 = IlExiste (Imp (a, Non b))
let p5 = PourTout (Imp (Xor (a, b), c))
let p6 = PourTout (Imp (Non c, a))
let c1 = IlExiste b
let c2 = IlExiste c
let c3 = IlExiste d

(** test premisses conclusion : teste si chacun des diagrammes de la combinaison
    de la liste prémisses est compatible avec au moins un des diagrammes de conclusion,
    tout en traçant les calculs réalisés et les diagrammes calculés,
    et en affichant tous les contre-exemples le cas échéant. *)
let test (_ : formule_syllogisme list) (_ : formule_syllogisme) : unit =
  failwith "à faire"

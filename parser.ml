type lexeme =
  (* Mots-clefs: *)
  | Select
  | From
  | Where
  | As
  | And | Or | Not
  | OrderBy | Asc | Desc
  | IsNull | IsNotNull
  | Limit | Offset
  | Union | Intersect | Except
  | Join | On
  | Min | Max | Sum | Avg
  | Count | Distinct
  | GroupBy | Having
  (* Autres lexemes *)
  | Asterisque
  | ParG | ParD
  | Virgule
  | Nom of string
  | Point
  | Egal | NonEgal
  | PlusGrand | PlusPetit
  | PlusGrandEgal | PlusPetitEgal
  | String of string
  | Valeur of int
  | Fois | Plus | Moins | Div

let rec requete_parser (l : lexeme list) : lexeme list = l (** TODO. Q26 **)

let parser (l : lexeme list)  =
  match requete_parser l with
  | [] -> ()
  | _ -> failwith "Lexème inattendu."

let test (i : int) (l : lexeme list) (correct : bool) =
  let result = 
    try
      Printf.printf "Test no %d: " i;
      parser l;
      Printf.printf "Success\n";
      true
    with
    | Failure s -> (Printf.printf "Erreur : %s\n" s; false)
  in
  if correct <> result then failwith "Echec du test"

let _ =
  test 1 [Select ; Nom("A") ; From ; Nom("T")] true;
  test 2 [Select ; Nom("A") ; From] false;
  test 3 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T")] true ;
  test 4 [Select ; Nom("A") ; Point ; Nom("A") ; From ; Nom("T") ; Where ; Nom("A") ; Egal ; Valeur(5)] true

(*
|------------------------------------------------------------------------------------------------------------------|
|                                     PARTIE I - Expresionn Arithmétique                                           |
|------------------------------------------------------------------------------------------------------------------|
*)

(* Question 1 *)

let colonne_parser (l : lexeme list) : lexeme list = (*? Détecte deux types de parenthèses *)
  match l with
  | Nom _ :: Point :: Nom _ :: t -> t 
  | Nom _ :: t -> t
  | _ -> failwith "Nom de colonne attendu"
;; 

(* Question 4 *)

let parentheses (l : lexeme list) : lexeme list * lexeme list =
  let rec aux (e : lexeme list) (acc : lexeme list) : lexeme list * lexeme list =
    match e with 
    | [] -> failwith "Absence de parenthèse droite."
    | h::t when h = ParD -> if acc = [] then failwith "Parenthèses vides."
                            else ((List.rev acc),t) (*? 1er : contenu des parenthèses ; 2ème : contenu après les parenthèses *)
    | h::t -> aux t (h::acc)
  in aux l [] 
;;

let rec expression_parser (l :lexeme list) : lexeme list =
  match l with 
  | [] -> []
  | Nom(_)::t -> operateur(colonne_parser(l))
  | Valeur(_)::t -> operateur(t)
  | String(_)::t -> operateur(t)
  | ParG::t -> let c = parentheses(t) in (expression_parser(fst c))@(operateur(snd c))
  | _ -> l

and operateur (l : lexeme list) : lexeme list =
  match l with 
  | [] -> []
  | h::t when (h=Fois) || (h=Plus) || (h=Moins) || (h=Div)
           -> if t=[] then failwith "Opérateur sans expression à droite."
              else if (expression_parser(t))=[] then []
              else failwith "Opérateur dont le membre droit n'est pas une expression." 
  | _ -> l
;;

(*
|------------------------------------------------------------------------------------------------------------------|
|                                     PARTIE II - Formule Logique : Condition                                      |
|------------------------------------------------------------------------------------------------------------------|
*)

(* Question 5 - Niveau 1 *)

let rec condition_parser1 (l : lexeme list) : lexeme list =
  let first_pars = expression_parser l in
  match first_pars with
  | h :: t when (h = Egal || h = NonEgal || h = PlusGrand || h = PlusPetit || h = PlusGrandEgal || h = PlusPetitEgal) -> expression_parser t
  | _ -> failwith "Pas de condition"

(* Question 6 - Niveau 2 *)

(* let rec condition_parser2 (l : lexeme list) : lexeme list =
  | Not::q -> 
;;*)

  
(*
|------------------------------------------------------------------------------------------------------------------|
|                                         PARTIE III - Closes optionelles                                          |
|------------------------------------------------------------------------------------------------------------------|
*)

(* Question 8 *)

let where_clause_parser (l : lexeme list) : lexeme list = 
  match l with 
  | Where :: t -> t 
  | _ -> l
;;

(* Question 9 *)

let having_clause_parser (l : lexeme list) : lexeme list = 
  match l with 
  | Having :: t -> t 
  | _ -> l
;;

(* Question 10 *)

let rec liste_colonne_parser (l : lexeme list) : lexeme list = (*? Est ce qu'il est bien fait ?*)
  let temp = colonne_parser l in
  match temp with
  | Virgule :: t -> liste_colonne_parser t
  | _ -> temp
;;

(* Question 11 *)

let groupby_clause_parser (l : lexeme list) : lexeme list = 
  match l with
  | GroupBy :: t -> having_clause_parser (liste_colonne_parser t)
  | _ -> l
;;

(* Question 12 *)

let order_parser (l : lexeme list) : lexeme list = 
  match l with 
  | Asc :: t -> t
  | Desc :: t -> t 
  | _ -> l
;;

(* Question 13 *)

let rec liste_order_colonne_parser (l : lexeme list) : lexeme list = (* ? Même principe que la question 10*)
  let first_pars = colonne_parser l in
  let second_pars = order_parser first_pars in 
  match second_pars with
  | Virgule :: t -> liste_order_colonne_parser t
  | _ -> second_pars
;;

let orderby_parser (l :lexeme list) : lexeme list = 
  match l with 
  | OrderBy :: t -> liste_order_colonne_parser t
  | _ -> l 
;;

(* Question 14 *)

let offset_close_parser (l : lexeme list) : lexeme list = 
  match l with 
  | Offset :: Valeur (_) :: t  -> t
  | _ -> l 
;;

(* Question 15 *)

let limit_clause_parser (l : lexeme list) : lexeme list = 
  match l with 
  | Limit :: Valeur (_) :: t -> offset_close_parser t 
  | _ -> l 
;;

(*
|------------------------------------------------------------------------------------------------------------------|
|                              PARTIE IV - Opérations sur les tables : Clause FROM                                 |
|------------------------------------------------------------------------------------------------------------------|
*)

(* Question 17 *)

let alias_parser (l : lexeme list) : lexeme list = 
  match l with 
  | As :: Nom (_) :: t -> t
  | _ -> l
;;

let tabledb_parser (l : lexeme list) : lexeme list = 
  match l with 
  | Nom (_) :: t -> alias_parser t 
  | _ -> failwith "Pas de nom trouvé"
;;

let onoption_parser (l : lexeme list) : lexeme list = (* ! J'ai besoin du condition parser ici !! *)
  match l with 
  | On :: t -> condition_parser1 t (* Mettre la version finale du condition parser ii*)
  | _ -> l
;;

let rec jointable_parser (l : lexeme list) : lexeme list = (* TODO a finir - j'ai d'abord besoin de savoir ce que fait ta fct parenthèses*)
  match l with 
  | Join ::t -> onoption_parser (table_parser t)
  | _ -> l

and table_parser (l : lexeme list) : lexeme list =
  match l with 
  | ParG :: t -> let c = parentheses t in (table_parser (fst c))@(alias_parser (jointable_parser (snd c)))
  | l -> jointable_parser (tabledb_parser l)
;;

(* Question 19 *)

let rec listetable_parser (l : lexeme list) : lexeme list = 
  let temp = table_parser l in 
  match temp with 
  | Virgule :: t -> listetable_parser t 
  | _ -> temp
;;

(*
|------------------------------------------------------------------------------------------------------------------|
|                                                      Tests                                                       |
|------------------------------------------------------------------------------------------------------------------|
*)

let test_parser (i : int) (parser_to_test) (l : lexeme list) (correct : bool) =
  let result = 
    try
      Printf.printf "Test no %d: " i;
      (match parser_to_test l with
      | [] -> ()
      | _ -> failwith "Lexème inattendu");
      Printf.printf "Success\n";
      true
    with
    | Failure s -> (Printf.printf "Erreur : %s\n" s; false)
  in
  if correct <> result then failwith "Echec du test"

let _ =
test_parser 1 colonne_parser [Nom("A") ; Point ; Nom("T")] true;
test_parser 2 colonne_parser [Nom("A") ; From] false;
test_parser 3 colonne_parser [Nom("A")] true;
test_parser 1 expression_parser [Nom("A") ; Point ; Nom("T")] true;
test_parser 2 expression_parser [Nom("A") ; From] false;
test_parser 3 expression_parser [Nom("A")] true;
test_parser 4 expression_parser [Nom("A") ; Plus ; ParG ; Valeur(3) ; Fois ; String("nom"); ParD] true;
test_parser 5 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; Valeur 1; ParD; ParG ; Valeur(3) ; ParD] false;
test_parser 6 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; ParD; ParG ; Valeur(3) ; ParD] false;
test_parser 7 expression_parser [ParG ; Nom("A") ; Div ; Valeur(2) ; Plus ; Valeur 1; ParD; Fois ; ParG ; Valeur(3) ; ParD] true

(*
! Partie 3 intégralement finie 
 Je te laisse vérifier que j'ai tout bien fait stv 
 Juste je t'ai laissé un message a un endroit pour etre sur tu verra
! J'ai aussi skip toutes les questions théoriques on verra plus tard pour les faire   
! Partie 4 finie aussi 
*)

(* Problem 1 *)
let rec last = function
    | [] -> None
    | [ b ] -> Some b
    | _ :: a -> last a
;;

(* Problem 2 *)
let rec last_two = function
    | [] | [ _ ] -> None
    | [ a; b ] -> Some (a, b)
    | _ :: b -> last_two b
;;

(* Problem 3 *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k - 1) t
;;

(* Problem 4 *)
let rec length = function
    | [] -> 0
    | _ :: b -> 1 + length b
;;

let length2 l =
    let rec aux n = function
        | [] -> n
        | _ :: t -> aux (n + 1) t
    in
        aux 0 l
;;

(* Problem 5 *)
let rev l =
    let rec aux l2 = function
        | [] -> l2
        | h :: t -> aux (h :: l2) t
    in
        aux [] l
;;

(* Problem 6 *)
let is_palindrome list = list = rev list

(* Problem 7 *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
    let rec aux acc = function
        | [] -> acc
        | One h :: t -> aux (h :: acc) t
        | Many h :: t -> aux (aux acc h) t
    in
        rev (aux [] list)
;;

(* Problem 8 *)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | rest -> rest
;;

(* Problem 9 *)
let pack list =
    let rec aux sub main = function
        | a :: (b :: _ as tail) ->
            if a = b
            then aux (a :: sub) main tail
            else aux [] ((a :: sub) :: main) tail
        | a :: [] -> aux (a :: sub) main []
        | [] -> sub :: main
    in
        List.rev (aux [] [] list)
;;

let pack2 list =
    let rec aux current acc = function
        | [] -> []
        | [ x ] -> (x :: current) :: acc
        | a :: (b :: _ as t) ->
            if a = b
            then aux (a :: current) acc t
            else aux [] ((a :: current) :: acc) t
    in
        List.rev (aux [] [] list)
;;

(* Problem 10 *)
let encode list =
    let rec aux = function
        | (a :: _ as h) :: t -> (length h, a) :: aux t
        | _ -> []
    in
        aux (pack list)
;;

let encode2 list =
    let rec aux acc counter = function
        | a :: (b :: _ as t) ->
            if a = b
            then aux acc (counter + 1) t
            else aux ((counter + 1, a) :: acc) 0 t
        | [ a ] -> (counter + 1, a) :: acc
        | [] -> []
    in
        List.rev (aux [] 0 list)
;;

(* Problem 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode3 list =
    let rec aux counter acc = function
        | a :: (b :: _ as t) ->
            if a = b
            then aux (counter + 1) acc t
            else if counter = 0
            then aux 0 (One a :: acc) t
            else aux 0 (Many (counter + 1, a) :: acc) t
        | [ a ] ->
            if counter = 0 then One a :: acc else Many (counter + 1, a) :: acc
        | [] -> []
    in
        List.rev (aux 0 [] list)
;;

(* Problem 12 *)
let decode list =
    let rec aux acc = function
        | [] -> acc
        | One a :: t -> aux (a :: acc) t
        | Many (c, a) :: t ->
            if c = 1
            then aux (a :: acc) t
            else aux (a :: acc) (Many (c - 1, a) :: t)
    in
        List.rev (aux [] list)
;;

(* Problem 13 *)
let encode4 list =
    let rle counter a = if counter = 1 then One a else Many (counter, a) in
    let rec aux counter acc = function
        | a :: (b :: _ as t) ->
            if a = b
            then aux (counter + 1) acc t
            else aux 0 (rle (counter + 1) a :: acc) t
        | [ a ] -> rle (counter + 1) a :: acc
        | [] -> []
    in
        List.rev (aux 0 [] list)
;;

(* Problem 14 *)
let duplicate list =
    let rec aux acc = function
        | h :: t -> aux (h :: h :: acc) t
        | [] -> acc
    in
        List.rev (aux [] list)
;;

let rec duplicate2 = function
    | h :: t -> h :: h :: duplicate2 t
    | [] -> []
;;

(* Problem 15 *)
let replicate list n =
    let rec aux count acc = function
        | h :: t ->
            if count = 0
            then aux n acc t
            else aux (count - 1) (h :: acc) (h :: t)
        | [] -> acc
    in
        List.rev (aux n [] list)
;;

(* Problem 16 *)
let drop list n =
    let rec aux count acc = function
        | h :: t ->
            if count = 1 then aux n acc t else aux (count - 1) (h :: acc) t
        | [] -> acc
    in
        List.rev (aux n [] list)
;;

(* Problem 17 *)
let split list n =
    let rec aux i acc = function
        | h :: t ->
            if i = 0 then List.rev acc, h :: t else aux (i - 1) (h :: acc) t
        | [] -> List.rev acc, []
    in
        aux n [] list
;;

(* Problem 18 *)
let slice list i k =
    let rec aux idx acc = function
        | h :: t ->
            if idx >= i && idx <= k
            then aux (idx + 1) (h :: acc) t
            else if idx > k
            then List.rev acc
            else aux (idx + 1) acc t
        | [] -> List.rev acc
    in
        aux 0 [] list
;;

let rec fold_until f acc n = function
    | h :: t as l -> if n = 0 then acc, l else fold_until f (f acc h) (n - 1) t
    | [] -> acc, []
;;

let slice2 list i k =
    let _, list = fold_until (fun _ _ -> []) [] i list in
    let acc, _ = fold_until (fun a h -> h :: a) [] (k - i + 1) list in
        List.rev acc
;;

(* Problem 19 *)

let rotate list n =
    if n >= 0
    then (
      let l1, l2 = split list n in
          l2 @ l1)
    else (
      let l1, l2 = split list (length list + n) in
          l2 @ l1)
;;

(* Problem 20 *)
let remove_at k list =
    let l1, l2 = fold_until (fun acc h -> h :: acc) [] k list in
    let _, l2 = fold_until (fun _ _ -> []) [] 1 l2 in
        List.rev l1 @ l2
;;

let rec remove_at2 k = function
    | h :: t -> if k = 0 then t else h :: remove_at2 (k - 1) t
    | [] -> []
;;

(* Problem 21 *)
let rec insert_at elem k = function
    | h :: t -> if k = 0 then elem :: h :: t else h :: insert_at elem (k - 1) t
    | [] -> [ elem ]
;;

(* Problem 22 *)
let range i k =
    let rec aux n m acc = if n <= m then aux (n + 1) m (n :: acc) else acc in
        if i <= k then List.rev (aux i k []) else aux k i []
;;

(* Problem 23 *)

let rand_select list k =
    let rec extract i acc = function
        | h :: t -> if i = 0 then h, acc @ t else extract (i - 1) (h :: acc) t
        | [] -> raise Not_found
    in
    let rec aux n list =
        if n = 0
        then []
        else (
          let elem, list = extract (Random.int (length list)) [] list in
              elem :: aux (n - 1) list)
    in
        aux k list
;;

(* Problem 24 *)
let lotto_select n r =
    let list = range 1 r in
        rand_select list n
;;

lotto_select 6 49

(* Problem 25 *)
let permutation list = rand_select list (length list);;

permutation [ "a"; "b"; "c"; "d"; "e"; "f" ]

(* Problem 26 *)
(* let extract list k = *)
(*     let rec extract_aux i acc = function *)
(*         | h :: t -> if i = 0 then h, acc @ t else extract_aux (i - 1) (h :: acc) t *)
(*         | [] -> raise Not_found *)
(*     in *)
(*     let len = length list in *)
(*     let rec aux acc rest start  = function *)
(*     | h::t -> aux ((h::start, t@rest)::acc) (h::rest) start t *)
(*     | [] -> acc *)
(*     in let rec aux2 acc range =  *)
(*          *)

let rec extract k list =
    let open Base in
    if k = 0
    then [ [] ]
    else (
      match list with
      | hd :: tl -> List.map ~f:(fun l -> hd :: l) (extract (k - 1) tl)
      | [] -> [])
;;

extract 0 []

let rec print_list = function
    | [] -> print_string "[] | "
    | hd :: tl ->
        print_string hd;
        print_list tl
;;

let rec extractt (k : int) (list : string list) =
    let open Base in
    if k <= 0
    then [ [] ]
    else (
      match list with
      | [] -> []
      | h :: tl ->
          let with_h = List.map ~f:(fun l -> h :: l) (extractt (k - 1) tl) in
          let without_h = extractt k tl in
              let open Stdio in
              print_endline ("k = " ^ Int.to_string k ^ "\n");
              print_string "with_h: ";
              List.iter ~f:print_list with_h;
              print_string "\n";
              print_string "without_h: ";
              List.iter ~f:print_list without_h;
              print_string "\n";
              with_h @ without_h)
;;

extractt 0 []

(* Arithmetic Section *)

(* SKIP *)

(* Logic and Codes *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let _ = Not (Var "a")

(* Problem 46 & 47. *)

let rec eval2 a a_val b b_val = function
    | Var x ->
        if x = a
        then a_val
        else if x = b
        then b_val
        else failwith "Unkown value"
    | Not x -> not (eval2 a a_val b b_val x)
    | And (x, y) -> eval2 a a_val b b_val x && eval2 a a_val b b_val y
    | Or (x, y) -> eval2 a a_val b b_val x || eval2 a a_val b b_val y
;;

let table2 a b expr =
    [ true, true, eval2 a true b true expr
    ; true, false, eval2 a true b false expr
    ; false, true, eval2 a false b true expr
    ; false, false, eval2 a false b false expr
    ]
;;

(* Problem 48 *)
(* TODO can I generalize this to work for more than just true,false as possible Var values? *)
let bool_permutations variables =
    let open Base in
    let rec aux acc = function
        | _ :: (_ :: _ as tl) ->
            let acc =
                List.concat
                  (List.map ~f:(fun l -> [ true :: l; false :: l ]) acc)
            in
                aux acc tl
        | [ _ ] -> acc
        | [] -> []
    in
        aux [ [ true ]; [ false ] ] variables
;;

(* Online Solution *)
let rec bool_permutations2 variables acc =
    match variables with
    | hd :: tl ->
        bool_permutations2 tl ((hd, true) :: acc)
        @ bool_permutations2 tl ((hd, false) :: acc)
    | [] -> acc
in
bool_permutations2 ["a";"b"] []
;;

let match_var_values variables list =
    let open Base in
    List.map ~f:(fun x -> List.map2 ~f:(fun v b -> v, b) variables x) list
;;

let rec eval vars expr =
    let rec get_val v = function
        | (a, a_val) :: tl -> if a = v then a_val else get_val v tl
        | [] -> failwith "EEEE"
    in
        match expr with
        | Var a -> get_val a vars
        | Not x -> not (eval vars x)
        | And (x, y) -> eval vars x && eval vars y
        | Or (x, y) -> eval vars x || eval vars y
;;

let table variables expr =
    let open Base in
    let inputs = match_var_values variables (bool_permutations variables) in
        List.map
          ~f:(fun x ->
            match x with
            | Ok x -> x, eval x expr
            | _ -> failwith "e")
          inputs
;;

let a = Var "a"
and b = Var "b"
and c = Var "c" in
    table
      [ "a"; "b"; "c" ]
      (Or (And (a, Or (b, c)), Or (And (a, b), And (a, c))))

(* Run tests cases *)
let test nr f inp out =
    print_endline (string_of_int nr ^ " ==> " ^ string_of_bool (f inp = out))
;;

let () =
    print_endline "\nRunning test cases:";
    test 1 last [ "a"; "b"; "c"; "d" ] (Some "d");
    test 1 last [] None;
    test 2 last_two [ "a"; "b"; "c"; "d" ] (Some ("c", "d"));
    test 2 last_two [ "a" ] None;
    test 3 (at 3) [ "a"; "b"; "c"; "d"; "e" ] (Some "c");
    test 3 (at 3) [ "a" ] None;
    test 4 length [ "a"; "b"; "c" ] 3;
    test 4 length [] 0;
    test 4 length2 [ "a"; "b"; "c" ] 3;
    test 4 length2 [] 0;
    test 5 rev [ "a"; "b"; "c" ] [ "c"; "b"; "a" ];
    test 6 is_palindrome [ "x"; "a"; "m"; "a"; "x" ] true;
    test 6 is_palindrome [ "a"; "b" ] false;
    test
      7
      flatten
      [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
      [ "a"; "b"; "c"; "d"; "e" ];
    test
      8
      compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      [ "a"; "b"; "c"; "a"; "d"; "e" ];
    test
      9
      pack
      [ "a"
      ; "a"
      ; "a"
      ; "a"
      ; "b"
      ; "c"
      ; "c"
      ; "a"
      ; "a"
      ; "d"
      ; "d"
      ; "e"
      ; "e"
      ; "e"
      ; "e"
      ]
      [ [ "a"; "a"; "a"; "a" ]
      ; [ "b" ]
      ; [ "c"; "c" ]
      ; [ "a"; "a" ]
      ; [ "d"; "d" ]
      ; [ "e"; "e"; "e"; "e" ]
      ];
    test
      9
      pack2
      [ "a"
      ; "a"
      ; "a"
      ; "a"
      ; "b"
      ; "c"
      ; "c"
      ; "a"
      ; "a"
      ; "d"
      ; "d"
      ; "e"
      ; "e"
      ; "e"
      ; "e"
      ]
      [ [ "a"; "a"; "a"; "a" ]
      ; [ "b" ]
      ; [ "c"; "c" ]
      ; [ "a"; "a" ]
      ; [ "d"; "d" ]
      ; [ "e"; "e"; "e"; "e" ]
      ];
    test
      10
      encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ];
    test
      10
      encode2
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ];
    test
      11
      encode3
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      [ Many (4, "a")
      ; One "b"
      ; Many (2, "c")
      ; Many (2, "a")
      ; One "d"
      ; Many (4, "e")
      ];
    test
      12
      decode
      [ Many (4, "a")
      ; One "b"
      ; Many (2, "c")
      ; Many (2, "a")
      ; One "d"
      ; Many (4, "e")
      ]
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ];
    test
      13
      encode4
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
      [ Many (4, "a")
      ; One "b"
      ; Many (2, "c")
      ; Many (2, "a")
      ; One "d"
      ; Many (4, "e")
      ];
    test
      14
      duplicate
      [ "a"; "b"; "c"; "c"; "d" ]
      [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ];
    test
      14
      duplicate2
      [ "a"; "b"; "c"; "c"; "d" ]
      [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ];
    test
      15
      (replicate [ "a"; "b"; "c" ])
      3
      [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ];
    test
      16
      (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
      3
      [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ];
    test
      17
      (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
      3
      ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]);
    test 17 (split [ "a"; "b"; "c"; "d" ]) 5 ([ "a"; "b"; "c"; "d" ], []);
    test
      18
      (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2)
      6
      [ "c"; "d"; "e"; "f"; "g" ];
    test
      18
      (slice2 [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2)
      6
      [ "c"; "d"; "e"; "f"; "g" ];
    test
      19
      (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ])
      3
      [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ];
    test
      19
      (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ])
      (-2)
      [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ];
    test 20 (remove_at 1) [ "a"; "b"; "c"; "d" ] [ "a"; "c"; "d" ];
    test 20 (remove_at2 1) [ "a"; "b"; "c"; "d" ] [ "a"; "c"; "d" ];
    test
      21
      (insert_at "alfa" 1)
      [ "a"; "b"; "c"; "d" ]
      [ "a"; "alfa"; "b"; "c"; "d" ];
    test
      21
      (insert_at "alfa" 3)
      [ "a"; "b"; "c"; "d" ]
      [ "a"; "b"; "c"; "alfa"; "d" ];
    test
      21
      (insert_at "alfa" 4)
      [ "a"; "b"; "c"; "d" ]
      [ "a"; "b"; "c"; "d"; "alfa" ];
    test 22 (range 4) 9 [ 4; 5; 6; 7; 8; 9 ];
    test 22 (range 9) 4 [ 9; 8; 7; 6; 5; 4 ];
    test
      46
      (table2 "a" "b")
      (And (Var "a", Or (Var "a", Var "b")))
      [ true, true, true
      ; true, false, true
      ; false, true, false
      ; false, false, false
      ];
    ()
;;

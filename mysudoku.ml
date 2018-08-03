(* solveur pour sudoku *)
(* FX Hugot *)

(*man
 * usage: mysudoku <puzzlename>
 *
 * compilation:
 * avec
 *   ocamlopt bigarray.cmxa mysudoku.ml -o mysudoku
 * ou avec
 *   ocamlc bigarray.cma mysudoku.ml -o mysudoku
 *
 * fichier: puzzle
 *  cat > puzzle <<eof
 *  9
 *  001005000
 *  050490000
 *  000102064
 *  000000750
 *  600000001
 *  035000000
 *  460903000
 *  000024090
 *  003600100
 *  eof
 *  
 *)


(* parametrage *)
let debug = 0

let a2get a i j = a.(i).(j)
let a2set a i j v = a.(i).(j) <- v

(* 0:inconnu
   1-9, A-.: nombre
 *)
let ch0 = int_of_char '0'
let cha = int_of_char 'A'

let char2int ch =
  let ich = int_of_char ch in
  if ich < ch0 + 10 then ich - ch0
  else ich - cha + 10

let int2char ich =
  if ich < 10 then char_of_int (ich + ch0)
  else char_of_int (ich - 10 + cha)

type parmsT = {
  size: int;
  subsizei: int;
  subsizej: int;
  a: int array array;
}

(* modes *)
let createParms size a =
  match size with
  | 8 -> { size; subsizei=4; subsizej=2; a }
  | 9 -> { size; subsizei=3; subsizej=3; a }
  | 16 -> { size; subsizei=4; subsizej=4; a }
  | _ -> failwith "invalid data"

(* lecture de la grille *)
let readGrid fdi size =
  Array.init size (fun _ ->
    input_char fdi |> ignore;
    Array.init size (fun _ -> input_char fdi |> char2int))

let readParms fdi =
  let size = input_char fdi |> char2int in
  let a = readGrid fdi size in
  createParms size a

(* ecriture de la grille *)
let dumpGrid fdo a =
  Array.iter (fun v -> Array.iter (fun e ->  output_char fdo (int2char e)) v; output_char fdo '\n') a

(* ajout d'un element dans une liste triee *)
let addSorted a lst =
  let rec asrec res lst =
    match lst with
    | [] -> List.rev_append (a::res) lst
    | car::cdr ->
      if a = car then List.rev_append res lst
      else if a < car then List.rev_append res (a::lst)
      else asrec (car::res) cdr in
  asrec [] lst

(* "complement a size" d'une liste *)
let sComplement lst size =
  let rec screc res i lst =
    if i = size + 1 then List.rev res
    else match lst with
    | [] -> screc (i::res) (succ i) []
    | car::cdr ->
      if i = car then screc res (succ i) cdr
      else if i < car then screc (i::res) (succ i) lst
      else screc res i cdr in
  screc [] 1 lst

(* recuperation des chiffres deja utilises
     - sur la meme ligne
     - sur la meme colonne
     - dans le meme carre 
 *)
let findImpossible { size; subsizei; subsizej; a } i j =
  let ibl = (i / subsizei) * subsizei and
      jbl = (j / subsizej) * subsizej in
  (* recherche horizontale des nombres deja pris *)
  let rec fhprec res n =
    if n = size then res
    else let v = a2get a i n in
    if v = 0 then fhprec res (succ n)
    else fhprec (addSorted v res) (succ n) in
  (* recherche verticale des nombres deja pris *)
  let rec fvprec res p =
    if p = size then res
    else let v = a2get a p j in
    if v = 0 then fvprec res (succ p)
    else fvprec (addSorted v res) (succ p) in
  (* recherche dans le carre *)
  let rec fcprec res n p =
    if p = subsizej then res
    else if n = subsizei then fcprec res 0 (succ p)
    else let v = a2get a (ibl + n) (jbl + p) in
    if v = 0 then fcprec res (succ n) p
    else fcprec (addSorted v res) (succ n) p in
  let res = fhprec [] 0 in
  let res = fvprec res 0 in
  let res = fcprec res 0 0 in
  res

(* recherche des nombres a decouvrir *)
(* deux possibilites de performances inegales sont dispo
     - findUnknownCoordinates_first: on prend le premier non defini
     - findUnknownCoordinates_min: on prend celui qui offre le 
     	minimum de possibilites au premier tour
 *)
let findUnknownCoordinates_first { size; a } =
  let rec frec i j =
    if i = size - 1 && j = size - 1 then (
      if a2get a i j = 0 then Some (i, j)
      else None
    )
    else if i = size - 1 then (
      if a2get a i j = 0 then Some (i, j)
      else frec 0 (succ j)
    )
    else (
      if a2get a i j = 0 then Some (i, j)
      else frec (succ i) j 
    ) in
  frec 0 0

let findUnknownCoordinates_min conf =
  let size, a = conf.size, conf.a in
  let rec frec res len i j =
    (* fin de grille *)
    if i = size - 1 && j = size - 1 then (
      if a2get a i j = 0 then (
        let lenl = List.length (findImpossible conf i j) in
        if lenl > len then Some (i, j)
	else res
      )
      else res
    )
    (* fin de ligne *)
    else if i = size - 1 then (
      if a2get a i j = 0 then (
        let lenl = List.length (findImpossible conf i j) in
	if lenl > len then frec (Some (i, j)) lenl 0 (succ j)
	else frec res len 0 (succ j)
      )
      else frec res len 0 (succ j)
    )
    (* element suivant *)
    else (
      if a2get a i j = 0 then (
	let lenl = List.length (findImpossible conf i j) in
	if lenl > len then frec (Some (i, j)) lenl (succ i) j
	else frec res len (succ i) j
      )
      else frec res len (succ i) j
    ) in
  frec None (-1) 0 0

(* on defini quelle methode est choisie *)
let findUnknownCoordinates = findUnknownCoordinates_min

(* solveur *)
let rec solveGrid fdo level conf =
  if debug > 0 && level mod 64 = 0 then Printf.fprintf fdo "# niveau = %d\n" level;
  match findUnknownCoordinates conf with
  | None -> dumpGrid fdo conf.a; output_string fdo "---\n"; 1
  | Some (i, j) ->
    let lst = findImpossible conf i j in
    let lst = sComplement lst conf.size in
    if lst = [] then 0
    else List.fold_left (fun count n -> 
      a2set conf.a i j n;
      let count = count + solveGrid fdo (succ level) conf in
      a2set conf.a i j 0;
      count
    ) 0 lst

(* emballage *)
let _ =
  let fdi = open_in Sys.argv.(1) in
  let fdo = stdout in
  let conf = readParms fdi in
  Printf.fprintf fdo "# grille traitee:\n";
  dumpGrid fdo conf.a;
  Printf.fprintf fdo "# =========\n";
  let count = solveGrid fdo 0 conf in
  Printf.fprintf fdo "# nb de solutions: %d\n" count



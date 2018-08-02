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

open Bigarray

(* parametrage *)
let gridtype = `G9x9
let debug = 0


(* traductions *)
let chdiv = int_of_char '/'
let ch0 = int_of_char '0'
let chat = int_of_char '@'

(* 0: inconnu
   1-9, A-.: nombre
 *)
let char2int_num1 ch =
  let ich = int_of_char ch in
  if ich < ch0 + 10 then ich - ch0
  else ich - chat + 9

let int2char_num1 ich =
  if ich < 10 then char_of_int (ich + ch0)
  else char_of_int (ich - 9 + chat)

(* /:inconnu
   0-9,
   A-.: nombre
 *)
let char2int_num0 ch =
  let ich = int_of_char ch in
  if ich < chdiv + 11 then ich - chdiv
  else ich - chat + 10

let int2char_num0 ich =
  if ich < 11 then char_of_int (ich + chdiv)
  else char_of_int (ich - 10 + chat)

(* modes *)
let size, subsizei, subsizej, int2char, char2int= (
  match gridtype with
    `G9x9 -> 9, 3, 3, int2char_num1, char2int_num1
  | `G16x16T0 -> 16, 4, 4, int2char_num0, char2int_num0
  | `G8x8P4x2 -> 8, 4, 2, int2char_num1, char2int_num1
  | _ -> 9, 3, 3, int2char_num1, char2int_num1
)

(* lecture de la grille *)
let readGrid fdi a =
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      Array2.set a i j (char2int (input_char fdi))
    done;
    ignore (input_char fdi)
  done

(* ecriture de la grille *)
let dumpGrid fdo a =
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      output_char fdo (int2char (Array2.get a i j))
    done;
    output_char fdo '\n';
  done

(* ajout d'un element dans une liste triee *)
let addSorted a lst =
  let rec asrec res lst =
    match lst with
      [] -> List.rev_append (a::res) lst
    | car::cdr ->
      if a = car then List.rev_append res lst
      else if a < car then List.rev_append res (a::lst)
      else asrec (car::res) cdr in
  asrec [] lst

(* "complement a size" d'une liste *)
let sComplement lst =
  let rec screc res i lst =
    if i = size + 1 then List.rev res
    else match lst with
      [] -> screc (i::res) (succ i) []
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
let findImpossible a i j =
  let ibl = (i / subsizei) * subsizei and
      jbl = (j / subsizej) * subsizej in
  (* recherche horizontale des nombres deja pris *)
  let rec fhprec res n =
    if n = size then res
    else let v = Array2.get a i n in
    if v = 0 then fhprec res (succ n)
    else fhprec (addSorted v res) (succ n) in
  (* recherche verticale des nombres deja pris *)
  let rec fvprec res p =
    if p = size then res
    else let v = Array2.get a p j in
    if v = 0 then fvprec res (succ p)
    else fvprec (addSorted v res) (succ p) in
  (* recherche dans le carre *)
  let rec fcprec res n p =
    if p = subsizej then res
    else if n = subsizei then fcprec res 0 (succ p)
    else let v = Array2.get a (ibl + n) (jbl + p) in
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
let findUnknownCoordinates_first a =
  let rec frec i j =
    if i = size - 1 && j = size - 1 then (
      if Array2.get a i j = 0 then Some (i, j)
      else None
    )
    else if i = size - 1 then (
      if Array2.get a i j = 0 then Some (i, j)
      else frec 0 (succ j)
    )
    else (
      if Array2.get a i j = 0 then Some (i, j)
      else frec (succ i) j 
    ) in
  frec 0 0

let findUnknownCoordinates_min a =
  let rec frec res len i j =
    (* fin de grille *)
    if i = size - 1 && j = size - 1 then (
      if Array2.get a i j = 0 then (
        let lenl = List.length (findImpossible a i j) in
        if lenl > len then Some (i, j)
	else res
      )
      else res
    )
    (* fin de ligne *)
    else if i = size - 1 then (
      if Array2.get a i j = 0 then (
        let lenl = List.length (findImpossible a i j) in
	if lenl > len then frec (Some (i, j)) lenl 0 (succ j)
	else frec res len 0 (succ j)
      )
      else frec res len 0 (succ j)
    )
    (* element suivant *)
    else (
      if Array2.get a i j = 0 then (
	let lenl = List.length (findImpossible a i j) in
	if lenl > len then frec (Some (i, j)) lenl (succ i) j
	else frec res len (succ i) j
      )
      else frec res len (succ i) j
    ) in
  frec None (-1) 0 0

(* on defini quelle methode est choisie *)
let findUnknownCoordinates = findUnknownCoordinates_min

(* solveur *)
let rec solveGrid fdo level a =
  (
    if debug > 0 && level mod 64 = 0 then Printf.fprintf fdo "# niveau = %d\n" level;
    match findUnknownCoordinates a with
      None -> 
      (* Printf.fprintf fdo "# possibilite (niveau %d):\n" level; *)
      dumpGrid fdo a;
      1
    | Some (i, j) ->
      let lst = findImpossible a i j in
      let lst = sComplement lst in
      if lst = [] then 0
      else List.fold_left (fun count n -> 
			Array2.set a i j n;
			let count = count + solveGrid fdo (succ level) a in
			Array2.set a i j 0;
			count
			)
		      0
		      lst
  )

(* emballage *)
let _ =
  let fdi = open_in Sys.argv.(1) in
  let fdo = stdout in
  let a = Array2.create int c_layout size size in
  readGrid fdi a;
  Printf.fprintf fdo "# grille traitee:\n";
  dumpGrid fdo a;
  let count = solveGrid fdo 0 a in
  Printf.fprintf fdo "# nb de solutions: %d\n" count



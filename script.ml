(**
*   Compression of binary search trees
*
*   @author:   Amine BENSLIMANE && Sofiane BRANECI
*   
*   Master 1 STL - Sorbonne Université
*       Jan 2021
*
*   https://github.com/bnslmn
* 
*   Licence GPLv3, feel free to use this code
*)


open Unix;;

(** PARTIE 1*)


(**
Renvoie un couple de listes dans lequel le r-ième(noté elt) element de L a été insert en tete de P
params
  l: list
  p: list
return
  (L-{elt}, {elt} UNION P)
*)

let extraction_alea l p =
  Random.self_init ();
  let rIndex =   Random.int (List.length l) in 
  let rec extractor l unused elt index = 
    match l with
    | [] -> (List.rev unused, elt :: p)
    | h::t -> if index = rIndex then extractor t unused h (index + 1) else extractor t (h::unused) elt (index + 1)

  in extractor l [] (-1) 0

;;

(**
* Génère des permutations aléatoires 
*
* Algorithme Shuffle de Fisher-Yates
*
params
  n: int
return    
  p: list des pemutations
*)
let gen_permutation n =
  let l = ref [] in
  for i=1 to (n) do
    l := !l @ [i]
  done;
  let rec aux l p =
    match l with
    |[] -> p
    |_ -> let (x,y) = (extraction_alea l p) in aux x y
  in aux !l []
          
;;
  
let intercal l p =
  Random.self_init ();
  let rec aux resultat l p n1 n2 =
  
    match (l,p) with
    |([],_) -> resultat@p
    |(_,[]) -> resultat@l
    |(h1::t1,h2::t2) -> if (Random.float 1.) < ((float_of_int n1)/.(float_of_int (n1+n2))) then
          aux (resultat@[h1]) t1 p (List.length t1) n2
        else
          aux (resultat@[h2]) l t2 n1 (List.length t2)
  in aux [] l p (List.length l) (List.length p);;


(** 
Génère des permutations en faisant appelle à intercal
param
    p: int
    q: int
return
List des permutations générées
*)
let rec gen_permutation2 p q =
  if p > q then
    []
  else if p = q then
    [p]
  else
    let l1 = gen_permutation2 p  ((p+q)/2) in 
    let l2 = gen_permutation2 (((p+q)/2) + 1) q in 
    intercal l1 l2
;;


(**
definition recursive du type arbe binaire de recherche polymorphique
Un arbre  | est soit une feuille
          | soit constitué d'un noeud racine, d'un sous arbre gauche et droit qui sont eux-même des arbres binaire de recherche
*)
type 'a tree = 
  | Empty
  | Tree of  'a * 'a tree * 'a tree * int;;


(** 
Definition du type d'arbre compressé
à utiliser dans la partie 2
*)
type 'a ctree = 
  | CEmpty
  | CTree of 'a * 'a ctree * 'a ctree * int
  | Compressed of 'a tree  * 'a array  
;;



(**
  Permet l'insertion d'une nouvelle cle dans l'arbre 
  params
   tree: ABR
   valeur: int
  return
  l'arbre avec la cle insert
*)
let rec insert tree key = 
  match tree with
  | Empty -> Tree(key, Empty, Empty, 1)
  | Tree(v, g, d, deg) -> if v < key then Tree(v, g, insert d key, deg + 1)
      else Tree(v, insert g key, d, deg + 1)
;;


(**
Construit l'arbre binaire de recherche à partir d'une Liste
parms
  tree: ABR, au début c'est une feuille (Empty)
  l: List
return
Un ABR dans qui contient toutes les cles de la liste l

*)
let rec construct_tree_from_list tree l = 
  match l with 
  | [] -> tree
  | h::t -> construct_tree_from_list (insert tree h) t
  
;;


(*let rec search tree key = 
  match tree with 
  | Empty -> false
  | Tree(v,g,d,_) -> if v = key then true else if v < key then search d key else search g key;; 
*)


(** PARTIE 2 *)

(** Helper functions *)


let get_degre tree = 
  match tree with
  | Empty -> 0
  | Tree(_,_,_,deg) -> deg
;; 


let get_left tree = 
  match tree with
  | Empty -> tree
  | Tree(_,g,_,_) -> g
;; 


let rec get_tree_with_key tree key = 
  match tree with 
  | Empty -> tree
  | Tree(v, g, d,_)-> if v = key then tree else if v < key then get_tree_with_key d key else get_tree_with_key g key;;

(** End *)

(** Implémentation de la fonction phi pour la construction des structures
  parms
    tree: ABR
  return 
  Chaîne de caractères associé à l'arbre en entrée
*)
let rec signature tree = 
  match tree with
  | Empty -> ""
  | Tree(_, g, d, _) -> "(" ^ (signature g) ^ ")" ^ signature d;;


(**
Construit le tableau associé a l'arbre dans le parcours prefix
params
  tree: ABR
return
  Array: contenant tout les éléments de l'arbre dans l'ordre prefix
*)
let prefix tree = 
  match tree with 
  | Empty -> Array.make 0 0
  | Tree(v, g, d, deg) ->
      let arr = Array.make deg 0 and index = ref 0 in 
      let rec aux tree arr index = 
        match tree with 
        | Empty
     -> ()
        | Tree(v, g, d, deg) -> 
            arr.(!index) <- v;
            index := (!index + 1);
            aux g arr index;
            aux d arr index;
      in
      aux tree arr index;
      arr
;;

(** 
Construit une list de tuplet dont chaqu'un contient la cle du noeud, phi(noeud), son degré 
params:
  tree: ABR
  l: list
return 
List de tuples dans l'ordre prefix
*)

let rec signature_builder tree l = 
  match tree with
  | Empty -> l
  | Tree(v, g, d, deg) -> let temp = (v, (signature tree), deg) in 
      let augmented = temp :: l in 
      augmented @ (signature_builder g l) @ (signature_builder d l)
;;



(** 
Construit l'arbre compressé
params
  tree: ABR 
  map: Hashmap, signature ---> noeud correspondant
return
  ABR de type CTree qui est l'arbre compressé
*)
let get_key tree = 
  match tree with 
  | Empty -> -1
  | Tree(v, _,_,_) -> v

let  tree_compressor tree =
  let map = Hashtbl.create 10 in (*init value, grows according to the number of elements*)
    let rec compressor tree map = 
      match tree with
    | Empty
 -> CEmpty
    | Tree(v, g, d, deg) -> 
        try 
          let s = signature tree in 
          let pointer = Hashtbl.find map s in 
          if deg > 1 then Compressed(pointer, (prefix tree))
          else Compressed(pointer, (Array.make 1 v))
        with Not_found ->
          Hashtbl.add map (signature tree) tree;
          let left = compressor g map in
          let right = compressor d map in 
          CTree(v, left, right, deg)
    in compressor tree map;;  


  
(** 
Recherche d'un élement dans l'arbre compressé
params
  tree: CTree
return
  bool
*)

let search_in_compressed ctree value =
  let rec find ctree value = 
    match ctree with 
    | CEmpty -> false
    | CTree(v, g, d, deg) -> if value = v then true else
        if v < value then find d value
        else find g value
    | Compressed(p, arr) -> 
        (*rechercher la valeur dans le tableau, 
        currentP represente le poiteur ayant la meme structure que l'arbre contenu dans le tableau dans l'ordre prefix
        *)
        let rec finder value arr index currentP =    
         (*print_int index;*)   
          try
            let current = (Array.get arr index) in 
            if current = value then true 
            else if current < value then finder value arr (index + 1 + (get_degre (get_left currentP))) (get_left currentP)
            else finder value arr (index + 1) currentP
          with Invalid_argument _ -> 
          (*print_string "bom\n";*)
          false
        in finder value arr 0 p
  in find ctree value 
    
;;

(** 
Recherche d'un élement dans l'arbre binaire de recherche
params
  tree: Tree
return
  bool
*)

let rec search_in_bst tree value = 
  match tree with 
  | Empty -> false
  | Tree(v,g,d, deg) -> 
          if v = value then true 
          else if v < value then search_in_bst d value else search_in_bst g value
  ;;


let counter tree = 
  let rec node_counter tree acc =
    match tree with
    | CEmpty -> acc
    | Compressed(_,_) -> acc
    | CTree(_,g,d,_) -> 1 + (node_counter g acc) + (node_counter d acc)
  in node_counter tree 0
;;


(** PARTIE 3 *)

let sizeof (x: 'a) : int = Obj.reachable_words (Obj.repr x)
;;


let timeit func tree key = 
  let t = Sys.time() in 
  let _ = func tree key in 
  Sys.time() -. t
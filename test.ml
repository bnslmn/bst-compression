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


(*this script is to be used to create files for plotting*)

open Script;;
Random.init 42;;
let append value l = 
 l := !l @ [value]
;;

let rec toStringInts l = 
  match l with 
  | [] -> ""
  | h :: t -> (string_of_int h) ^ "," ^ (toStringInts t) 
  ;;

let rec toStringFloats l = 
    match l with 
    | [] -> ""
    | h :: t -> (string_of_float h) ^ "," ^ (toStringFloats t) 
    ;;
  
let write_time l f = 
  let oc = open_out f in 
  Printf.fprintf oc "%s" (toStringFloats l)
  ;;

let write_size l f = 
    let oc = open_out f in 
    Printf.fprintf oc "%s" (toStringInts l)
    ;;
  
let number_of_nodes = [5; 10; 100; 1000; 2000; 3000;4000; 5000; 6000;  7000; 10000; 20000];;

let bst_time = ref [] and bst_size = ref [] and compressed_time = ref [] and compressed_size = ref [] in 
let test n = 
  
  let value = Random.int (3*(n + 1)) in 
  let list = gen_permutation2 0 (n+1) in 
  let tree = construct_tree_from_list Script.Empty list in 
  let compressed = tree_compressor tree in 
  let t1 = timeit search_in_bst tree value in
  let t2 = timeit search_in_compressed compressed  value in
  let s1 = sizeof tree in
  let s2 = sizeof compressed in
  Printf.fprintf stdout "Running test for n = %d\n" n;
  Printf.fprintf stdout "Time taken to search for %d in bst = %f, compressed = %f | Size of bst = %d, size of compressed = %d\n" value t1  t2 s1 s2;
  Printf.fprintf stdout "-----------------------\n";
  append t1 bst_time;
  append t2 compressed_time;
  append s1 bst_size;
  append s2 compressed_size;
in
List.iter test number_of_nodes;

write_time !bst_time "time_bst1.txt";
write_time !compressed_time "time_compressed1.txt";
write_size !bst_size "size_bst1.txt";
write_size !compressed_size "size_compressed1.txt";
true;;
 

 
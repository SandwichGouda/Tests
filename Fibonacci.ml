


let rec fibo_nulachier n = match n with
| 0 -> 0 
| 1 -> 1 
| n -> fibo_nulachier (n-1) + fibo_nulachier (n-2) ;;


let fibo_lineaire n = 
if n = 0 then 0 else if n = 1 then 1 else
let a = ref 0 in 
let b = ref 1 in 
let u = ref 0 in 	
for k = 1 to n do 
    u := !b ;
    b := !a + !b ;
    a := !u ;
done ; !b ;;

let fibo_lineaire_indices n = 
if n = 0 then 0,1 else if n = 1 then 1,1 else
let a = ref 0 in 
let b = ref 1 in 
let u = ref 0 in 
for k = 1 to n do 
    u := !b ;
    b := !a + !b ;
    a := !u ;
done ; n,!b ;;


let rec n_premiers_entiers m = 

let rec aux n = match n with 
| 1 -> [1]
| n -> n::(aux (n-1))

in List.rev (aux m) ;;

List.map fibo_lineaire_indices (n_premiers_entiers 100) ;;

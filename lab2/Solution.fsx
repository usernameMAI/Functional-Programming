// Solution

// Нахождение максимального модуля разности соседних элементов списка

// Method 1: Library Function
let find_max_abs_diff1 =
    function
    | l when List.length l <= 1 -> failwith "Error"
    | l -> (List.pairwise >> List.fold (fun x (a, b) -> max (abs (a - b)) x) 0) l


// Method 2: Recursion
let find_max_abs_diff2 =
    let rec solve ans prev list =
        match list with
        | [] -> ans
        | h :: t -> max ans (solve (abs (prev - h)) h t)

    function
    | []
    | [ _ ] -> failwith "Error"
    | h :: t -> solve 0 h t


// Method 3: Tail Rec
let find_max_abs_diff3 =
    let rec solve ans prev list =
        match list with
        | [] -> ans
        | h :: t -> solve (max ans (abs (prev - h))) h t

    function
    | []
    | [ _ ] -> failwith "Error"
    | h :: t -> solve 0 h t


let l = [ 0; 2; 3; -1 ]
printfn "%d" (find_max_abs_diff1 l)

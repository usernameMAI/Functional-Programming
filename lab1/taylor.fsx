let EPS = 0.0000001
let n = 10
let a = 0.0
let b = 0.5

let builtin_function x = (x * (3. - x)) / (pown (1. - x) 3)

let taylor_member x n = (float n) * (float n + 2.) * (pown x n)

let low_accuracy a b = abs ((-) a b) > EPS

let rec calculate_value f acc n prev =
    let current_member = f n prev
    let current_value = (+) current_member acc

    if low_accuracy current_value acc then
        calculate_value f current_value ((+) n 1) current_member
    else
        acc, n

let taylor_naive x =
    calculate_value (fun n _ -> taylor_member x n) 0. 1 0.

let taylor x =
    let first_member = taylor_member x 1

    calculate_value
        (fun i prev -> (x * (float i) * ((float i) + 2.) * prev / (((float i) - 1.) * ((float i) + 1.))))
        first_member
        2
        first_member

[<EntryPoint>]
let main args =
    printfn "%-10s%-15s%-15s%-10s%-15s%-10s" "x" "Builtin" "Smart Taylor" "terms" "Dumb Taylor" "terms"

    for i = 0 to n do
        let x = a + (float i) * ((a + b) / 10.)
        let dumb_taylor_value, dumb_taylor_terms = taylor_naive x
        let smart_taylor_value, smart_taylor_terms = taylor x

        printfn
            "%-10.2f%-15.5f%-15.5f%-10d%-15.5f%-10d"
            x
            (builtin_function x)
            dumb_taylor_value
            dumb_taylor_terms
            smart_taylor_value
            smart_taylor_terms

    0

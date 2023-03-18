let EPS = 0.0000001
let f1 x = 3. * ((log x) ** 2.) + 6. * log x - 5.
let f2 x = 0.6 * (3. ** x) - 2.3 * x - 3.
let f3 x = x ** 2. - log (1. + x) - 3.
let f1' x = 6. * log x / x + 6. / x
let f2' x = 0.6 * (3. ** x) - 2.3
let f3' x = 2. * x - 1. / (1. + x)
let phi1 x = exp ((1. / 6.) * (5. - 3. * ((log x) ** 2.)))
let phi2 x =  log ((2.3 * x + 3.) / 0.6) / log 3.
let phi3 x = sqrt (log (1. + x) + 3.)

let low_accuracy a b = abs ((-) a b) > EPS

let rec dichotomy f a b =
    let c = ((+) a b) / 2.

    if b - a < EPS then c
    else if (f b) * (f c) < 0. then dichotomy f c b
    else dichotomy f a c

let rec iterations phi x0 =
    let new_x = phi x0

    if low_accuracy x0 new_x then
        iterations phi new_x
    else
        new_x

let newthon f f' x0 =
    let phi x = x - (f x / f' x)
    iterations phi x0


[<EntryPoint>]
let main args =
    printfn "%-20s%-20s%-20s" "Метод дихотомии" "Метод итераций" "Метод Ньютона"
    printfn "%-20.5f%-20.5f%-20.5f" (dichotomy f1 1. 3.) (iterations phi1 1.) (newthon f1 f1' 1.)
    printfn "%-20.5f%-20.5f%-20.5f" (dichotomy f2 2. 3.) (iterations phi2 2.) (newthon f2 f2' 2.)
    printfn "%-20.5f%-20.5f%-20.5f" (dichotomy f3 2. 3.) (iterations phi3 2.) (newthon f3 f3' 3.)
    0

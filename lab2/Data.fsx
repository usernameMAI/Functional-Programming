// Part 2

// Load the data
#load "Four.fsx"

// Напечатайте среднюю оценку за каждый предмет
let task1 =
    Four.studs
    |> List.collect (fun (_, _, grades) -> grades)
    |> List.groupBy fst
    |> List.iter (fun x ->
        printf
            "%s: %f\n"
            (snd (List.find (fun sub -> fst sub = (fst x)) Four.subjs))
            (List.averageBy (snd >> float) (snd x)))

// Для каждой группы, напечатайте список студентов, заваливших сессию (хотя бы одна оценка = 2)
let task2 =
    Four.studs
    |> List.filter (fun (_, _, grades) -> List.exists (fun x -> snd x = 2) grades)
    |> List.groupBy (fun (n, _, _) -> n)
    |> List.iter (fun (n, l) ->
        printf "Группа %d: " n
        List.iter (fun (_, person, _) -> printf "%s " person) l
        printfn "")


// Для каждого студента, найдите среднюю оценку
let task3 =
    Four.studs
    |> List.iter (fun (_, name, grades) ->
        printfn "%s: %f" name (List.averageBy (fun grade -> (snd >> float) grade) grades))

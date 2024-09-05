open System


let f x = 0.5 * Math.Log(x)


let a = 0.2
let b = 0.7
let num = 10
let step = (float (b - a) / float num)
let eps = 1e-7


let naive_taylor x eps =
    let rec loop term n sum =
        if abs term < eps then (sum, n)
        else
            let newterm = (((x - 1.0) / (x + 1.0)) ** (float (2 * n + 1))) / (float (2 * n + 1))
            loop newterm (n + 1) (sum + newterm)
    let (result, terms) = loop ((x - 1.0) / (x + 1.0)) 1 ((x - 1.0) / (x + 1.0))
    (2.0 * result, terms)


let taylor x =
    let rec loop term n sum =
        if abs term < eps then (sum, n)
        else
            let newTerm = -term * ((x - 1.0) / (x + 1.0)) * ((x - 1.0) / (x + 1.0))
            loop newTerm (n + 1) (sum + newTerm / (float (2 * n + 1)))
    let (result, terms) = loop ((x - 1.0) / (x + 1.0)) 1 ((x - 1.0) / (x + 1.0))
    (2.0 * result, terms)


let table a b step eps =
    printfn "%-10s %-12s %-15s %-8s %-15s %-10s" "x" "Builtin" "Smart Taylor" "terms" "Dumb Taylor" "terms"
    let rec loop x =
        if x <= b then
            let builtin = f x
            let (smart, smartTerms) = taylor x 
            let (dumb, dumbTerms) = naive_taylor x eps
            printfn "%-10f %-12f %-15f %-8d %-15f %-10d" x builtin smart smartTerms dumb dumbTerms
            loop (x + step)
    loop a


let main =
    table a b step eps

main

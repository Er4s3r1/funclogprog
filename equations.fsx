open System 

let eps = 1e-6

let rec dichotomy f a b =
  let c = (a + b) / 2.0
  if abs (b - a) < eps then c
  else
    if f a * f c < 0.0 then dichotomy f a c
    else dichotomy f c b

let rec iteration phi x0 =
  let nextx = phi x0
  if abs (nextx - x0) < eps then nextx
  else iteration phi nextx

let newthon f f' x0 = 
  iteration (fun x -> x - ((f x) / (f' x))) x0

let f1 x = 0.25 * x ** (3.0) + x - 1.2502
let f2 x= x + sqrt x + x ** (1.0/3.0) - 2.5
let f3 x= x - (1.0 / (3.0 + sin (3.6 * x)))

let phi1 x = 1.2502 - 0.25 * x ** (3.0)
let phi2 x=  -1.0 * (sqrt x + x ** (1.0 / 3.0) - 2.5) 
let phi3 x= 1.0 / (3.0 + sin (3.6 * x))

let f1' x = ((3.0 / 4.0) * x ** 2.0) + 1.0
let f2' x= (3.0 * (x ** (2.0 / 3.0) + 2.0 * sqrt x ) )/ (6.0 * x ** (7.0 / 6.0)) + 1.0 
let f3' x= (18.0 * cos((18.0 * x)/5. ) ) / (5.0 *(sin((18.0 * x) / 5.0) ** 2.0 + 6.0 * sin((18.0 * x) / 5.0) + 9.0)) + 1.0


let main = 
  printfn "%-10s  %-10s  %-10s" "Dichotomy" "Iterarion" "Newthon"
  printfn "%-10f  %-10f  %-10f" (dichotomy f1  0.0 2.0) (iteration phi1 1.0) (newthon f1 f1' 0.0)
  printfn "%-10f  %-10f  %-10f" (dichotomy f2  0.4 1.0) (iteration phi2 0.7) (newthon f2 f2' 0.4)
  printfn "%-10f  %-10f  %-10f" (dichotomy f3  0.0 0.85) (iteration phi3 0.425) (newthon f3 f3' 0.0)

main


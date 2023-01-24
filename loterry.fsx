open System

(* 
    Assumption:

    For simplicity sake lets say that we do not
    take the multiplier into account.
*)

(*
    5 White Balls + Powerball = Grandprize
    5 White Balls             = $1 Million
    4 White Balls + Powerball = $50,000
    4 White Balls             = $100
    3 White Balls + Powerball = $100
    3 White Balls             = $7
    2 White Balls + Powerball = $7
    1 White Ball  + Powerball = $4
    Powerball                 = $4
*)

(*
    White Balls range from (1-69)
    Red Balls range from (1-26)
*)

(*
    Simmulating:
    
    How many drawings does it take for me to win the jackpot?
    What is the ROI?
*)

(*
    Generating random values with the new random functionality.
*)

Random.Shared.Next(1, 70)// Generates numbers 1 to 69
Random.Shared.Next(1,27) // Generates numbers 1 to 26

type WhiteBall = WhiteBall of int
type PowerBall = Powerball of int

type LotteryCombination = {
    WhiteBalls: WhiteBall list
    PowerBall: PowerBall
}

open System

let genUnique picks = 
    let mutable next = Random.Shared.Next(1, 70)
    while List.contains next picks do
        next <- Random.Shared.Next(1, 70)
    next

let generator state = 
    match state with
    | [] -> 
        let next = Random.Shared.Next(1, 70)
        Some (next, [next])
    | [_;_;_;_;_] -> 
        None
    | picks -> 
        let next = genUnique picks
        Some (next, next :: picks)

let generateLottery() = 
    []
    |> List.unfold generator
    |> List.sort
    |> List.map ( fun x -> WhiteBall x)


let findCorrectWhiteBalls lotteryPick drawingResult  =
    Set.intersect 
        (Set.ofList lotteryPick.WhiteBalls) 
        (Set.ofList drawingResult.WhiteBalls) 
    |> Set.toList

let checkPowerball lotteryPick drawingResult=
    if lotteryPick.PowerBall = drawingResult.PowerBall then
        Some(lotteryPick.PowerBall)
    else
        None

let scoreLotteryPick whiteBalls powerball grandprize  =
    let numOfWhiteBalls = whiteBalls |> List.length
    match (numOfWhiteBalls,powerball)  with
    | 5,Some(_) -> grandprize
    | 5,None -> 1_000_000
    | 4,Some(_) -> 50_000
    | 4,None -> 100
    | 3,Some(_) -> 100
    | 3,None -> 7
    | 2,Some(_) -> 7
    | 2,None -> 0
    | 1,Some(_) -> 4
    | 1,None -> 0
    | 0,Some(_) -> 4
    | 0,None -> 0
    | _ -> failwithf $"I have not found this PowerBall Cases yet: {numOfWhiteBalls}; {powerball}"

let scorePowerBall lotteryPick drawingResult grandprize = 
    let matchingwhiteBalls = findCorrectWhiteBalls lotteryPick drawingResult
    let matchingpowerBall = checkPowerball lotteryPick drawingResult
    scoreLotteryPick matchingwhiteBalls matchingpowerBall grandprize

let lotteryPick =
    {
        WhiteBalls = [WhiteBall 14; WhiteBall 33; WhiteBall 43; WhiteBall 60; WhiteBall 67]
        PowerBall = Powerball 7
    }

let drawPowerballs() =
    {
        WhiteBalls = generateLottery()
        PowerBall = Powerball(Random.Shared.Next(1,27))
    }

// scorePowerBall lotteryPick (drawingPowerballs()) 20_000_000


(*
    14 33 43 60 67 7
*)

let mutable lotteryTickets: int = 0
let mutable profit: int = 0
let mutable loss: int = 0
let mutable ticketsBought: int = 0
let mutable x = 0
let mutable ammountWon = 0

let grandprize = 1_000_000_000

while ammountWon <> grandprize do
    let drawingResults = drawPowerballs()
    let matchingwhiteBalls = findCorrectWhiteBalls lotteryPick drawingResults
    let matchingpowerBall = checkPowerball lotteryPick drawingResults
    ammountWon <- scoreLotteryPick matchingwhiteBalls matchingpowerBall grandprize
    lotteryTickets <- lotteryTickets + 1
    loss <- loss + 2
    profit <- profit + ammountWon
    x <- x + 1

printfn $"Tickets Purchased: ${lotteryTickets}"
printfn $"Profit: ${profit}"
printfn $"Loss: ${loss}"
printfn $"Net Profit: ${profit - loss}"

(*
    Create a function that takes in a lottery pick
    and Counts how many numbers are correct in the
    ball picks.
*)
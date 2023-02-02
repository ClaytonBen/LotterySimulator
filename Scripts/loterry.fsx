(*
    Simulating:
    
    How many drawings does it take for me to win the jackpot?
    What is the ROI?
*)
 
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
    Generating random values with the new random functionality.
*)
open System

Random.Shared.Next(1, 70)// Generates numbers 1 to 69
Random.Shared.Next(1,27) // Generates numbers 1 to 26

type WhiteBall = WhiteBall of int
type PowerBall = Powerball of int

[<Struct>]
type LotteryCombination = {
    WhiteBalls: WhiteBall Set
    PowerBall: PowerBall
}

let generateWhiteBalls n =
    let rec generateRandomNumbers' n set =
        if set |> Set.count = n then set
        else 
            generateRandomNumbers' n (Set.add (WhiteBall (Random.Shared.Next(1, 70))) set)
    generateRandomNumbers' n Set.empty

generateWhiteBalls 5

// This will not need to be here.
// When I use a set I don't need to go back to lists.
let findCorrectWhiteBalls lotteryPick drawingResult  =
    Set.intersect lotteryPick.WhiteBalls drawingResult.WhiteBalls

// Let lotteryP
let checkPowerball lotteryPick drawingResult=
    lotteryPick.PowerBall = drawingResult.PowerBall

// Change to true false
let scoreLotteryPick whiteBalls powerball grandprize  =
    let numOfWhiteBalls = whiteBalls |> Set.count
    match (numOfWhiteBalls,powerball)  with
    | 5,true    -> grandprize
    | 5,false   -> 1_000_000
    | 4,true    -> 50_000
    | 4,false   -> 100
    | 3,true    -> 100
    | 3,false   -> 7
    | 2,true    -> 7
    | 2,false   -> 0
    | 1,true    -> 4
    | 1,false   -> 0
    | 0,true    -> 4
    | 0,false   -> 0
    | _ -> failwithf $"I have not found this PowerBall Cases yet: {numOfWhiteBalls}; {powerball}"

// let scorePowerBall lotteryPick drawingResult grandprize = 
//     let matchingwhiteBalls = findCorrectWhiteBalls lotteryPick drawingResult
//     let matchingpowerBall = checkPowerball lotteryPick drawingResult
//     scoreLotteryPick matchingwhiteBalls matchingpowerBall grandprize

let lotteryPick =
    {
        WhiteBalls = Set.ofList [WhiteBall 14; WhiteBall 33; WhiteBall 43; WhiteBall 60; WhiteBall 67]
        PowerBall = Powerball 7
    }

let drawLottery() =
    {
        WhiteBalls = generateWhiteBalls 5
        PowerBall = Powerball(Random.Shared.Next(1,27))
    }

// scorePowerBall lotteryPick (drawingPowerballs()) 20_000_000

let addCommas (num: int) =
    let isNegative = num < 0
    let numString = Math.Abs(num).ToString()
    let reversed = numString.ToCharArray() |> Array.rev
    let chunks = reversed |> Array.chunkBySize 3
    
    let numberWithCommas =
        chunks
        |> Array.map Array.rev
        |> Array.rev
        |> Array.map ( fun x -> 
            x 
            |> Array.map string 
            |> String.concat "")
        |> String.concat ","

    if isNegative then $"-{numberWithCommas}"
    else numberWithCommas
    
addCommas -342_433

let x = -1000
let s = $"{x:n0}"

let num = 600344
let numString = num.ToString()
let reversed = numString.ToCharArray() |> Array.rev
let chunks = reversed |> Array.chunkBySize 3
chunks
|> Array.map Array.rev
|> Array.rev
|> Array.map ( fun x -> 
    x 
    |> Array.map string 
    |> String.concat "")
|> String.concat ","
(*
    14 33 43 60 67 7
*)

// Record Result type
type LotterySimResults ={
    TicketsBought: int
    Profit: int
    Loss: int
}


let simulatePowerBall lotteryPick grandprize = 
    let mutable lotteryTickets: int = 0
    let mutable profit: int = 0
    let mutable loss: int = 0
    let mutable ammountWon = 0
    let mutable jackpotHit = false

    let grandprize = 1_000_000_000

    // PowerBall hit While not powerball hit do. Set that to true when I hit powerball
    while jackpotHit = false do
        let drawingResults = drawLottery()
        let matchingwhiteBalls = findCorrectWhiteBalls lotteryPick drawingResults
        let matchingpowerBall = checkPowerball lotteryPick drawingResults
        ammountWon <- scoreLotteryPick matchingwhiteBalls matchingpowerBall grandprize
        if ammountWon = grandprize then
            jackpotHit <- true
        lotteryTickets <- lotteryTickets + 1
        loss <- loss + 2
        profit <- profit + ammountWon
        lotteryTickets <- lotteryTickets + 1
        if (lotteryTickets % 1_000_000) = 0 then 
            printfn "Number of Draws: %s Net Profit: $%s"(lotteryTickets|>addCommas)((profit-loss)|>addCommas)
    {
        TicketsBought = lotteryTickets
        Profit = profit
        Loss = loss
    }


let simResults = simulatePowerBall lotteryPick 10_000_000

printfn $"Tickets Purchased: {simResults.TicketsBought |>addCommas}"
printfn $"Profit: ${simResults.Profit |> addCommas}"
printfn $"Loss: ${simResults.Loss |> addCommas}"
printfn $"Net Profit: ${(simResults.Profit - simResults.Loss) |>addCommas}"


(*
    Create a function that takes in a lottery pick
    and Counts how many numbers are correct in the
    ball picks.
*)

let parseWhiteballInputs (str:string) =
    str.Split(",")
    |> Set.ofArray
    |> Set.map (fun x -> x |> int |> WhiteBall)
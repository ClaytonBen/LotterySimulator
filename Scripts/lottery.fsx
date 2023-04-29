(*
    Goal:
    
    How many drawings does it take for me to win the Powerball Jackpot?
    What is the Net Profit?
*)
 
(*
    How to Play:

    5 unique White Balls range from (1-69)
    1 PowerBall ranging from (1-26)
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


// Sketch out our Domain

type WhiteBall = WhiteBall of int
type PowerBall = PowerBall of int


type LotteryCombination = {
    WhiteBalls: WhiteBall Set
    PowerBall: PowerBall
}




(*
    Generating random values with the new random functionality.
*)

open System

Random.Shared.Next(1,70)
Random.Shared.Next(1,27)








  
// We want create a function that generates whiteballs
// Input: n number of balls that are generated.
// Recursive function that takes 

let rec generateRandomNumbers n set =
        if set |> Set.count = n then set
        else 
            generateRandomNumbers n (Set.add (WhiteBall (Random.Shared.Next(1, 70))) set)

generateRandomNumbers 5 Set.empty






// Create a function that takes n as an input
// Generates n number of balls with

let generateWhiteBalls n =
    let rec generateRandomNumbers n set =
        if set |> Set.count = n then set
        else 
            generateRandomNumbers n (Set.add (WhiteBall (Random.Shared.Next(1, 70))) set)
    generateRandomNumbers n Set.empty



generateWhiteBalls 5

// I need to find the lottery balls that are matched

let set1 =
    [1;3;6]
    |> Set.ofList

let set2 =
    [1;4;3]
    |> Set.ofList

Set.intersect set1 set2






// Will this work with types?

let setType =
    [WhiteBall 1;WhiteBall 3;WhiteBall 6]
    |> Set.ofList

let setType2 =
    [WhiteBall 1;WhiteBall 4;WhiteBall 3]
    |> Set.ofList

Set.intersect setType setType2




// Create a function that takes lotteryPick and the Drawing Result
// Returns the matched balls


let checkPowerball (lotteryPick:LotteryCombination) (drawingResult: LotteryCombination) =
    lotteryPick.PowerBall = drawingResult.PowerBall

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
let findCorrectWhiteBalls lotteryPick drawingResult  =
    Set.intersect lotteryPick.WhiteBalls drawingResult.WhiteBalls

// Scoring the lottery Pick

let scoreLotteryPick matchedWhiteBalls matchedPowerball grandprize  =
    let numOfWhiteBalls = matchedWhiteBalls |> Set.count
    match (numOfWhiteBalls,matchedPowerball)  with
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
    | _ -> failwithf $"I have not found this PowerBall Cases yet: {numOfWhiteBalls}; {matchedPowerball}"

let drawLottery() =
    {
        WhiteBalls = generateWhiteBalls 5
        PowerBall = PowerBall(Random.Shared.Next(1,27))
    }

let lotteryDrawn = drawLottery()






// Add Commas for visibility
let addCommas (num: int) =
    $"{num:n0}"

addCommas 342_433

// Will This work on negative numbers

addCommas -342_433




// Simulating the Powerball.


type LotterySimResults ={
    TicketsBought: int
    Profit: int
    Loss: int
}

let simulatePowerBall (lotteryPick:LotteryCombination) (grandprize:int) :LotterySimResults = 
    let rec simulate (lotteryTickets:int) (profit:int) (loss:int) =
        let drawingResults = drawLottery()
        let matchingwhiteBalls = findCorrectWhiteBalls lotteryPick drawingResults
        let matchingpowerBall = checkPowerball lotteryPick drawingResults
        let ammountWon = scoreLotteryPick matchingwhiteBalls matchingpowerBall grandprize

        let newLotteryTickets = lotteryTickets + 1
        let newProfit = profit + ammountWon
        let newLoss = loss + 2

        if (newLotteryTickets % 1_000_000) = 0 then 
            printfn "Number of Draws: %s Net Profit: $%s"(newLotteryTickets|>addCommas)((newProfit-newLoss)|>addCommas)

        if ammountWon = grandprize then
            {
                TicketsBought = newLotteryTickets
                Profit = newProfit
                Loss = newLoss
            }
        else
            simulate (newLotteryTickets) (newProfit) (newLoss)
    simulate 0 0 0


// Inputs
let jackpot = 100_000_000
 
let lotteryPick =
    {
        WhiteBalls = Set.ofList [WhiteBall 14; WhiteBall 33; WhiteBall 43; WhiteBall 60; WhiteBall 67]
        PowerBall = PowerBall 7
    }

let simResults  = simulatePowerBall lotteryPick jackpot


printfn $"Jackpot: {jackpot |>addCommas}"
printfn $"Tickets Purchased: {simResults.TicketsBought |>addCommas}"
printfn $"Profit: ${simResults.Profit |> addCommas}"
printfn $"Loss: ${simResults.Loss |> addCommas}"
printfn $"Net Profit: ${(simResults.Profit - simResults.Loss) |>addCommas}"


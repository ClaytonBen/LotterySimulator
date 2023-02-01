open LotteryLib.Engine
open LotteryLib.Domain
open System

let parseWhiteballInputs (str:string) =
    str.Split(",")
    |> Set.ofArray
    |> Set.map (fun x -> x |> int |> WhiteBall)

[<EntryPoint>]
printfn $"${100000 |>addCommas}"
Console.Write "\nPick 5 Whiteballs comma seperated (1-69):"
let whiteballInput = Console.ReadLine()
Console.Write "\nPick 1 Powerball (1-26):"
let powerBall = Console.ReadLine()
Console.Write "\nWhat is the Jackpot?:"
let jackpot = Console.ReadLine() |> int

let lotteryPick:LotteryCombination =
    {
        WhiteBalls = parseWhiteballInputs whiteballInput
        PowerBall = Powerball (powerBall |> int)
    } 

let simResults  = simulatePowerBall lotteryPick jackpot

printfn $"Jackpot: {jackpot |>addCommas}"
printfn $"Tickets Purchased: {simResults.TicketsBought |>addCommas}"
printfn $"Profit: ${simResults.Profit |> addCommas}"
printfn $"Loss: ${simResults.Loss |> addCommas}"
printfn $"Net Profit: ${(simResults.Profit - simResults.Loss) |>addCommas}"


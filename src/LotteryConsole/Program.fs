open LotteryLib.Engine
open LotteryLib.Domain

let lotteryPick:LotteryCombination =
    {
        WhiteBalls = Set.ofList [WhiteBall 14; WhiteBall 33; WhiteBall 43; WhiteBall 60; WhiteBall 67]
        PowerBall = Powerball 7
    } 
let simResults = simulatePowerBall lotteryPick 100_000_000

[<EntryPoint>]

printfn $"Tickets Purchased: {simResults.TicketsBought |>addCommas}"
printfn $"Profit: ${simResults.Profit |> addCommas}"
printfn $"Loss: ${simResults.Loss |> addCommas}"
printfn $"Net Profit: ${(simResults.Profit - simResults.Loss) |>addCommas}"


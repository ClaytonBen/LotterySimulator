open LotteryLib.Engine
open LotteryLib.Domain

let lotteryPick =
    {
        WhiteBalls = Set.ofList [WhiteBall 14; WhiteBall 33; WhiteBall 43; WhiteBall 60; WhiteBall 67]
        PowerBall = Powerball 7
    }

[<EntryPoint>]
let simResults = simulatePowerBall lotteryPick 100_000_000

printfn $"Tickets Purchased: {simResults.TicketsBought |>addCommas}"
printfn $"Profit: ${simResults.Profit |> addCommas}"
printfn $"Loss: ${simResults.Loss |> addCommas}"
printfn $"Net Profit: ${(simResults.Profit - simResults.Loss) |>addCommas}"


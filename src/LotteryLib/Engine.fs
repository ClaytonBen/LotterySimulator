namespace LotteryLib

open Domain
open System

module Engine =

    let generateWhiteBalls n =
        let rec generateRandomNumbers' n set =
            if set |> Set.count = n then set
            else 
                generateRandomNumbers' n (Set.add (WhiteBall (Random.Shared.Next(1, 70))) set)
        generateRandomNumbers' n Set.empty

    let findCorrectWhiteBalls lotteryPick drawingResult  =
        Set.intersect lotteryPick.WhiteBalls drawingResult.WhiteBalls

    let checkPowerball lotteryPick drawingResult=
        lotteryPick.PowerBall = drawingResult.PowerBall
    
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

    let drawLottery() =
        {
            WhiteBalls = generateWhiteBalls 5
            PowerBall = Powerball(Random.Shared.Next(1,27))
        }

    let addCommas (num: int) =
        $"{num:n0}"

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
                    TicketsBought = lotteryTickets
                    Profit = profit
                    Loss = loss
                }
            else
                simulate (newLotteryTickets) (newProfit) (newLoss)
        simulate 0 0 0
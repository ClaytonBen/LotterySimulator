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

    // Let lotteryP
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
        let numString = num.ToString()
        let reversed = numString.ToCharArray() |> Array.rev
        let chunks = reversed |> Array.chunkBySize 3
        chunks
        |> Array.map Array.rev
        |> Array.rev
        |> Array.map ( fun x -> 
            x 
            |> Array.map string 
            |> String.concat ""
            )
        |> String.concat ","
    
    let simulatePowerBall (lotteryPick:LotteryCombination) (grandprize:int) :LotterySimResults = 
        let mutable lotteryTickets: int = 0
        let mutable profit: int = 0
        let mutable loss: int = 0
        let mutable ammountWon = 0
        let mutable jackpotHit = false

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

    


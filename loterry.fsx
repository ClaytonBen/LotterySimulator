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

let lotteryPick =
    {
        WhiteBalls = generateLottery()
        PowerBall = Powerball(Random.Shared.Next(1,27))
    }

let drawingResults =
    {
        WhiteBalls = generateLottery()
        PowerBall = Powerball(Random.Shared.Next(1,27))
    }

let findCorrectBalls lotteryPick drawingResults  =
    Set.intersect 
        (Set.ofList lotteryPick.WhiteBalls) 
        (Set.ofList drawingResults.WhiteBalls) 
    |> Set.toList

findCorrectBalls lotteryPick drawingResults
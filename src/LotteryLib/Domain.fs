namespace LotteryLib

module Domain =
    type WhiteBall = WhiteBall of int
    type PowerBall = Powerball of int

    [<Struct>]
    type LotteryCombination = {
        WhiteBalls: WhiteBall Set
        PowerBall: PowerBall
    }
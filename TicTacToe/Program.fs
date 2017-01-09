// En savoir plus sur F# sur le site http://fsharp.org
// Voir le projet 'Didacticiel F#' pour obtenir de l'aide.
open System
open System.Linq

type Symbol =
    | Cross
    | Circle
    member this.Print() =
        match this with
            | Cross -> 'X'
            | Circle -> 'O'

type Position = {
    X: int;
    Y: int;
}

type Move = {
    Position: Position
    Symbol: Symbol
}

type Board(array:Symbol option[]) =
    let mutable array = array
    let rowSize = 3

    let get x y =         
        array.[x + y * rowSize]

    let getLines() =
        match array with
        | [| a; b; c; d; e; f; g; h; i;|] -> [(a, b, c); (d, e, f); (g, h, i); (a, d, g); (b, e, h); (c, g, i); (a, e, i); (c, e, g)]
        | _ -> failwith ""

    let isLineWinForPlayer symbol line =
        let (a, b, c) = line
        not(Option.isNone a) && not(Option.isNone b) && not(Option.isNone c) && 
            symbol = Option.get a && Option.get a = Option.get b && Option.get b = Option.get c

    let printPlayer(symbol:Symbol option) = 
        match symbol with
        | None -> ' '
        | Some(value) -> value.Print()  

    new() = Board(Array.create 9 None)

    member this.Move(move:Move) = 
        let position = move.Position.X + move.Position.Y * rowSize
        if Option.isSome array.[position] then
            failwith "Invalid move"
        let newArray = Array.copy array
        Array.set newArray position (Some move.Symbol)
        new Board(newArray)

    member this.IsPlayerWin symbol =
        getLines().Any(fun line -> isLineWinForPlayer symbol line)

    member this.IsEndOfGame() = 
        (this.IsPlayerWin Cross) || this.IsPlayerWin Circle || this.IsFull()
        
    member this.IsFull() =
        array.All(fun e -> Option.isSome e)

    member this.GetAllPossibleMoves() =
        seq {
            for i in 0..array.Length - 1 do
            if (Option.isNone array.[i]) then
                yield { X = i % rowSize; Y = (int)(i / rowSize) }
        }

    member this.Evaluate (symbol:Symbol) =
        let otherPlayerSymbol = if symbol = Circle then Cross else Circle
        if this.IsPlayerWin symbol then
            10
        else if this.IsPlayerWin otherPlayerSymbol then
            -10
        else
            5            

    member this.Print() =
        printfn ""
        for i in 0..rowSize - 1 do
            printf "  "
            for j in 0..rowSize - 1 do
                printf "%c" (printPlayer (get i j))
                if j <> rowSize - 1 then
                    printf " | "
            printfn ""
            if i <> rowSize - 1 then
                printfn " -----------"
        printfn ""
    
type IPlayer =
    abstract member Symbol : Symbol with get
    abstract member GetMove : board:Board -> Move

type ConsolePlayer(symbol:Symbol) =
    let _symbol = symbol

    let rec getMoveInternal board = 
        printfn "Your move, Player %c (row,col) :" (_symbol.Print())
        try
            let moveConsole = Console.ReadLine().Trim().Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries)
            {
                Position = { X = (Int32.Parse (moveConsole.[0].Trim())) - 1; Y = (Int32.Parse (moveConsole.[1].Trim())) - 1 };
                Symbol = _symbol
            }
        with
            | _ -> getMoveInternal board

    interface IPlayer with
        member val Symbol = _symbol
        member this.GetMove(board) = getMoveInternal board
        
        
 type AIPlayer(symbol:Symbol) =
    let _symbol = symbol

    let rec getMoveInternal (board:Board) (move:Move) (playerSymbol:Symbol) = 
        let boardWithMove = board.Move move
        let possibleMoves = boardWithMove.GetAllPossibleMoves()

        if not(possibleMoves.Any()) then
            (boardWithMove.Evaluate(playerSymbol), move)
        else
            let newPlayerSymbol = if playerSymbol = Circle then Cross else Circle
            let evaluatedMoves = seq {
                for position in possibleMoves do
                let move = { Position = position; Symbol = playerSymbol }
                yield getMoveInternal boardWithMove move newPlayerSymbol
            }
            
            let minMax = if newPlayerSymbol = _symbol then min else max
            let mutable bestEvaluatedMove = evaluatedMoves.First()
            for currentEvaluatedMove in evaluatedMoves do
                if (minMax (fst bestEvaluatedMove) (fst currentEvaluatedMove)) <> (fst bestEvaluatedMove) then
                    bestEvaluatedMove <- currentEvaluatedMove
            bestEvaluatedMove
            

    interface IPlayer with
        member val Symbol = _symbol
        member x.GetMove(board: Board): Move = 
            let position = board.GetAllPossibleMoves().First()
            printfn "Playing %A %A" (position.X + 1) (position.Y + 1)
            {
                Position = position;
                Symbol = _symbol
            }       
           

type Game(player1:IPlayer, player2:IPlayer) =
    let player1 = player1
    let player2 = player2
    let mutable board = new Board()
    let mutable currentPlayer = player1
    let rec makePlayerMove() =
        try
            let move = currentPlayer.GetMove(board)
            board <- board.Move(move)
        with
            _ -> makePlayerMove()

    member this.Play = 
        board.Print()
        while not(board.IsEndOfGame()) do
            makePlayerMove()
            board.Print()
            currentPlayer <- if currentPlayer = player1 then player2 else player1
        printfn "End game"
        

[<EntryPoint>]
let main argv = 
    let g = new Game(new ConsolePlayer(Circle), new AIPlayer(Cross))
    g.Play
    ignore(Console.ReadLine())
    0 // retourne du code de sortie entier

open System
open System.Linq

type Symbol = 
    | Cross
    | Circle
    member this.GetOtherPlayerSymbol() =
        match this with
        | Cross -> Circle
        | Circle -> Cross
    override this.ToString() = 
        match this with
        | Cross -> "X"
        | Circle -> "O"

type Position = 
    { X : int
      Y : int }
    override this.ToString() =
        this.X.ToString() + "," + this.Y.ToString()

type Move = 
    { Position : Position
      Symbol : Symbol }

// Immutable board
type Board(array : Symbol option []) = 
    let mutable array = array
    let get x y = array.[x + y * Board.RowSize]
    
    //Get all possible lines on the board
    let getLines() = 
        match array with
        | [| a; b; c; d; e; f; g; h; i |] -> 
            [ (a, b, c)
              (d, e, f)
              (g, h, i)
              (a, d, g)
              (b, e, h)
              (c, f, i)
              (a, e, i)
              (c, e, g) ]
        | _ -> failwith ""
    
    // Determine if a line contains three of the specified symbol
    let isLineWinForPlayer symbol line = 
        let (a, b, c) = line
        not (Option.isNone a) && not (Option.isNone b) && not (Option.isNone c) && 
        symbol = Option.get a && Option.get a = Option.get b && Option.get b = Option.get c
    
    let cellToString (symbol : Symbol option) = 
        match symbol with
        | None -> " "
        | Some(value) -> value.ToString()
    
    new() = Board(Array.create (Board.RowSize * Board.RowSize) None)

    static member RowSize 
        with private get () = 3
    
    // returns a new board with the move played
    member this.Move(move : Move) = 
        let position = move.Position.X + move.Position.Y * Board.RowSize
        if Option.isSome array.[position] then 
            failwith "Invalid move"
        let newArray = Array.copy array
        Array.set newArray position (Some move.Symbol)
        new Board(newArray)
    
    // check if the player won the game
    member this.IsPlayerWin symbol = getLines().Any(fun line -> isLineWinForPlayer symbol line)
    // check if the game is over
    member this.IsEndOfGame() = (this.IsPlayerWin Cross) || (this.IsPlayerWin Circle) || this.IsFull()
    // check if the board is full
    member this.IsFull() = array.All(fun e -> Option.isSome e)
    
    // returns all playable moves
    member this.GetAllPossibleMoves() = 
        seq { 
            for i in 0..array.Length - 1 do
                if (Option.isNone array.[i]) then 
                    yield { X = i % Board.RowSize
                            Y = (int) (i / Board.RowSize) }
        }
    
    // prints the board
    member this.Print() = 
        printfn ""
        for i in 0..Board.RowSize - 1 do
            printf "  "
            for j in 0..Board.RowSize - 1 do
                printf "%s" (cellToString (get i j))
                if j <> Board.RowSize - 1 then printf " | "
            printfn ""
            if i <> Board.RowSize - 1 then printfn " -----------"
        printfn ""

// Player interface
type IPlayer = 
    abstract Symbol : Symbol
    abstract GetMove : board:Board -> Move

// Human player using the console
type ConsolePlayer(symbol : Symbol) = 
    let _symbol = symbol
    
    interface IPlayer with
        member this.Symbol
            with get() = _symbol

        member this.GetMove(board) = 
            let rec getMoveInternal board = 
                printf "Enter your move (row, col) : "
                try 
                    let moveConsole = Console.ReadLine().Trim().Split([| ' '; ',' |], StringSplitOptions.RemoveEmptyEntries)
                    { Position = 
                          { X = (Int32.Parse(moveConsole.[0].Trim())) - 1
                            Y = (Int32.Parse(moveConsole.[1].Trim())) - 1 }
                      Symbol = _symbol }
                with _ -> 
                    printfn "Invalid move"
                    getMoveInternal board

            getMoveInternal board

// Computer player
type AIPlayer(symbol : Symbol) = 
    let _symbol = symbol

    // compute the score of the board for the specified player
    let evaluateBoard (board : Board) (playerSymbol : Symbol) (depth : int) =
        let otherPlayerSymbol = playerSymbol.GetOtherPlayerSymbol()

        // very simple scores since we know the AIPlayer will explore the complete tree of possibilities
        if board.IsPlayerWin symbol then 
            100 - depth
        else if board.IsPlayerWin otherPlayerSymbol then 
            -100 + depth
        else 
            0

    // main AI logic
    let rec getMoveInternalFull (board : Board) (move : Move) (playerSymbol : Symbol) (depth : int) = 
        let boardWithMove = board.Move move
        if boardWithMove.IsEndOfGame() then 
            ((evaluateBoard boardWithMove playerSymbol depth), move)
        else 
            let newPlayerSymbol = playerSymbol.GetOtherPlayerSymbol()
            fst (getMoveInternal boardWithMove newPlayerSymbol (depth + 1))

    and getMoveInternal (board : Board) (playerSymbol : Symbol) (depth : int) = 
        let possibleMoves = board.GetAllPossibleMoves()
        
        let evaluatedMoves = 
            seq { 
                for position in possibleMoves do
                    let move = 
                        { Position = position
                          Symbol = playerSymbol }
                    yield (getMoveInternalFull board move playerSymbol depth), move
            }
        
        let minMax = 
            if playerSymbol = _symbol then max
            else min
        
        let mutable bestEvaluatedMove = evaluatedMoves.First()
        for currentEvaluatedMove in evaluatedMoves do
            if (minMax (fst bestEvaluatedMove) (fst currentEvaluatedMove)) <> (fst bestEvaluatedMove) then 
                bestEvaluatedMove <- currentEvaluatedMove
        bestEvaluatedMove
    
    interface IPlayer with
        member this.Symbol 
            with get() = _symbol
        member this.GetMove(board : Board) = 
            printfn "Thinking..."
            let move = snd (getMoveInternal board _symbol 0)
            printfn "Playing %O" move.Position
            move

// Game engine
type Game(player1 : IPlayer, player2 : IPlayer) = 
    let player1 = player1
    let player2 = player2
    let mutable board = new Board()
    let mutable currentPlayer = player1
    
    let rec makePlayerMove() = 
        printfn "* Your move, Player %O :" currentPlayer.Symbol
        try 
            let move = currentPlayer.GetMove(board)
            board <- board.Move(move)
        with _ -> makePlayerMove()
    
    // start the game
    member this.Play() = 
        board.Print()
        while not (board.IsEndOfGame()) do
            makePlayerMove()
            board.Print()
            currentPlayer <- if currentPlayer = player1 then player2
                             else player1

        printf "* "
        if board.IsPlayerWin Circle then
            printfn "Player %s wins !" (Circle.ToString())
        else if board.IsPlayerWin Cross then
            printfn "Player %s wins !" (Cross.ToString())
        else
            printfn "Draw"

[<EntryPoint>]
let main argv = 
    let mutable rematch = "y"
    while rematch = "y" do
        let g = new Game(new ConsolePlayer(Circle), new AIPlayer(Cross))
        g.Play()
        Console.WriteLine "Rematch ?"
        rematch <- Console.ReadLine()
    0

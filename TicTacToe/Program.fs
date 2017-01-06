// En savoir plus sur F# sur le site http://fsharp.org
// Voir le projet 'Didacticiel F#' pour obtenir de l'aide.
open System

type Symbol =
    | Cross
    | Circle
    member this.Print() =
        match this with
            | Cross -> 'X'
            | Circle -> 'O'

type Move = {
    X: int;
    Y: int;
    Symbol: Symbol
}

type Board(array) =
    let mutable array = array

    let printPlayer(symbol:Symbol option) = 
        match symbol with
        | None -> ' '
        | Some(value) -> value.Print()  

    new() = Board(Array2D.create 3 3 None)
    member this.Move(move:Move) = 
        let newArray = Array2D.copy array
        Array2D.set newArray move.X move.Y (Some move.Symbol)
        new Board(newArray)
    member this.Print() =
        printfn ""
        printfn " %c | %c | %c" (printPlayer array.[0, 0]) (printPlayer array.[0, 1]) (printPlayer array.[0, 2])
        printfn " ---------"
        printfn " %c | %c | %c" (printPlayer array.[1, 0]) (printPlayer array.[1, 1]) (printPlayer array.[1, 2])
        printfn " ---------"
        printfn " %c | %c | %c" (printPlayer array.[2, 0]) (printPlayer array.[2, 1]) (printPlayer array.[2, 2])
    
type IPlayer =
    abstract member Symbol : Symbol with get
    abstract member GetMove : board:Board -> Move

type ConsolePlayer(symbol) =
    let _symbol = symbol

    interface IPlayer with
        member val Symbol = _symbol
        member this.GetMove(board) = 
            printfn "Your move, Player %c (x y) :" (_symbol.Print())
            let moveConsole = Console.ReadLine().Trim().Split ','
            {
                X = (Int32.Parse (moveConsole.[0].Trim())) - 1;
                Y = (Int32.Parse (moveConsole.[1].Trim())) - 1;
                Symbol = _symbol
            }
            

type Game(player1:IPlayer, player2:IPlayer) =
    let player1 = player1
    let player2 = player2
    let mutable board = new Board()
    let mutable currentPlayer = player1
    member this.Play = 
        board.Print()
        while true do
            let move = currentPlayer.GetMove(board)
            board <- board.Move(move)
            board.Print()
            currentPlayer <- if currentPlayer = player1 then player2 else player1
        printfn "End game"
        

[<EntryPoint>]
let main argv = 
    let g = new Game(new ConsolePlayer(Circle), new ConsolePlayer(Cross))
    g.Play

    0 // retourne du code de sortie entier

﻿// En savoir plus sur F# sur le site http://fsharp.org
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

type Move = {
    X: int;
    Y: int;
    Symbol: Symbol
}

type Board(array:Symbol option[]) =
    let mutable array = array
    let rowSize = 3

    let get x y =         
        array.[x + y * rowSize]

    let printPlayer(symbol:Symbol option) = 
        match symbol with
        | None -> ' '
        | Some(value) -> value.Print()  

    new() = Board(Array.create 9 None)

    member this.Move(move:Move) = 
        let newArray = Array.copy array
        Array.set newArray (move.X + move.Y * rowSize) (Some move.Symbol)
        new Board(newArray)

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

type ConsolePlayer(symbol) =
    let _symbol = symbol

    interface IPlayer with
        member val Symbol = _symbol
        member this.GetMove(board) = 
            printfn "Your move, Player %c (row,col) :" (_symbol.Print())
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

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.




let flatten (A:'a[,]) = A |> Seq.cast<'a> |> Seq.toArray

let getBoards = 

    let readlines filePath = System.IO.File.ReadAllLines(filePath) |> List.ofSeq

    let boardMap = function
        | 0 -> None
        | x -> Some(x)

    let parseChar c = c.ToString() |> int
    let parseLine (s:string) = s.ToCharArray() |> Array.map parseChar
    let boardParse b = 
        let rows = b |> List.map parseLine |> Array.ofList
        Array2D.init 9 9 (fun x y -> boardMap rows.[x].[y])

    readlines >> List.chunkBySize(10) >> List.map List.tail >> List.map boardParse


    //test
let printBoard (b:Option<int>[,]) = 
    let s = function
        | None -> "-"
        | Some(x) -> x.ToString()
    let boardStr = b |> Array2D.map s 
    do printfn "%A \n" boardStr
    b

let getPossible (board:Option<int>[,]) =
    let allItems = set<int> [1..9]

    let bfold (acc:Set<int>) x =
        match x with
            | Some(i) -> acc.Add(i)
            | None -> acc

    let getPoss slice = 
        slice |> Array.fold bfold Set.empty

    let rowOpts = [for i in 0..8 -> getPoss board.[i,*]]
    let colOpts = [for i in 0..8 -> getPoss board.[*,i]]
    let nonOpts = [for i in 0..3..8 -> [ for j in 0..3..8 -> getPoss (board.[i..i+2,j..j+2] |> flatten) ]]

    let combineOpts i j = function
        | Some(_) -> None
        | None -> Some(allItems - nonOpts.[i/3].[j/3] - rowOpts.[i] - colOpts.[j])

    board |> Array2D.mapi combineOpts


let checkOptions (opts:Set<int> option [,]) = 
    //check if any square has only one option
    let hasOneOption = function
            | Some(v) -> Set.count v = 1
            | None -> false
    
    let oneOpts = [for i in 0..8 do for j in 0..8 do if hasOneOption(opts.[i,j]) then yield (i,j,(Set.toList opts.[i,j].Value).[0])  ]

    if oneOpts.Length = 0 then None else Some(oneOpts)




let rec solveBoard b = 
    let moves = b |> getPossible |> checkOptions
    
    let makeMoves mvs  (b:int option [,])  = 
        do List.iter (fun (x,y,v) -> b.[x,y] <- Some(v)) mvs
        b

    match moves with
        | None -> b
        | Some(mvs) -> 
            b |> makeMoves mvs |>  solveBoard

        


[<EntryPoint>]
let main argv = 
    
    let d = (getBoards "simple.txt").[0]
   
    

    let s = solveBoard d
    do d |> printBoard |> ignore


    //printfn "%A" argv
    0 // return an integer exit code

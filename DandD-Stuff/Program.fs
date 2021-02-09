// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open DandD_Stuff.HitDie
open FSharpPlus

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

[<EntryPoint>]
let main argv =
//    seq {
//        for n in [ 6; 8; 10; 12 ] do
//            for m in [NoMercy; ReRoll1s] ->
//                (D n, m)
//    }
//    |> Seq.iter (uncurry makeGraph)
    [ 50 .. 100 ]
    |> List.map ordinalSuffix
    |> List.iter (printfn "%s")
    0 // return an integer exit code

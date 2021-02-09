module DandD_Stuff.HitDie

open System
open System.IO
open FSharp.Stats.Distributions
open FSharpPlus
open FSharpPlus.Data
open XPlot.Plotly
open FSharp.Stats.Distributions.Continuous

let ordinalSuffix n =
    let suffixes = [|"th"; "st"; "nd"; "rd"|]
    let (d, r) = Math.DivRem(n, 10)
    n.ToString() + suffixes.[ if r < 4 && (d &&& 1) = 0 then r else 0 ]

type D = D of int

type MercyRule =
    | NoMercy
    | ReRoll1s
    override this.ToString() =
        match this with
        | NoMercy -> "no mercy rule"
        | ReRoll1s -> "re-roll 1s"

let getStats rolls (D n) mercy =
    let fn = float n
    
    let mean =
        [ 1. .. fn ]
        |> match mercy with
            | NoMercy -> List.map ((*) (1. / (fn)))
            | ReRoll1s -> List.map (function
              | 1. -> 1. / (fn ** 2.)
              | v -> v * (fn + 1.) / (fn ** 2.))
        |> List.sum
        
    let std =
        [ 1. .. fn ]
        |> List.map ((-) mean >> fun v -> v ** 2.)
        |> List.sum
        |> fun v -> (float rolls) * v / fn
        |> sqrt
        
    (mean * float rolls, std)
    
let makeGraph (D n) mercy =
    let xs = [ 1 .. 10 ]
    
    let predetermined =
        xs
        |> List.map ((*) ((n + 1) / 2 + 1) >> float) 
        
    let stats =
        xs
        |> List.map (fun i -> getStats i (D n) mercy)
        
    let avgLine =
        stats
        |> List.map fst
        
    let ``95th`` =
        stats
        |> List.map (fun (mean, std) -> mean + 2. * std)
        
    let ``5th`` =
        stats
        |> List.map (fun (mean, std) -> mean - 2. * std)
        
    let bestCase =
        xs
        |> List.map ((*) n >> float)
        
    let worstCase =
        xs
        |> List.map float
        
    let predeterminedPercentile =      
        predetermined
        |> List.zip stats
        |> List.map (fun ((mean, std), value) -> Normal.CDF mean std value)
        |> List.average
        |> (*) 100.
        |> round
        |> int
        
    let title =
        match mercy with
        | NoMercy -> $"Hit points gained by level for a d%i{n}"
        | ReRoll1s -> $"Hit points gained by level for a d%i{n}, re-rolling first 1"
        
    let chart =
        [ predetermined, $"Predetermined values (%s{predeterminedPercentile |> ordinalSuffix} percentile)"
          avgLine, "Mean"
          ``95th``, "95th percentile"
          ``5th`` , "5th percentile"
          bestCase, "Best case"
          worstCase, "Worst case"]
        |> List.map (fun (ys, name) -> Scatter(x = xs, y = ys, name = name))
        |> Chart.Plot
        |> Chart.WithTitle title
        |> Chart.WithXTitle "Levels gained"
        |> Chart.WithYTitle "Hit points gained"
    
    chart.GetHtml()
    |> curry File.WriteAllText $"d%i{n} {mercy.ToString()}.html"
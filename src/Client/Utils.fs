module Utils

open Feliz

let bgColors = [|
    color.darkBlue
    color.darkGreen
    color.darkRed
    color.darkOrange
    color.darkOrchid
    color.darkSeaGreen
    color.darkCyan
    color.darkGoldenRod
    color.darkViolet
    color.darkGray
    color.darkOliveGreen
    color.darkKhaki
    color.darkMagenta
    color.darkSalmon
    color.darkSlateBlue
    color.darkSlateGray
    color.darkTurqouise
|]

let getColor i = bgColors.[i % bgColors.Length]

open System
open Informedica.PICE.Shared.Types

let round (n : int) (c : float) = Math.Round(c, n)

let calcAverage getTotal getCount tots =
    let t =
        tots
        |> List.sumBy (fun t -> (t |> getCount) / (t |> getTotal |> float))
        |> float
    t / (tots |> List.length |> float) 
    
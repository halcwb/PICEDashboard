module Utils

open Feliz

let bgColors = [|
    color.darkRed
    color.darkOrange
    color.darkOrchid
    color.darkSeaGreen
    color.darkBlue
    color.darkGreen
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

    
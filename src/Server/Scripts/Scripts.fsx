#r "nuget: ExcelProvider"
#r "nuget: Newtonsoft.Json"
#r "nuget: Markdig"

#r "../../Informedica.PICE.Lib/bin/Debug/net9.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.PICE.Lib/bin/Debug/net9.0/Informedica.PimPrism.Lib.dll"
#r "../../Informedica.PICE.Lib/bin/Debug/net9.0/Informedica.PICE.Lib.dll"

#load "../../Shared/Shared.fs"
#load "../Report.fs"


open System
open Informedica.Utils.Lib
open Informedica.PICE.Lib

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let cachePath = "./..//data/cache/data.cache"
let exportPath = "./..//data/mrdm/Export_PICE.xlsx"

cachePath |> File.exists
exportPath |> File.exists

let stats =
    Parsing.parseMRDM exportPath cachePath
    |> Result.toValueOption
    |> _.Value
    |> fst
    |> Array.toList
    |> Statistics.calculate Types.NoFilter
    
stats.Totals.VentilationDays
    
let report = Report.create stats

report.Markdown
|> printfn "%s"
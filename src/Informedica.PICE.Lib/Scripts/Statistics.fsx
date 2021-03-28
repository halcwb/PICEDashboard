
#I __SOURCE_DIRECTORY__


#r "System.Data.Linq"
#load "../../../.paket/load/net472/PICELib/picelib.group.fsx"
open System
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"/../."

#load "../StartUp.fs"
#load "../NullCheck.fs"
#load "../String.fs"
#load "../StringBuilder.fs"
#load "../File.fs"
#load "../Cache.fs"
#load "../Markdown.fs"
#load "../Result.fs"
#load "../Types.fs"
#load "../Utils.fs"
#load "../Click.fs"
#load "../MRDM.fs"
#load "../Options.fs"
#load "../PIM.fs"
#load "../PRISM.fs"
#load "../Patient.fs"
#load "../Validation.fs"
#load "../Parsing.fs"
#load "../Statistics.fs"


open Informedica.PICE.Lib
open Types


Parsing.parseMRDM ()
|> function 
| Error _ -> failwith  "oeps"
| Ok (pats, _) ->
    pats
    |> Array.toList
    |> Statistics.calculate Filter.NoFilter
    |> fun stats ->
        printfn "Printing diagnoses"
        stats.Totals.Diagnoses
        |> List.sortByDescending snd
        |> List.iter (fun (k, v) ->
            printfn "%s: %A" k v
        )
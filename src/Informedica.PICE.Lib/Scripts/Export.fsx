
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
#load "../Export.fs"

#time

open System
open Informedica.PICE.Lib


let path = __SOURCE_DIRECTORY__ + "./../" + Parsing.cachePath



path
|> Parsing.parseMRDMwithCache
|> Export.export
|> List.filter (fun xs -> xs.[0] |> String.isNullOrWhiteSpace |> not)
|> List.distinctBy (fun xs -> xs.[0], xs.[1])
|> List.map (fun xs ->
    xs |> String.concat "\t"
)
|> List.filter (String.isNullOrWhiteSpace >> not)
|> String.concat "\n"
|> File.writeTextToFile "scores.csv"

path
|> Parsing.parseMRDMwithCache
|> Export.export
//|> Seq.skip 1
|> Seq.distinctBy (fun xs -> xs.[0], xs.[1])
|> Seq.filter (fun xs -> xs.[0] |> String.isNullOrWhiteSpace)

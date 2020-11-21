
#I __SOURCE_DIRECTORY__

#r "System.Data.Linq"
#load "../../../.paket/load/net472/PICELib/picelib.group.fsx"


#load "../NullCheck.fs"
#load "../String.fs"
#load "../StringBuilder.fs"
#load "../File.fs"
#load "../Cache.fs"
#load "../Markdown.fs"
#load "../Result.fs"
#load "../Types.fs"
#load "../Utils.fs"

open Informedica.PICE.Lib

module Click =

    open System.IO
    open FSharp.Interop.Excel

    [<Literal>]
    let path =  __SOURCE_DIRECTORY__ + "./../../mrdm/pimprism_hist.xlsx"

    //[<Literal>]
    //let path = "./../mrdm/pimprism_hist.xlsx"

    type PIMPRISMHist = ExcelFile<path, HasHeaders = true, ForceString = true>

    let pimprismHist = PIMPRISMHist path


module MRDM =

    open System
    open FSharp.Interop.Excel

    [<Literal>]
    let path = __SOURCE_DIRECTORY__ + "./../../mrdm/Export_PICE.xlsx"

    //[<Literal>]
    //let path = "./../mrdm/Export_PICE.xlsx"

    type MRDMPatient = ExcelFile<path, SheetName = "patient", HasHeaders = true, ForceString = true>
    type MRDMHospital = ExcelFile<path, SheetName = "ziekenhuis-episode", HasHeaders = true, ForceString = true>
    type MRDMPicu = ExcelFile<path, SheetName = "picu-episode", HasHeaders = true, ForceString = true>
    type MRDMDiagnose = ExcelFile<path, SheetName = "bijkomendediagnoses", HasHeaders = true, ForceString = true>

    let mrdmPatient = MRDMPatient path
    let mrdmHospital = MRDMHospital path
    let mrdmPicu = MRDMPicu path
    let mrdmDiagnose = MRDMDiagnose path

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


let patnums = 
    [
        "8157341"
        "612547"
        "2611916"
        "3512441"
    ]

path
|> Parsing.parseMRDMwithCache
|> Export.export
|> fun xs ->
    patnums
    |> List.fold (fun acc pn ->
        match xs |> List.tryFind(List.head >> ((=) pn)) with
        | Some x -> [ x ] |> List.append acc
        | None -> 

            acc
    ) [[]]
|> List.map (fun xs ->
    xs |> String.concat ";"
)
|> String.concat "\n"

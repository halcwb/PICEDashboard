#r "nuget: ExcelProvider"
#r "nuget: Newtonsoft.Json"
#r "nuget: Markdig"
#r "nuget: Microsoft.Data.SqlClient"

#r "../../Informedica.Utils.Lib/bin/Debug/net9.0/Informedica.Utils.Lib.dll"
#r "../../Informedica.PimPrism.Lib/bin/Debug/net9.0/Informedica.PimPrism.Lib.dll"

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
#load "../Database.fs"
#load "../MRDM.fs"
#load "../Options.fs"
#load "../Patient.fs"
#load "../Validation.fs"
#load "../Parsing.fs"
#load "../Statistics.fs"
#load "../Export.fs"


open System
open Informedica.Utils.Lib
open Informedica.PICE.Lib


fsi.AddPrinter<DateTime> _.ToString("dd-MM-yyyy")


[<Literal>]
let cachePath = __SOURCE_DIRECTORY__ + "./../../../data/cache/data.cache"

[<Literal>]
let exportPath = __SOURCE_DIRECTORY__ + "./../../../data/mrdm/Export_PICE.xlsx"


File.exists exportPath
File.exists cachePath
File.exists (__SOURCE_DIRECTORY__ + "./../pice.xlsx")


let picuData = (MRDM.getMrdmPicu exportPath).Data |> Seq.toArray


picuData[1].adm_disreasonid


let pats, msgs =
    Parsing.parseMRDM exportPath cachePath
    |> function
        | Ok pats -> pats
        | Error errs -> errs |> String.concat "\n" |> failwith


let stats = pats |> Array.toList |> Statistics.calculate Types.NoFilter


Statistics.toMarkdown stats


let display1 showProgress (text: string) =
    
    if showProgress then
            $"""{""}{""} 
            import LinearProgress from '@mui/material/LinearProgress';
            <LinearProgress>{text}</LinearProgress>
            """
    else
            $"""{""}{""}
            import Typography from '@mui/material/Typography';

            <React.Fragment>
                <Typography variant="h6" gutterBottom >
                    {text}
                </Typography>
            </React.Fragment>
            """
            
let display2 showProgress (text: string) =
    
    if showProgress then
            $""" 
            import LinearProgress from '@mui/material/LinearProgress';
            <LinearProgress>{text}</LinearProgress>
            """
    else
            $"""
            import Typography from '@mui/material/Typography';

            <React.Fragment>
                <Typography variant="h6" gutterBottom >
                    {text}
                </Typography>
            </React.Fragment>
            """

let test1 = $"""
        import React from 'react';

        <React.StrictMode>
            <React.Fragment>
                {display1 true "test"}
            </React.Fragment>
        </React.StrictMode>
    """


let test2 = $"""
        import React from 'react';

        <React.StrictMode>
            <React.Fragment>
                {display2 true "test"}
            </React.Fragment>
        </React.StrictMode>
    """


test1 = test2

#r "System.Data.Linq"
#load "../../../.paket/load/net472/PICELib/picelib.group.fsx"

#load "../NullCheck.fs"
#load "../String.fs"
#load "../StringBuilder.fs"
#load "../File.fs"
#load "../Markdown.fs"
#load "../Result.fs"
#load "../Types.fs"
#load "../Click.fs"
#load "../MRDM.fs"
#load "../Patient.fs"
#load "../Validation.fs"
#load "../Parsing.fs"
#load "../Statistics.fs"
#load "../Export.fs"


#time

open System
open Informedica.PICE.Lib

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

fsi.AddPrinter<DateTime>(sprintf "%A")


let pats = Parsing.parseMRDM ()


pats
|> Result.getMessages
|> Array.length
//|> Array.take 1000
//|> Array.iter (printfn "%s")


pats
|> Result.valueOrDefault (fun _ -> [||]) 
|> Array.toList
|> Statistics.calculate
|> Statistics.toString
//|> Markdown.toHtml
|> Markdown.toBrowser
|> ignore


pats
|> Result.valueOrDefault (fun _ -> [||]) 
|> Array.map Validation.validatePat
|> Array.filter ((<>) [])
|> Array.toList
|> List.collect id
|> List.map (fun pv ->
    match pv with
    | Types.NotValid(p, s) -> sprintf "%s: %s" p.HospitalNumber s
    | Types.IsValid -> ""
)
|> List.filter ((<>) "")
|> List.iter (printfn "%s")



"""
---
### Rapportage van 2019
* Totaal aantal patienten: 961
* Totaal aantal opnames 1269
* Totaal aantal ontslagen 1265
* Totaal aantal overleden 42
* Totaal aantal verpleegdagen 4531
#### Ontslag redenen
* Klaar voor niet-ICU zorg: 1222
* Overleden: 32
* Huidige (PICU) zorg voortgezet op andere afdeling: 13
* Ontslag naar palliatieve zorg: 4
* Ontslag ten gevolge van plaatsgebrek op PICU: 3
* Ontslag wegens uitstel ingreep: 1
#### Rapportage per maand

|Maand|Patienten  |Opnames  |Ontslagen  |Overleden  |Ligdagen  |
|-----|:---:|-----|-----|-----|-----|
|januari|111|125|0|1|294|
|februari|106|111|0|5|357|
|maart|100|96|0|5|385|
|april|97|107|0|10|314|
|mei|106|108|0|5|343|
|juni|107|107|0|5|357|
|juli|112|109|0|4|405|
|augustus|107|97|0|5|388|
|september|108|104|0|3|360|
|oktober|101|97|0|8|364|
|november|111|107|0|7|416|
|december|93|101|0|4|320|


---
"""
|> Markdown.toBrowser



pats
|> Result.valueOrDefault (fun _ -> [||])
|> Array.toList
|> List.collect (fun p ->
    p.HospitalAdmissions
    |> List.collect (fun ha ->
        ha.PICUAdmissions
        |> List.map (fun pa -> pa.AdmissionDate, pa.PrimaryDiagnosis)
    ) 
)
|> List.filter (fun (dt, ds) -> ds |> List.isEmpty |> not)
|> List.minBy fst


pats
|> Result.valueOrDefault (fun _ -> [||])
|> Array.tryFind (fun p ->
    p.HospitalNumber = "3240761"
)


pats
|> Export.export
|> List.map (String.concat ";")
|> String.concat "\n"
|> File.writeTextToFile "scores.csv"

Environment.CurrentDirectory

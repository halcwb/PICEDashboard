
#I __SOURCE_DIRECTORY__

#r "System.Data.Linq"
#r "../../../libs/Informedica.MetaVisionServices.Lib.dll"
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
open Informedica.MetaVisionServices.Lib


fsi.AddPrinter<DateTime> (fun dt -> dt.ToString("dd-MM-yyyy"))

let getAllHospNums () =
    use ctx = DataContext.getDataContext ()
    let frmTime = DateTime(2000, 1, 1)
    let toTime = DateTime(2020, 10, 1)
    Patient.getHospNumsForDepBetween ctx "Pediatrie" frmTime toTime


let hospNums = 
    getAllHospNums ()
    |> List.filter (fun hn -> hn |> String.isNullOrWhiteSpace |> not)


let path = __SOURCE_DIRECTORY__ + "./../" + Parsing.cachePath


let picePats =
    path
    |> Parsing.parseMRDMwithCache
    |> function
    | Ok (pats, _) -> 
        pats
        |> Array.toList
    | Error e -> e |> String.concat "\n" |> failwith


printfn "=== PICE patienten niet in MetaVision ==="
picePats 
|> List.filter (fun pat -> 
    pat.HospitalNumber |> String.isNullOrWhiteSpace |> not
)
|> List.collect (fun ha ->
    ha.HospitalAdmissions
    |> List.collect (fun ha -> 
        ha.PICUAdmissions
        |> List.filter (fun pa -> 
            pa.HospitalNumber |> String.isNullOrWhiteSpace |> not &&
            pa.AdmissionDate.IsSome &&
            pa.AdmissionDate.Value > DateTime(2008, 3, 27)
        )
        |> List.collect (fun pa -> 
            picePats
            |> List.filter (fun pat -> pat.HospitalNumber = pa.HospitalNumber)
        )
    )
)
//|> List.length
|> List.filter (fun pat ->
    let hn = pat.HospitalNumber
    hospNums
    |> List.exists ((=) hn)
    |> not
)
|> List.map (fun pat ->
    {|
        HospitalNumber = pat.HospitalNumber
        LastName = pat.LastName
        FirstName = pat.FirstName
        BirthDate = pat.BirthDate.Value
    |}
)
|> List.distinct
|> List.sortBy (fun p -> p.LastName, p.FirstName)
|> List.iter (fun p ->
    printfn "%A\t%s\t%s\t%s" 
        p.HospitalNumber
        (p.BirthDate.ToString("dd-MM-yyyy"))
        p.LastName
        p.FirstName
)



hospNums
|> List.filter (fun hn ->
    picePats
    |> List.exists (fun p -> 
        p.HospitalNumber |> String.isNullOrWhiteSpace |> not &&
        p.HospitalNumber.Trim() = hn)
    |> not
)
|> List.sort
|> List.map (fun hn ->
    use ctx = DataContext.getDataContext()
    Patient.getPatientByHospNum ctx hn
)
|> List.filter Option.isSome
|> List.map Option.get
|> List.map (fun pat ->
    {|
        HospitalNumber = pat.HospitalNumber
        LastName = pat.LastName
        FirstName = pat.FirstName
        BirthDate = pat.BirthDate
    |}
)
|> List.distinct
|> List.sortBy (fun p -> p.LastName, p.FirstName)
|> fun xs -> 
    printfn "=== MetaVision patienten niet in PICE ==="
    xs
|> List.iter (fun p ->
    printfn "%A\t%s\t%s\t%s" 
        p.HospitalNumber
        (p.BirthDate.ToString("dd-MM-yyyy"))
        p.LastName
        p.FirstName
)

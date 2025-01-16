module ServerApi

open System

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Shared
open Api
open Informedica.PICE.Lib

module PICETypes = Informedica.PICE.Lib.Types


let cachePath = "data/cache/data.cache"
let exportPath = "data/mrdm/Export_PICE.xlsx"


let init = Parsing.parseMRDM exportPath cachePath 


let mapTotals (totals: Statistics.Totals) =
    {
        Period = totals.Period
        InvalidPatients = totals.InvalidPatients |> List.toArray
        Patients = totals.Patients
        Readmission = totals.Readmission |> List.toArray
        Admissions = totals.Admissions
        Admitted = totals.Admitted
        Deaths = totals.Deaths
        DeathMode = totals.DeathMode |> List.toArray
        Discharged = totals.Discharged
        DischargeReasons = totals.DischargeReasons |> List.toArray
        HospitalDischargeDestinations = totals.HospitalDischargeDestinations |> List.toArray
        Urgency = totals.Urgency |> List.toArray
        Gender = totals.Gender |> List.toArray
        AgeGroup = totals.AgeGroup |> List.toArray
        DiagnoseGroups = totals.DiagnoseGroups |> List.toArray
        Diagnoses = totals.Diagnoses |> List.toArray
        Specialisme = totals.Specialism |> List.toArray
        Occupancy = totals.Occupancy |> List.toArray
        Cannule = totals.Canule |> List.toArray
        VentilationDays = totals.VentilationDays |> List.toArray
        VentilationDuration = totals.VentilationDuration |> List.toArray
        TransportHospital = totals.TransportHospital |> List.toArray
        TransportTeam = totals.TransportTeam |> List.toArray
        PICUDays = totals.PICUDays
        LengthOfStay = totals.LengthOfStay |> List.toArray
        PICUDeaths = totals.PICUDeaths
        PIM2Mortality = totals.PIM2Mortality
        PIM3Mortality = totals.PIM3Mortality
        PRISM4Mortality = totals.PRISM4Mortality
    }


let createReport filter =
    let filterPath =
        $"%A{filter}".ToLower() |> sprintf "data/cache/%s.report.cache"

    let mapParagraph (p: Report.Paragraph) =
        { Title = p.Title; Content = p.Content }

    let rec mapChapter (chapter: Report.Chapter) =
        match chapter.Chapters with
        | [] ->
            {
                Title = chapter.Title
                Chapters = [||]
                Paragraphs = chapter.Paragraphs |> List.map mapParagraph |> List.toArray
            }
        | _ ->
            {
                Title = chapter.Title
                Chapters = chapter.Chapters |> List.map mapChapter |> List.toArray
                Paragraphs = chapter.Paragraphs |> List.map mapParagraph |> List.toArray
            }

    match filterPath |> Cache.getCache<Report> with
    | Some report -> report
    | None ->
        printfn "creating report ..."

        init
        |> Result.valueOrDefault (fun _ -> [||])
        |> Array.toList
        |> Statistics.calculate filter
        |> Report.create
        |> fun rep ->
            {
                Sections =
                    rep.Sections
                    |> List.map (fun s ->
                        {
                            Title = s.Title
                            Chapters = s.Chapters |> List.map mapChapter |> List.toArray
                            Totals = s.Totals |> mapTotals
                            YearTotals = s.YearTotals |> List.map mapTotals |> List.toArray
                            MonthTotals = s.MonthTotals |> List.map (fun (yr, tots) -> yr, tots |> List.map mapTotals |> List.toArray) |> List.toArray
                        })
                    |> List.toArray
                Markdown = rep.Markdown
            }
        |> fun report ->
            report |> Cache.cache filterPath
            report


/// An implementation of the Shared IServerApi protocol.
/// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =

    member this.SayHello() =

        async {
            try
                return Ok "Hello World"
            with error ->
                logger.LogError(error, "Error while trying to say hello world")
                return Error error.Message
        }

    member this.GetReport filter =
        async {
            try
                let filter =
                    match filter with
                    | NoFilter -> PICETypes.NoFilter
                    | AgeFilter f ->
                        match f with
                        | Neonate -> PICETypes.Neonate
                        | Infant -> PICETypes.Infant
                        | Toddler -> PICETypes.Toddler
                        | EarlyChildhood -> PICETypes.EarlyChildhood
                        | MiddleChildhood -> PICETypes.MiddleChildhood
                        | Adolescence -> PICETypes.Adolescence
                        |> PICETypes.AgeFilter
                    | DiagnoseFilter f ->
                        match f with
                        | Oncology -> PICETypes.Oncology
                        | Cardiac -> PICETypes.Cardicac
                        | OtherDiagnoses -> PICETypes.OtherDiagnoses
                        |> PICETypes.DiagnoseFilter

                let report = createReport filter
                return Ok report
            with error ->
                printfn "%A" error
                logger.LogError(error, "Error while trying create report")
                return Error error.Message
        }


    member this.GetScoresCSV(patnums: string list) =
        printfn "getting scores from %i patients" (patnums |> List.length)
        //            printfn "first entry is: ||%s||" (patnums.[0])
        async {
            try
                let csv =
                    Parsing.parseMRDM exportPath cachePath
                    |> Export.export
                    |> fun xs ->
                        let headings = [ xs |> List.head ]

                        patnums
                        |> List.fold
                            (fun acc pn ->
                                match xs |> List.tryFind (List.head >> ((=) pn)) with
                                | Some x -> [ x ] |> List.append acc
                                | None -> [ [ sprintf "could not find: %A" pn ] ] |> List.append acc)
                            [ [] ]
                        |> List.filter (List.isEmpty >> not)
                        |> List.append headings
                    |> fun xs ->
                        printfn "found: %i patients" ((xs |> List.length) - 1)
                        xs
                    |> List.map (fun xs -> xs |> String.concat "\t")
                    |> String.concat "\n"

                return Ok csv
            with error ->
                logger.LogError(error, "Error while trying to get CSV file")
                return Error error.Message
        }


    member this.Build() : IServerApi =
        {
            SayHello = this.SayHello
            GetReport = this.GetReport
            GetScoresCSV = this.GetScoresCSV
        }

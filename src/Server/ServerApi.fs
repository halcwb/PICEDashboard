namespace Informedica.PICE.Server

module ServerApi =

    open System

    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.Configuration
    open Informedica.PICE.Shared.Types
    open Informedica.PICE.Shared.Api
    open Informedica.PICE.Lib

    let mapTotals (totals : Statistics.Totals) =
        {
            Period = totals.Period
            InvalidPatients = totals.InvalidPatients
            Patients = totals.Patients
            Admissions = totals.Admissions
            Admitted = totals.Admitted
            Deaths = totals.Deaths
            Discharged = totals.Discharged
            DischargeReasons = totals.DischargeReasons
            HospitalDischargeDestinations = totals.HospitalDischargeDestinations
            Urgency = totals.Urgency
            Gender =totals.Gender
            AgeGroup = totals.AgeGroup
            DiagnoseGroups = totals.DiagnoseGroups
            Specialisme = totals.Specialism
            Occupancy = totals.Occupancy
            Cannule = totals.Canule
            TransportHospital = totals.TransportHospital
            TransportTeam = totals.TransportTeam
            PICUDays = totals.PICUDays
            PICUDeaths = totals.PICUDeaths
            PIM2Mortality = totals.PIM2Mortality
            PIM3Mortality = totals.PIM3Mortality
            PRISM4Mortality = totals.PRISM4Mortality
        }

    let createReport () =
        let path = "../mrdm/report.cache"

        let mapParagraph (p : Report.Paragraph) =
            {
                Title = p.Title
                Content = p.Content
            }

        let rec mapChapter (chapter : Report.Chapter) =
            match chapter.Chapters with
            | [] -> 
                {
                    Title = chapter.Title
                    Chapters = []
                    Paragraphs = 
                        chapter.Paragraphs |> List.map mapParagraph
                }
            | _ ->
                {
                    Title = chapter.Title
                    Chapters = chapter.Chapters |> List.map mapChapter
                    Paragraphs = chapter.Paragraphs |> List.map mapParagraph
                }

        match path |> Cache.getCache<Report> with
        | Some report -> report
        | None        ->
            printfn "creating report ..."
            Parsing.parseMRDM ()
            |> Result.valueOrDefault (fun _ -> [||])
            |> Array.toList
            |> Statistics.calculate
            |> Report.create
            |> fun rep ->
                {
                    Sections =
                        rep.Sections
                        |> List.map (fun s ->
                            {
                                Title = s.Title
                                Chapters = 
                                    s.Chapters |> List.map mapChapter
                                Totals = s.Totals |> mapTotals
                                YearTotals = 
                                    s.YearTotals
                                    |> List.map mapTotals
                                MonthTotals =
                                    s.MonthTotals
                                    |> List.map (fun (yr, tots) ->
                                        yr, 
                                        tots
                                        |> List.map mapTotals
                                    )
                            }
                        )
                    Markdown = rep.Markdown
                }
            |> fun report ->
                report |> Cache.cache path
                report


    /// An implementation of the Shared IServerApi protocol.
    /// Can require ASP.NET injected dependencies in the constructor and uses the Build() function to return value of `IServerApi`.
    type ServerApi(logger: ILogger<ServerApi>, config: IConfiguration) =

        member this.SayHello () = 
            
            async {
                try 
                    return Ok "Hello World"
                with 
                    | error -> 
                        logger.LogError(error, "Error while trying to say hello world")
                        return Error error.Message
            }

        member this.GetReport () =

            async {
                try 
                    let report = createReport()
                    return Ok report
                with
                | error ->
                        logger.LogError(error, "Error while trying to say hello world")
                        return Error error.Message
            }


        member this.Build() : IServerApi =
            {
                SayHello = this.SayHello
                GetReport = this.GetReport
            }
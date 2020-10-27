namespace Informedica.PICE.Server

module ServerApi =

    open System

    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.Configuration
    open Informedica.PICE.Shared.Types
    open Informedica.PICE.Shared.Api
    open Informedica.PICE.Lib

    let mapTotals period (totals : Statistics.Totals) =
        {
            Period = period
            InvalidPatients = totals.InvalidPatients
            Patients = totals.Patients
            Admissions = totals.Admissions
            Admitted = totals.Admitted
            Deaths = totals.Deaths
            Discharged = totals.Discharged
            DischargeReasons = totals.DischargeReasons
            HospitalDischargeDestinations = totals.HospitalDischargeDestinations
            Gender =totals.Gender
            AgeGroup = totals.AgeGroup
            PICUDays = totals.PICUDays
            PICUDeaths = totals.PICUDeaths
            PIM2Mortality = totals.PIM2Mortality
            PIM3Mortality = totals.PIM3Mortality
            PRISM4Mortality = totals.PRISM4Mortality
        }

    let createReport () =
        let path = "../mrdm/report.cache"
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
                                Groups = 
                                    s.Groups
                                    |> List.map (fun g ->
                                        { 
                                            Title = g.Title
                                            Items =
                                                g.Items
                                                |> List.map (fun i ->
                                                    {
                                                        Title = i.Title
                                                        Content = i.Content
                                                    }
                                                )
                                        }
                                    )
                                Totals = 
                                    s.Totals
                                    |> List.map (fun (p, t) -> 
                                        mapTotals p t
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
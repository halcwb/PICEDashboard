namespace Informedica.PICE.Server

module ServerApi =

    open System

    open Microsoft.Extensions.Logging
    open Microsoft.Extensions.Configuration
    open Informedica.PICE.Shared
    open Informedica.PICE.Shared.Api
    open Informedica.PICE.Lib

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

        member this.GetStatistics () =
            let mapTotals (totals : Statistics.Totals) =
                {
                    Statistics.totals with
                        Patients = totals.Patients
                        Admissions = totals.Admissions
                        Admitted = totals.Admitted
                        Deaths = totals.Deaths
                        Discharged = totals.Discharged
                        DischargeReasons = totals.DischargeReasons
                        PICUDays = totals.PICUDays
                        PICUDeaths = totals.PICUDeaths
                        PIM2Mortality = totals.PIM2Mortality
                        PIM3Mortality = totals.PIM3Mortality
                }

            async {
                try 
                    let pats = Parsing.parseMRDM ()
                    let stats = 
                        pats
                        |> Result.valueOrDefault (fun _ -> [||])
                        |> Array.toList
                        |> Statistics.calculate
                        |> fun stats -> 
                            {
                                Statistics.statistics with
                                    Totals = stats.Totals |> mapTotals
                                    InvalidPatients = stats.InvalidPatients
                                    YearTotals = 
                                        stats.YearTotals
                                        |> List.map (fun ytot ->
                                            {
                                                Statistics.yearTotals with
                                                    Year = ytot.Year
                                                    Totals = ytot.Totals |> mapTotals
                                                    MonthTotals =
                                                        ytot.MonthTotals
                                                        |> List.map (fun mtot ->
                                                            {
                                                                Statistics.monthTotals with
                                                                    Month = mtot.Month
                                                                    Totals = mtot.Totals |> mapTotals
                                                            }
                                                        )
                                            }
                                        )
                                    Html = 
                                        stats 
                                        |> Statistics.toString
//                                        |> fun s -> printfn "%s" s; s

                            }

                    return Ok stats
                with
                | error ->
                        logger.LogError(error, "Error while trying to say hello world")
                        return Error error.Message
            }


        member this.Build() : IServerApi =
            {
                SayHello = this.SayHello
                GetStatistics = this.GetStatistics
            }
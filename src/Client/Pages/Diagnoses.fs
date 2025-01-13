namespace Pages


module Diagnoses =

    open System

    open Elmish
    open Feliz
    open Fable.Core
    open Fable.Core.JsInterop

    open Shared


    let private getStackedBarChart title section get =
        let perYr =
            section.YearTotals
            |> List.map (fun t -> t.Period, t |> get)
            |> List.filter (fun (_, tots) -> tots |> List.sumBy snd > 0)

        let perMo =
            section.MonthTotals
            |> List.map (fun (yr, xs) ->
                yr,
                xs
                |> List.map (fun t -> t.Period, t |> get)
                |> List.filter (fun (p, tots) -> tots |> List.sumBy snd > 0))

        let props =
            {|
                title = title
                perYear = perYr
                perMonth = perMo
            |}

        Components.StackedBarChart.View(props)


    [<JSX.Component>]
    let View
        (props:
            {|
                displayType: DisplayType
                selected: string[]
                report: Report
            |})
        =
        let selectTxt = props.selected |> String.concat "/" |> sprintf "Selectie: %s"

        match props.selected with
        | [||] ->

            let sx = {| paddingTop = 10 |}

            JSX.jsx
                $"""
            import React from 'react';
            import Typography from '@mui/material/Typography';

            <React.Fragment>
                <Typography variant="h4">Selecteer 1 of meerdere diagnoses</Typography>
                <Typography variant="body1" sx={sx}>{selectTxt}</Typography>
            </React.Fragment>
            """
        | _ ->

            let sx = {| paddingTop = 10 |}

            let chart =
                let section = props.report.Sections |> List.head

                fun totals ->
                    totals.Diagnoses
                    |> List.filter (fun (k, v) -> props.selected |> Array.exists ((=) k))
                |> getStackedBarChart "Selectie" section

            JSX.jsx
                $"""
            import React from "react";
            import Typography from "@mui/material/Typography";
            import Box from "@mui/material/Box";

            <Box>
                {chart}
                <Typography variant="body1" sx={sx}>{selectTxt}</Typography>
            </Box>
            """

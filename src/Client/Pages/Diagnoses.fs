namespace Pages


module Diagnoses =

    open System

    open Elmish
    open Feliz
    open Fable.Core
    open Fable.Core.JsInterop

    open Shared


    let private getStackedBarChart title section get =
        let perYr = section.YearTotals |> List.map (fun t -> t.Period, t |> get)

        let perMo =
            section.MonthTotals
            |> List.map (fun (yr, xs) -> yr, xs |> List.map (fun t -> t.Period, t |> get))

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
                selected: string list
                report: Report
            |})
        =
        let selectTxt = props.selected |> String.concat "/" |> sprintf "Selectie: %s"

        match props.selected with
        | [] ->

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
                    |> List.filter (fun (k, v) -> props.selected |> List.exists ((=) k))
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


(*
        Html.div [
                prop.style [ style.paddingLeft 20 ]
                prop.children [
                    match props.selected with
                    | [] -> 
                        Mui.typography [
                            typography.variant.h4
                            prop.text "Selecteer 1 of meerdere diagnoses"
                        ]
                    | _ -> 
                        Html.div [
                        ]
                        fun totals ->
                            totals.Diagnoses
                            |> List.filter (fun (k, v) ->
                                props.selected
                                |> List.exists ((=) k)
                            )
                        |> getStackedBarChart (props.report.Sections |> List.head) "Selectie"

                        Mui.typography [
                            typography.variant.body1
                            prop.style [
                                style.paddingTop 10
                            ]
                            prop.text selectTxt
                        ]
                    ]
            ]
        )
*)

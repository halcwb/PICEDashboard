namespace Components

open Feliz
open Elmish
open Fable.Core
open Fable.React
open Fable.Core.JsInterop

module PieChart =


    module private Elmish =


        let createPieSlice name value color =
            {|
                name = name
                value = value
                color = color
            |}


        let colorKeyValueList xs =
            xs |> List.mapi (fun i (k, v) -> createPieSlice k v (Colors.getColor i))


        let createListItem color background text =
            {|
                text = text
                color = color
                background = background
            |}

        let coloredItemList xs =
            xs |> List.mapi (fun i s -> createListItem color.white (Colors.getColor i) s)

        let keyValueListToColoredItems kvs =
            let t = kvs |> List.map snd |> List.sum |> float

            kvs
            |> List.map (fun (k, v) -> sprintf "%s: %i (%.0f" k v (100. * (v |> float) / t) |> sprintf "%s%%)")
            |> coloredItemList


        type State =
            {
                position: Position
                last: int
                showPercentage: bool
            }

        and Position =
            | Position of int
            | First
            | Last
            | Stopped


        type Msg =
            | SkipLast
            | SkipFirst
            | SkipPrevious
            | SkipNext
            | Stop


        let init last =
            fun () ->
                {
                    position = Stopped
                    last = last
                    showPercentage = true
                },
                Cmd.none


        let update msg state =
            printfn $"msg"

            match msg with
            | Stop -> { state with position = Stopped }, Cmd.none
            | SkipPrevious ->
                { state with
                    position =
                        match state.position with
                        | Position i when i > 1 -> i - 1 |> Position
                        | Position _ -> First
                        | Stopped -> 0 |> Position
                        | Last -> state.last - 1 |> Position
                        | First -> Last
                },
                Cmd.none
            | SkipNext ->
                { state with
                    position =
                        match state.position with
                        | Position i -> i + 1
                        | Stopped
                        | Last -> 0
                        | First -> 1
                        |> Position
                },
                Cmd.none
            | SkipFirst -> { state with position = First }, Cmd.none
            | SkipLast -> { state with position = Last }, Cmd.none


    open Elmish


    let View
        (props:
            {|
                title: string
                data: (string * int) list
                periods: (string * (string * int) list) list
            |})
        =
        let last = (props.periods |> List.length) - 1
        let state, dispatch = React.useElmish (init last, update, [||])

        let p, data =
            match state.position with
            | Stopped ->
                let start, end' =
                    props.periods |> List.head |> fst, props.periods |> List.rev |> List.head |> fst

                sprintf "%s - %s" start end', props.data
            | _ ->
                let i =
                    match state.position with
                    | Position i -> i
                    | Last -> (props.periods |> List.length) - 1
                    | _ -> 0

                props.periods[i % (props.periods |> List.length)]

        let coloredList =
            let data = data |> keyValueListToColoredItems
            Components.ColoredList.View(data)

        let pieChart =
            let data = data |> colorKeyValueList |> List.toArray

            let cells =
                data
                |> Array.map (fun d ->
                    JSX.jsx
                        $"""
                    import React from "react";
                    import {{ Cell }} from 'recharts';
                    <Cell fill={d.color} />
                    """)

            JSX.jsx
                $"""
            import React from "react";
            import {{ PieChart, Tooltip, Pie }} from "recharts";
            
            <PieChart width={500} height={500}>
                <Tooltip/>
                <Pie
                    data={data}
                    dataKey="value"
                    animationDuration={700}
                >
                    {cells}
                </Pie>
            </PieChart>

            """

        let toolbarTitle = $"{props.title} {p}"

        let bxSx = {| width = "100%" |}

        let gdSx =
            {|
                justifyContent = "space-evenly"
                spacing = 8
            |}

        let tpSx = {| flexGrow = 1 |}

        JSX.jsx
            $"""
        import React from 'react';
        import Grid from "@mui/material/Grid2";
        import Box from "@mui/material/Box";
        import Toolbar from "@mui/material/Toolbar";
        import Typography from "@mui/material/Typography";
        import IconButton from "@mui/material/IconButton";
        import FirstPageIcon from '@mui/icons-material/FirstPage';
        import SkipPreviousIcon from "@mui/icons-material/SkipPrevious";
        import SkipNextIcon from "@mui/icons-material/SkipNext";
        import LastPageIcon from '@mui/icons-material/LastPage';
        import StopIcon from '@mui/icons-material/Stop';

        <Box sx = {bxSx}>
            <Toolbar disableGutters={true}>
                <Typography sx={tpSx}>
                    {toolbarTitle}
                </Typography>
                <IconButton onClick={fun _ -> SkipPrevious |> dispatch}>
                    <FirstPageIcon/>
                </IconButton>
                <IconButton onClick={fun _ -> SkipPrevious |> dispatch}>
                    <SkipPreviousIcon/>
                </IconButton>
                <IconButton onClick={fun _ -> SkipNext |> dispatch}>
                    <SkipNextIcon/>
                </IconButton>
                <IconButton onClick={fun _ -> SkipLast |> dispatch}>
                    <LastPageIcon/>
                </IconButton>
                <IconButton onClick={fun _ -> Stop |> dispatch}>
                    <StopIcon/>
                </IconButton>
            </Toolbar>
            <Grid sx ={gdSx} container >
                {coloredList}
                {pieChart}
            </Grid>
        </Box>
        """


(*
    
    let renderCustomLabel (input: IPieLabelProperties) =
        let radius = input.innerRadius + (input.outerRadius - input.innerRadius) * 0.5;
        let radian = System.Math.PI / 180.
        let x = (input.cx + radius * cos (-input.midAngle * radian))
        let y = (input.cy + radius * sin (-input.midAngle * radian))

        Html.text [
            prop.style [
                style.fill color.white
            ]
            prop.x x
            prop.y y
            prop.dominantBaseline.central
            if x > input.cx then prop.textAnchor.startOfText else prop.textAnchor.endOfText
            prop.text (sprintf "%.0f%%" (100. * input.percent))
        ]


    let private comp =
        React.functionComponent("piechart", fun (props: {| title : string; data: (string * int) list; periods : (string * (string * int) list) list |}) -> 

            let createPieChart data =
                let cells =
                    data
                    |> List.map (fun d ->
                        Recharts.cell [
                            cell.fill d.color
                        ])

                Recharts.pieChart [
                    pieChart.width  500
                    pieChart.height 500
                    pieChart.children [
                        Recharts.tooltip []
                        Recharts.pie [
                            pie.data data
                            pie.labelLine false
                            pie.label false
                            pie.dataKey (fun p -> p.value)
                            pie.animationDuration 700
                            pie.children cells
                        ]
                    ]
            ]

            Html.div [
                Mui.toolbar [
                    toolbar.disableGutters true
                    toolbar.children [
                        Mui.typography [
                            prop.style [ style.flexGrow 1]
                            typography.color.primary
                            typography.variant.h6
                            prop.text (sprintf "%s %s" props.title p)
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> SkipFirst |> dispatch)
                            iconButton.children [
                                Icons.firstPageIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> SkipPrevious |> dispatch)
                            iconButton.children [
                                Icons.skipPreviousIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> SkipNext |> dispatch)
                            iconButton.children [
                                Icons.skipNextIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> SkipLast |> dispatch)
                            iconButton.children [
                                Icons.lastPageIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> Stop |> dispatch)
                            iconButton.children [
                                Icons.stopIcon []
                            ]
                        ]                    ]
                ]

                Mui.grid [
                    grid.container true
                    grid.justify.spaceEvenly
                    grid.alignItems.center
                    grid.direction.row
                    grid.children [
                        data
                        |> ColoredList.keyValueListToColoredItems
                        |> ColoredList.render

                        data 
                        |> colorKeyValueList
                        |> createPieChart
                    ]
                ]
            ]
        )

*)

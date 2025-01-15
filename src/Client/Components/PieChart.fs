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


    [<JSX.Component>]
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
            ColoredList.View(data)

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

        let navBar =
            NavigationBar.View(
                {|
                    title = toolbarTitle
                    showPerc = None
                    skipFirst = fun _ -> SkipFirst |> dispatch
                    skipPrev = fun _ -> SkipPrevious |> dispatch
                    skipNext = fun _ -> SkipNext |> dispatch
                    skipLast = fun _ -> SkipLast |> dispatch
                    stop = fun _ -> Stop |> dispatch
                |}
            )

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
            {navBar}
            <Grid sx ={gdSx} container >
                {coloredList}
                {pieChart}
            </Grid>
        </Box>
        """

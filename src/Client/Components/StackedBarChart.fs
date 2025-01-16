namespace Components


module StackedBarChart =

    open Elmish
    open Fable.Core
    open Fable.React

    module private Elmish =

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
            | ShowPercentage


        let init last =
            fun () ->
                {
                    position = Stopped
                    last = last
                    showPercentage = false
                },
                Cmd.none


        let update msg state =
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
            | ShowPercentage ->
                { state with
                    showPercentage = state.showPercentage |> not
                },
                Cmd.none


    let private createBars labels =
        let create i label =
            let getKey =
                fun (_, xs) ->
                    if xs |> Array.length <= i then
                        0.
                    else
                        xs |> Array.item i |> snd |> float

            JSX.jsx
                $"""
            import React from 'react';
            import {{ Bar }} from 'recharts';
            <Bar
                key={i}
                name={label}
                dataKey={getKey}
                stackId="a"
                fill={Colors.getColor (i)}
            />
            """

        labels |> Array.mapi create


    open Elmish


    [<JSX.Component>]
    let View
        (props:
            {|
                title: string
                perYear: (string * (string * int)[])[]
                perMonth: (string * (string * ((string * int)[]))[])[]
            |})
        =
        let last = (props.perMonth |> Array.length) - 1
        let state, dispatch = React.useElmish (init last, update, [||])

        let p, data =
            let total xs =
                xs |> Array.map snd |> Array.sum |> float

            let map xs =
                xs
                |> Array.map (fun (period, data) ->
                    let t = data |> total

                    period,
                    data
                    |> Array.map (fun (k, v) ->
                        k,
                        if state.showPercentage then
                            (100. * (v |> float) / t) |> Math.round 2
                        else
                            v |> float))

            match state.position with
            | Stopped -> "", props.perYear |> map
            | _ ->
                let i =
                    match state.position with
                    | Position i -> i
                    | Last -> (props.perMonth |> Array.length) - 1
                    | _ -> 0

                let xs = props.perMonth |> Array.item (i % (props.perMonth |> Array.length))

                xs |> fst, xs |> snd |> map

        let bars =
            props.perYear
            |> Array.collect snd
            |> Array.map fst
            |> Array.distinct
            |> createBars

        let toolbarTitle = $"{props.title} {p}"

        let toolBar =
            NavigationBar.View(
                {|

                    title = toolbarTitle
                    showPerc = Some(fun _ -> ShowPercentage |> dispatch)
                    skipFirst = (fun _ -> SkipFirst |> dispatch)
                    skipPrev = (fun _ -> SkipPrevious |> dispatch)
                    skipNext = (fun _ -> SkipNext |> dispatch)
                    skipLast = (fun _ -> SkipLast |> dispatch)
                    stop = (fun _ -> Stop |> dispatch)
                |}
            )

        let getDataKey = fun (k, _) -> k.ToString()

        let data = data |> Array.map (fun (k, xs) -> k, xs)

        if p <> "" then
            Logging.log $"data fro {p}" data

        JSX.jsx
            $"""
        import React from 'react';
        import Box from '@mui/material/Box';
        import {{ BarChart, CartesianGrid, XAxis, YAxis, Tooltip, Legend }} from 'recharts';

        <Box>
            {toolBar}
            <BarChart
                width={1100}
                height={500}
                data={data}
            >
                <CartesianGrid strokeDasharray="1 1" />
                <XAxis dataKey={getDataKey} />
                <YAxis />
                <Tooltip />
                <Legend verticalAlign="bottom" />

                {bars}
            </BarChart>
        </Box>
        """

namespace Views

module OccupancyGraph =

    open System
    open Feliz
    open Elmish
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React


    module private Elmish =

        type State =
            {
                position: Position
                last: int
                legend: string option
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
            | EnterLegend of string

        let init last =
            fun () ->
                {
                    position = Stopped
                    last = last
                    legend = None
                },
                Cmd.none


        let update msg state =
            match msg with
            | Stop ->
                { state with
                    position = Stopped
                    legend = None
                },
                Cmd.none
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
            | EnterLegend s -> { state with legend = Some s }, Cmd.none


        type Serie = { name: string; data: Point[] }
        and Point = { date: string; value: int }


        let createLine i label opaque (data: Point[]) =
            let dataKey = fun (x: Point) -> x.value

            match opaque with
            | _ when label = "gemiddeld" ->
                {|
                    strokeColor = color.gray
                    strokeWidth = 4.
                    strokeOpacity = 1.
                    strokeDashArray = [| 15; 5 |]
                |}
            | _ when label = "max" ->
                {|
                    strokeColor = color.red
                    strokeWidth = 4.
                    strokeOpacity = 0.5
                    strokeDashArray = [| 15; 5 |]
                |}
            | _ when label = "min" ->
                {|
                    strokeColor = color.green
                    strokeWidth = 4.
                    strokeOpacity = 0.5
                    strokeDashArray = [| 15; 5 |]
                |}
            | _ when label = "mean" ->
                {|
                    strokeColor = color.gray
                    strokeWidth = 4.
                    strokeOpacity = 0.5
                    strokeDashArray = [| 15; 5 |]
                |}
            | false ->
                {|
                    strokeColor = color.darkBlue
                    strokeWidth = 1.
                    strokeOpacity = 0.2
                    strokeDashArray = [||]
                |}
            | true ->
                {|
                    strokeColor = color.darkBlue
                    strokeWidth = 2.
                    strokeOpacity = 1.
                    strokeDashArray = [||]
                |}
            |> fun
                   (props:
                       {|
                           strokeColor: string
                           strokeWidth: float
                           strokeOpacity: float
                           strokeDashArray: int[]
                       |}) ->
                JSX.jsx
                    $"""
                <Line
                    key={i} 
                    name={label}
                    dataKey={dataKey}
                    dot={false}
                    strokeOpacity={props.strokeOpacity}
                    stroke={props.strokeColor}
                    strokeWidth={props.strokeWidth}
                    type="monotone"
                    data={data}
                />
                """


    open Elmish


    [<JSX.Component>]
    let View
        (props:
            {|
                title: string
                data: (string * ((DateTime * int)[]))[]
            |})
        =
        let last = (props.data |> Array.length) - 1
        let state, dispatch = React.useElmish (init last, update, [||])

        let max = props.data |> Array.collect snd |> Array.maxBy snd |> snd

        let mean =
            props.data
            |> Array.collect snd
            |> fun xs ->
                [|
                    "gemiddeld",
                    xs
                    |> Array.map (fun (dt, value) -> dt.ToString("dd-MM"), value)
                    |> Array.groupBy fst
                    |> Array.map (fun (dt, values) ->
                        dt,
                        values
                        |> Array.map snd
                        |> Array.sum
                        |> fun x -> (x |> float) / (values |> Array.length |> float) |> int)
                |]

        let data =
            let data =
                props.data
                |> Array.map (fun (p, data) -> p, data |> Array.map (fun (dt, value) -> dt.ToString("dd-MM"), value))

            match state.position with
            | Stopped -> data
            | _ ->
                let i =
                    match state.position with
                    | Position i -> i
                    | Last -> (props.data |> Array.length) - 1
                    | _ -> 0

                data
                |> Array.item (i % (props.data |> Array.length))
                |> fun (label, xs) ->
                    [|
                        "max", xs |> Array.map (fun (k, _) -> k, xs |> Array.map snd |> Array.max)
                        "mean",
                        xs
                        |> Array.map (fun (k, _) -> k, xs |> Array.map (snd >> float) |> Array.average |> int)
                        label, xs
                        "min", xs |> Array.map (fun (k, _) -> k, xs |> Array.map snd |> Array.min)

                    |]
            |> Array.append mean
            |> Array.map (fun (yr, data) ->
                {
                    name = yr
                    data = data |> Array.map (fun (dt, value) -> { date = dt; value = value })
                })

        let lines =
            data
            |> Array.mapi (fun i d ->
                let opaque = state.legend = Some d.name || state.position <> Stopped
                d.data |> createLine i d.name opaque)

        let title =
            match state.position with
            | Position i ->
                props.data
                |> Array.item (i % (props.data |> Array.length))
                |> fst
                |> sprintf "%s"
            | Stopped ->
                match state.legend with
                | Some s -> sprintf "%s" s
                | None ->
                    sprintf "%s - %s" (props.data |> Array.head |> fst) (props.data |> Array.rev |> Array.head |> fst)
            | First -> sprintf "%s" (props.data |> Array.head |> fst)
            | Last -> sprintf "%s" (props.data |> Array.rev |> Array.head |> fst)
            |> sprintf "%s %s" props.title

        let toolBar =
            Components.NavigationBar.View(
                {|
                    title = title
                    showPerc = None
                    skipFirst = (fun _ -> SkipFirst |> dispatch)
                    skipLast = (fun _ -> SkipLast |> dispatch)
                    skipPrev = (fun _ -> SkipPrevious |> dispatch)
                    skipNext = (fun _ -> SkipNext |> dispatch)
                    stop = (fun _ -> Stop |> dispatch)
                |}
            )

        JSX.jsx
            $"""
        import React from 'react';
        import {{ LineChart, Line, CartesianGrid, XAxis, YAxis, Tooltip, Legend }} from 'recharts';

        <React.Fragment>
            {toolBar}
            <LineChart
                width={1100}
                height={600}
            >
                <CartesianGrid strokeDasharray={[ 1, 1 ]} />
                <XAxis dataKey={fun (x: Point) -> x.date} allowDuplicatedCategory={false} />
                <YAxis number dataKey={fun (x: Point) -> x.value} domain={[| 0; max |]} />
                <Tooltip />
                <Legend verticalAlign="bottom" onMouseEnter={fun e -> e?value |> EnterLegend |> dispatch} />
                {lines}
            </LineChart>
        </React.Fragment>
        """

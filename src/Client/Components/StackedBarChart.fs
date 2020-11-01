namespace Components

module StackedBarChart =

    open System
    open Feliz
    open Feliz.UseElmish
    open Elmish
    open Feliz.MaterialUI
    open Fable.MaterialUI
    open Feliz.Recharts


    type State = { position : Position; last : int; showPercentage : bool }
    and Position = | Position of int | First | Last | Stopped


    type Msg = | SkipLast | SkipFirst | SkipPrevious | SkipNext | Stop | ShowPercentage


    let init last =
        fun () -> { position = Stopped; last = last; showPercentage = true }, Cmd.none

    
    let update msg state =
        match msg with
        | Stop -> { state with position = Stopped }, Cmd.none
        | SkipPrevious -> 
            { state with 
                position = 
                    match state.position with
                    | Position  i  when i > 1 -> i - 1 |> Position
                    | Position _ -> First
                    | Stopped    -> 0 |> Position
                    | Last       -> state.last - 1 |> Position
                    | First      -> Last
            }, Cmd.none
        | SkipNext -> 
            { state with 
                position = 
                    match state.position with
                    | Position  i    -> i + 1 
                    | Stopped | Last -> 0 
                    | First          -> 1
                    |> Position
            }, Cmd.none
        | SkipFirst ->
            { state with
                position = First
            }, Cmd.none
        | SkipLast ->
            { state with
                position = Last
            }, Cmd.none
        | ShowPercentage ->
            { state with
                showPercentage = state.showPercentage |> not
            }, Cmd.none



    let createBars labels =
        labels
        |> List.mapi (fun i label ->
            Recharts.bar [
                bar.name label
                bar.dataKey (fun (_, xs) -> xs |> List.item i |> snd |> float)
                bar.stackId "a"
                bar.fill (Utils.getColor i)
            ]
        )


    let private comp =
        React.functionComponent("stacked-chart", fun (props: {| title : string; perYear : (string * (string * int) list) list; perMonth : (string * (string * ((string * int) list)) list) list |}) ->
            let last = (props.perMonth |> List.length) - 1
            let state, dispatch = React.useElmish(init last, update, [||])
            Browser.Dom.console.log("index", state.position)
            let p, data =
                let total xs = 
                    xs
                    |> List.map snd
                    |> List.sum
                    |> float

                let map xs =
                    xs
                    |> List.map (fun (period, data) ->
                        let t = data |> total
                        period,
                        data
                        |> List.map (fun (k, v) ->
                            k , 
                            if state.showPercentage then (100. * (v |> float) / t) |> Utils.round 2
                            else v |> float
                        )
                    )

                match state.position with
                | Stopped -> "", props.perYear |> map
                | _ ->
                    let i = 
                        match state.position with
                        | Position i -> i
                        | Last       -> (props.perMonth |> List.length) - 1
                        | _          -> 0

                    let xs = 
                        props.perMonth 
                        |> List.item (i % (props.perMonth |> List.length))

                    xs |> fst,
                    xs
                    |> snd
                    |> map


            let bars =
                props.perYear
                |> List.collect snd
                |> List.map fst
                |> List.distinct
                |> createBars

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
                            prop.onClick (fun _ -> ShowPercentage |> dispatch)
                            iconButton.children [
                                Icons.equalizerIcon []
                            ]
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
                        ]
                    ]
                ]


                Recharts.barChart [
                    barChart.width 1100
                    barChart.height 500
                    barChart.data data
                    barChart.children [
                        Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1) ]
                        Recharts.xAxis [ xAxis.dataKey (fun (k, _) -> k |> string) ]
                        Recharts.yAxis []
                        Recharts.tooltip []
                        Recharts.legend [ legend.verticalAlign.bottom ]

                        yield! bars
                    ]
                ]
            ]
        )


    let render title perYear perMonth = comp({| title = title; perYear = perYear; perMonth = perMonth |})
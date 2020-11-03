namespace Components

module OccupancyGraph = 

    open System
    open Feliz
    open Feliz.UseElmish
    open Elmish
    open Feliz.MaterialUI
    open Fable.MaterialUI
    open Feliz.Recharts
    open Fable.Core.JsInterop


    type State = 
        { 
            position : Position
            last : int
            legend : string option
        }
    and Position = | Position of int | First | Last | Stopped


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
            }, Cmd.none


    let update msg state =
        match msg with
        | Stop -> 
            { state with 
                position = Stopped 
                legend = None
            }, Cmd.none
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
        | EnterLegend s -> 
            { state with
                legend = Some s
            }, Cmd.none


    type Serie = 
        {
            name : string
            data : Point list
        }
    and Point = { date : string; value : int }


    let createLine i label opaque (data : Point list) =
        Recharts.line [
            line.name label
            line.dataKey (fun (x: Point) -> x.value)
            line.dot false
            line.monotone
            line.data data

            match opaque with
            | _ when label = "gemiddeld" ->
                line.strokeDasharray [| 15; 5 |]
                line.stroke (color.gray)
                line.strokeWidth 4
            | _ when label = "max" ->
                line.strokeDasharray [| 15; 5 |]
                line.stroke (color.red)
                bar.strokeOpacity 0.5
                line.strokeWidth 4
            | _ when label = "min" ->
                line.strokeDasharray [| 15; 5 |]
                line.stroke (color.green)
                line.strokeWidth 4
                bar.strokeOpacity 0.5
            | _ when label = "mean" ->
                line.strokeDasharray [| 15; 5 |]
                line.stroke (color.gray)
                line.strokeWidth 4
                bar.strokeOpacity 0.5
            | false -> 
                bar.strokeOpacity 0.2
                line.stroke (color.darkBlue)
            | true  -> 
                line.strokeWidth 2
                line.stroke (color.darkBlue)
        ]

    let comp =
        React.functionComponent("line-chart", fun (props : {| title : string; data : (string * ((DateTime * int) list)) list |}) ->
            let last = (props.data |> List.length) - 1
            let state, dispatch = React.useElmish(init last, update, [||])

            let max =
                props.data
                |> List.collect snd
                |> List.maxBy snd
                |> snd

            let mean =
                props.data
                |> List.collect snd
                |> fun xs ->
                    [
                        "gemiddeld",
                        xs
                        |> List.map (fun (dt, value) ->
                            dt.ToString("dd-MM"), value
                        )
                        |> List.groupBy fst
                        |> List.map (fun (dt, values) ->
                            dt,
                            values
                            |> List.map snd
                            |> List.sum
                            |> fun x -> 
                                (x |> float) / (values |> List.length |> float)
                                |> int
                        )
                    ]

            let data =
                let data =
                    props.data
                    |> List.map (fun (p, data) ->
                        p,
                        data
                        |> List.map (fun (dt, value) -> dt.ToString("dd-MM"), value)
                    )

                match state.position  with
                | Stopped ->
                    data
                | _ -> 
                    let i = 
                        match state.position with
                        | Position i -> i
                        | Last       -> (props.data |> List.length) - 1
                        | _          -> 0

                    data
                    |> List.item (i % (props.data |> List.length))
                    |> fun (label, xs) ->
                        [
                            "max" ,
                            xs
                            |> List.map (fun (k, _) ->
                                k, xs |> List.map snd |> List.max
                            )
                            "mean" ,
                            xs
                            |> List.map (fun (k, _) ->
                                k, xs |> List.map (snd >> float) |> List.average |> int
                            )
                            label, xs
                            "min" ,
                            xs
                            |> List.map (fun (k, _) ->
                                k, xs |> List.map snd |> List.min
                            )

                        ]
                |> List.append mean
                |> List.map (fun (yr, data) ->
                    {
                        name = yr
                        data =
                            data
                            |> List.map (fun (dt, value) -> 
                                { date = dt ; value = value})
                    }
                )

            let lines =
                data
                |> List.mapi (fun i d -> 
                    let opaque = state.legend = Some d.name || state.position <> Stopped
                    d.data 
                    |> createLine i d.name opaque
                )

            let title =
                match state.position with
                | Position i -> 
                    props.data
                    |> List.item (i % (props.data |> List.length))
                    |> fst
                    |> sprintf "%s"
                | Stopped ->
                    match state.legend with
                    | Some s -> sprintf "%s" s
                    | None ->
                        sprintf "%s - %s"
                            (props.data |> List.head |> fst)
                            (props.data |> List.rev |> List.head |> fst)
                | First ->
                    sprintf "%s"
                        (props.data |> List.head |> fst)
                | Last ->
                    sprintf "%s"
                        (props.data |> List.rev |> List.head |> fst)
                |> sprintf "%s %s" props.title


            Html.div [
                Mui.toolbar [
                    toolbar.disableGutters true
                    toolbar.children [
                        Mui.typography [
                            prop.style [ style.flexGrow 1]
                            typography.color.primary
                            typography.variant.h6
                            prop.text title
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

                Recharts.lineChart [
                    lineChart.width 1100
                    lineChart.height 600

                    lineChart.children [
                        Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1) ]
                        Recharts.xAxis [ 
                            xAxis.dataKey (fun (x : Point) -> x.date)
                            xAxis.allowDuplicatedCategory false 
                            xAxis.category
                        ]
                        Recharts.yAxis [ 
                            yAxis.number
                            yAxis.dataKey (fun (x : Point) -> x.value) 
                            yAxis.domain (domain.constant 0, domain.constant max)
                        ]
                        Recharts.tooltip []
                        Recharts.legend [ 
                            legend.verticalAlign.bottom 
                            prop.onMouseEnter (fun e -> e?value |> EnterLegend |> dispatch)
                        ]
                        yield! lines
                    ]
                ]

                """Ga met de muis over de labels onderaan om de x-as om
                het gemiddelde of een jaar uit te lichten. Gebruik de bovenste 
                knoppen om door de jaren heen te lopen en een specifiek jaar te bekijken.
                """
                |> Markdown.render
            ]
        )


    let render title data = comp({| title = title; data = data |})
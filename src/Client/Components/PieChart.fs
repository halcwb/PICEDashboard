namespace Components

module PieChart =

    open Feliz
    open Feliz.UseElmish
    open Elmish
    open Feliz.MaterialUI
    open Fable.MaterialUI
    open Feliz.Recharts
//    open Informedica.PICE.Shared

    open System
    open Types

    type State = { period : Position }
    and Position = | Position of int | First | Last | Paused


    type Msg = | SkipLast | SkipFirst | Play | Pause


    let init () = { period = Paused }, Cmd.none

    
    let update msg state =
        match msg with
        | Pause -> { state with period = Paused }, Cmd.none
        | Play -> 
            { state with 
                period = 
                    match state.period with
                    | Position  i  -> i + 1 
                    | Paused       -> 0 
                    | First | Last -> 1
                    |> Position
            }, Cmd.none
        | SkipFirst ->
            { state with
                period = First
            }, Cmd.none
        | SkipLast ->
            { state with
                period = Last
            }, Cmd.none


    type PieSlice = { name : string; value : int; color : string }


    let createPieSlice name value color = { name = name; value = value; color = color }


    let colorKeyValueList xs =
        xs
        |> List.mapi (fun i (k, v) ->
            createPieSlice k v (Utils.getColor i)
        )
    
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
            let state, dispatch = React.useElmish(init, update, [||])

            let p, data =
                match state.period with
                | Paused   -> 
                    let start, end' = 
                        props.periods |> List.head |> fst, props.periods |> List.rev |> List.head |> fst
                    sprintf "%s - %s" start end', props.data
                | _ -> 
                    let i = 
                        match state.period with
                        | Position i -> i
                        | Last       -> (props.periods |> List.length) - 1
                        | _          -> 0

                    props.periods.[ i % (props.periods |> List.length) ]

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
                            pie.isAnimationActive false
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
                                Icons.skipPreviousIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> Play |> dispatch)
                            iconButton.children [
                                Icons.playArrowIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> SkipLast |> dispatch)
                            iconButton.children [
                                Icons.skipNextIcon []
                            ]
                        ]
                        Mui.iconButton [
                            prop.onClick (fun _ -> Pause |> dispatch)
                            iconButton.children [
                                Icons.pauseIcon []
                            ]
                        ]
                    ]
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

    let render title kvs periods = comp ({| title = title; data = kvs; periods = periods |})
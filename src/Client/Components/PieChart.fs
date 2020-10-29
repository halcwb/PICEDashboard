namespace Components

module PieChart =

    open Feliz
    open Feliz.MaterialUI
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types

    type PieSlice = { name : string; value : int }

    let bgColors = [|
        color.darkBlue
        color.darkGreen
        color.darkCyan
        color.darkGoldenRod
        color.darkViolet
        color.darkGray
        color.darkOliveGreen
        color.darkKhaki
        color.darkMagenta
        color.darkOrange
        color.darkOrchid
        color.darkSalmon
        color.darkSlateBlue
        color.darkSlateGray
        color.darkTurqouise
    |]


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
        React.functionComponent("piechart", fun (props: {| data: (string * int) list |}) ->
            let data =
                props.data
                |> List.map (fun (k, v)  ->
                    { name = k; value = v }
                )
                
            let cells =
                data
                |> List.mapi (fun index _ ->
                    Recharts.cell [
                        cell.fill bgColors.[ index % bgColors.Length ]
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
                        pie.children cells
                    ]
                ]
            ]
        
        )


    let render data = comp ({| data = data |})
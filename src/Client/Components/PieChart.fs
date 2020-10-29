namespace Components

module PieChart =

    open Feliz
    open Feliz.MaterialUI
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types


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
        React.functionComponent("piechart", fun (props: {| data: (string * int) list |}) -> 

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
                            pie.children cells
                        ]
                    ]
            ]

            Mui.grid [
                grid.container true
                grid.justify.spaceEvenly
                grid.alignItems.center
                grid.direction.row
                grid.children [
                    props.data
                    |> ColoredList.keyValueListToColoredItems
                    |> ColoredList.render

                    props.data 
                    |> colorKeyValueList
                    |> createPieChart
                ]
            ]
        )


    let render kvs = comp ({| data = kvs |})
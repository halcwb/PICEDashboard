namespace Components

module PieChart =

    open Feliz
    open Feliz.MaterialUI
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types

    type PieSlice = { name : string; value : int }

    let private comp =
        React.functionComponent("piechart", fun (props: {| data: (string * int) list |}) ->
            let data =
                props.data
                |> List.map (fun (k, v)  ->
                    { name = k; value = v }
                )
            Recharts.pieChart [
                pieChart.width  500
                pieChart.height 500
                pieChart.children [
                    Recharts.pie [
                        pie.data data
                        pie.dataKey (fun p -> p.value)
                    ]
                ]
            ]
        
        )


    let render data = comp ({| data = data |})
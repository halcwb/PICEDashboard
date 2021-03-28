namespace Components

module DiagnosesChart =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared
    open Informedica.PICE.Shared.Utils

    open System
    open Types

    type Point = { period : string; diagnoses : float; other : float }

    let private comp =
        React.functionComponent("diagnoses-chart", fun (props: {| selected : string list; data : Totals list |}) ->
            let find incl xs = 
                let t = 
                    xs 
                    |> List.map snd
                    |> List.sum
                    |> float
                xs
                |> List.filter (fun (k, v) ->
                    props.selected
                    |> List.exists ((=) k)
                    |> fun b -> if incl then b else not b
                )
                |> List.sumBy snd
                |> fun v ->
                    (v |> float) / t
                |> Math.round 2

            let data =
                props.data
                |> List.map (fun tot ->
                    { 
                        period = tot.Period
                        diagnoses = tot.Diagnoses |> find true
                        other = tot.Diagnoses |> find false
                    }
                )

            Recharts.barChart [
                barChart.width 1100
                barChart.height 500
                barChart.data data
                barChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1) ]
                    Recharts.xAxis [ xAxis.dataKey(fun p -> p.period)]
                    Recharts.yAxis []
                    Recharts.tooltip []
                    Recharts.legend [ legend.verticalAlign.bottom ]
                    Recharts.bar [
                        bar.name "geselecteerde diagnoses"
                        bar.dataKey (fun p -> p.diagnoses)
                        bar.stackId "a"
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "overige diagnoses"
                        bar.dataKey (fun p -> p.other)
                        bar.stackId "a"
                        bar.fill color.darkGreen
                    ]
                ]
            ]
        )


    let render selected totals = comp ({| selected = selected; data = totals |})
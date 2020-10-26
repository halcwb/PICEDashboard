namespace Components

module StackedGenderChart =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared
    open Informedica.PICE.Shared.Utils

    open System
    open Types

    type Point = { period : string; male : float; female : float; unknown : float }

    let private comp =
        React.functionComponent("stacked-gender-chart", fun (props: {| data : Totals list |}) ->
            let find g xs = 
                let t = 
                    xs 
                    |> List.map snd
                    |> List.sum
                    |> float
                xs
                |> List.tryFind (fst >> ((=) g))
                |> function
                | Some (_, v) -> (v |> float) / t
                | None -> 0.
                |> Math.round 2

            let data =
                props.data
                |> List.map (fun tot ->
                    { 
                        period = tot.Period
                        male = tot.Gender |> find "Man"
                        female = tot.Gender |> find "Vrouw"
                        unknown = tot.Gender |> find "Onbekend"
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
                        bar.name "Man"
                        bar.dataKey (fun p -> p.male)
                        bar.stackId "a"
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "Vrouw"
                        bar.dataKey (fun p -> p.female)
                        bar.stackId "a"
                        bar.fill color.darkGreen
                    ]
                    Recharts.bar [
                        bar.name "Onbekend"
                        bar.dataKey (fun p -> p.unknown)
                        bar.stackId "a"
                        bar.fill color.darkMagenta
                    ]
                ]
            ]
        )


    let render totals = comp ({| data = totals |})
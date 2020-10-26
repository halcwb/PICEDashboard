namespace Components

module StackedAgeChart =
    
    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types

    type Point = 
        { 
            period : string
            ``0 dagen - 4 weken`` : float
            ``1 maand - 1 jaar`` : float
            ``1 jaar - 4 jaar`` : float
            ``4 jaar - 12 jaar`` : float
            ``12 jaar - 16 jaar`` : float
            ``16 jaar - 18 jaar`` : float
            ``ouder dan 18 jaar`` : float
        }

    let private comp =
        React.functionComponent("stacked-age-chart", fun (props: {| data : Totals list |}) ->
            let find g xs = 
                xs
                |> List.tryFind (fst >> ((=) g))
                |> function
                | Some (_, v) -> v |> float
                | None -> 0.

            let data =
                props.data
                |> List.map (fun tot ->
                    let perc c = 
                        let t = 
                            tot.AgeGroup
                            |> List.map snd
                            |> List.sum
                            |> float
                        Math.Round(c / float t, 3, MidpointRounding.AwayFromZero)
//                        |> fun x -> if x > 100. then 100. else x

                    { 
                        period = tot.Period
                        ``0 dagen - 4 weken`` = tot.AgeGroup |> find "0 dagen - 4 weken" |> perc
                        ``1 maand - 1 jaar`` = tot.AgeGroup |> find "1 maand - 1 jaar" |> perc
                        ``1 jaar - 4 jaar`` = tot.AgeGroup |> find "1 jaar - 4 jaar" |> perc
                        ``4 jaar - 12 jaar`` = tot.AgeGroup |> find "4 jaar - 12 jaar" |> perc
                        ``12 jaar - 16 jaar`` = tot.AgeGroup |> find "12 jaar - 16 jaar" |> perc
                        ``16 jaar - 18 jaar`` = tot.AgeGroup |> find "16 jaar - 18 jaar" |> perc
                        ``ouder dan 18 jaar`` = tot.AgeGroup |> find "ouder dan 18 jaar" |> perc
                    }
                )

            Recharts.barChart [
                barChart.width 1100
                barChart.height 500
                barChart.data data
                barChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1) ]
                    Recharts.xAxis [ xAxis.dataKey(fun p -> p.period)]
                    Recharts.yAxis [  ]
                    Recharts.tooltip []
                    Recharts.legend [ legend.verticalAlign.bottom ]
                    Recharts.bar [
                        bar.name "0 dagen - 4 weken"
                        bar.dataKey (fun p -> p.``0 dagen - 4 weken``)
                        bar.stackId "a"
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "1 maand - 1 jaar"
                        bar.dataKey (fun p -> p.``1 maand - 1 jaar``)
                        bar.stackId "a"
                        bar.fill color.darkGreen
                    ]
                    Recharts.bar [
                        bar.name "1 jaar - 4 jaar"
                        bar.dataKey (fun p -> p.``1 jaar - 4 jaar``)
                        bar.stackId "a"
                        bar.fill color.darkMagenta
                    ]
                    Recharts.bar [
                        bar.name "4 jaar - 12 jaar"
                        bar.dataKey (fun p -> p.``4 jaar - 12 jaar``)
                        bar.stackId "a"
                        bar.fill color.darkCyan
                    ]
                    Recharts.bar [
                        bar.name "12 jaar - 16 jaar"
                        bar.dataKey (fun p -> p.``12 jaar - 16 jaar``)
                        bar.stackId "a"
                        bar.fill color.darkGoldenRod
                    ]
                    Recharts.bar [
                        bar.name "16 jaar - 18 jaar"
                        bar.dataKey (fun p -> p.``16 jaar - 18 jaar``)
                        bar.stackId "a"
                        bar.fill color.darkKhaki
                    ]
                    Recharts.bar [
                        bar.name "ouder dan 18 jaar"
                        bar.dataKey (fun p -> p.``ouder dan 18 jaar``)
                        bar.stackId "a"
                        bar.fill color.darkOliveGreen
                    ]

                ]
            ]
        )


    let render totals = comp ({| data = totals |})


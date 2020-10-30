namespace Components


module FunnelPlot =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared
    open Informedica.PICE.Shared.Utils

    open System
    open Types

    type Point = 
        { 
            name : string
            mortality : float
            smr : float
            reference : float
            upper : float
            lower : float
        }

    let private comp =
        React.functionComponent(fun (props : {| totals : Totals list |}) ->

            let pim2 =
                props.totals
                |> List.map (fun tot ->
                    { 
                        name = tot.Period
                        mortality = tot.Deaths |> float 
                        smr = (tot.Deaths |> float) / tot.PIM2Mortality
                        reference = 1.
                        upper = 0.
                        lower = 0.
                    }
                )

            let pim3 =
                props.totals
                |> List.map (fun tot ->
                    { 
                        name = tot.Period
                        mortality = tot.Deaths |> float 
                        smr = (tot.Deaths |> float) / tot.PIM3Mortality
                        reference = 1.
                        upper = 0.
                        lower = 0.
                    }
                )

            let prism =
                props.totals
                |> List.map (fun tot ->
                    { 
                        name = tot.Period
                        mortality = tot.Deaths |> float 
                        smr = (tot.Deaths |> float) / tot.PRISM4Mortality
                        reference = 1.
                        upper = 0.
                        lower = 0.
                    }
                )

            let data =
                let calcSD (p : Point) =
                    Math.Sqrt(1./ (p.mortality |> float))
                pim2 @ pim3 @ prism
                |> List.map (fun p ->
                    {
                        p with
                            name = "reference"
                            smr = 1.
                            upper = 1. + 1.96 * (calcSD p)
                            lower = 1. - 1.96 * (calcSD p)
                    }
                )
                |> List.distinct

            Recharts.composedChart [
                composedChart.width 1100
                composedChart.height 600
                composedChart.data data
                composedChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(5, 5)]
                    Recharts.xAxis [ xAxis.number; xAxis.dataKey (fun p -> p.mortality ); xAxis.name "mortiliteit"; xAxis.tickCount 5  ]
                    Recharts.yAxis [ yAxis.number; yAxis.dataKey (fun p -> p.smr); yAxis.name "smr" ]

                    Recharts.legend []

                    Recharts.scatter [
                        scatter.name "PIM-2"
                        scatter.data pim2
                        scatter.fill color.darkGreen
                    ]

                    Recharts.scatter [
                        scatter.name "PIM-3"
                        scatter.data pim3
                        scatter.fill color.darkMagenta
                    ]

                    Recharts.scatter [
                        scatter.name "PRISM"
                        scatter.data prism
                        scatter.fill color.darkCyan
                    ]

                    Recharts.line [
                        line.name "referentie"
                        line.monotone
                        line.dataKey (fun p -> p.reference)
                        line.strokeWidth 4
                        line.dot false
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkBlue
                    ]

                    Recharts.line [
                        line.name "boven grens"
                        line.monotone
                        line.dataKey (fun p -> p.upper)
                        line.strokeWidth 4
                        line.dot false
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkRed
                    ]

                    Recharts.line [
                        line.name "onder grens"
                        line.monotone
                        line.dataKey (fun p -> p.lower)
                        line.strokeWidth 4
                        line.dot false
                        line.activeDot false
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkGreen
                    ]
                ]
            ]
        )

    let render totals = comp({| totals = totals |})


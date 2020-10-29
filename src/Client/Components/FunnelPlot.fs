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
                    }
                )

            let pim3 =
                props.totals
                |> List.map (fun tot ->
                    { 
                        name = tot.Period
                        mortality = tot.Deaths |> float 
                        smr = (tot.Deaths |> float) / tot.PIM3Mortality
                    }
                )

            let prism =
                props.totals
                |> List.map (fun tot ->
                    { 
                        name = tot.Period
                        mortality = tot.Deaths |> float 
                        smr = (tot.Deaths |> float) / tot.PRISM4Mortality
                    }
                )

            Recharts.scatterChart [
                scatterChart.width 1100
                scatterChart.height 600
                
                scatterChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1)]
                    Recharts.xAxis [ xAxis.number; xAxis.dataKey (fun p -> p.mortality ); xAxis.name "mortiliteit" ]
                    Recharts.yAxis [ yAxis.number; yAxis.dataKey (fun p -> p.smr); yAxis.name "smr" ]
                    Recharts.tooltip [
                        tooltip.active true
//                        tooltip.content (fun o -> Browser.Dom.console.log("tooltip", o); Html.none)
                    ]
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
                ]
            ]
        )

    let render totals = comp({| totals = totals |})


namespace Components 

module MortalityGraph =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types

    type Point = 
        { 
            name : string
            mortality : float
            pim2 : float
            pim3: float 
            prism : float
            smrPRISM : float
            smrPIM2 : float
            smrPIM3 : float
        }

    //let useStyles = Styles.makeStyles(fun styles theme ->
    //    {|
    //        theme = theme
    //    |}
    //)

    let private comp =
        React.functionComponent(fun (props : {| totals : Totals list |}) ->
//            let classes = useStyles ()
//            Browser.Dom.console.log("color", classes.barColor)
            let round (n : int) (c : float) = Math.Round(c, n)

            let data =
                props.totals
                |> List.map (fun tot ->
                    let perc c = 
                        Math.Round(100. * c / float tot.Admissions, 1)
                    // Browser.Dom.console.log(tot.Year, tot.Totals.PRISM4Mortality)
                    { 
                        name = tot.Period
                        mortality = tot.Deaths |> float |> perc
                        pim2 = tot.PIM2Mortality |> perc
                        pim3 = tot.PIM3Mortality |> perc
                        prism = tot.PRISM4Mortality |> perc
                        smrPRISM = (tot.Deaths |> float) / tot.PRISM4Mortality |> round 2
                        smrPIM2 = (tot.Deaths |> float) / tot.PIM2Mortality |> round 2
                        smrPIM3 = (tot.Deaths |> float) / tot.PIM3Mortality |> round 2
                    }
                )

            Recharts.composedChart [
                barChart.width 1150
                barChart.height 700
                barChart.data data
                barChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1)]
                    Recharts.xAxis [ xAxis.dataKey (fun p -> p.name ) ]
                    Recharts.yAxis [ yAxis.yAxisId "left"; yAxis.minTickGap 10 ]
                    Recharts.yAxis [ yAxis.yAxisId "right"; yAxis.orientation.right; yAxis.tickCount 5 ]
                    Recharts.tooltip []
                    Recharts.bar [
                        bar.name "Mortaliteit"
                        bar.yAxisId "left"
                        bar.dataKey (fun p -> p.mortality)
                        bar.fill color.darkBlue
                        bar.fillOpacity 0.5
                    ]
                    Recharts.bar [
                        bar.name "PIM-2"
                        bar.yAxisId "left"
                        bar.dataKey (fun p -> p.pim2)
                        bar.fill color.darkGreen
                        bar.fillOpacity 0.5
                    ]
                    Recharts.bar [
                        bar.name "PIM-3"
                        bar.yAxisId "left"
                        bar.dataKey (fun p -> p.pim3)
                        bar.fill color.darkMagenta
                        bar.fillOpacity 0.5
                    ]
                    Recharts.bar [
                        bar.name "PRISM-4"
                        bar.yAxisId "left"
                        bar.dataKey (fun p -> p.prism)
                        bar.fill color.darkCyan
                        bar.fillOpacity 0.5
                    ]
                    Recharts.line [
                        line.name "SMR-PRISM"
                        line.yAxisId "right"
                        line.monotone
                        line.dataKey (fun p -> p.smrPRISM)
                        line.strokeWidth 4
                        line.stroke color.darkCyan
                    ]
                    Recharts.line [
                        line.name "SMR-PIM2"
                        line.yAxisId "right"
                        line.monotone
                        line.dataKey (fun p -> p.smrPIM2)
                        line.strokeWidth 4
                        line.stroke color.darkGreen
                    ]
                    Recharts.line [
                        line.name "SMR-PIM3"
                        line.yAxisId "right"
                        line.monotone
                        line.dataKey (fun p -> p.smrPIM3)
                        line.strokeWidth 4
                        line.stroke color.darkMagenta
                    ]

                    Recharts.legend [ legend.verticalAlign.top ]

                ]
            ]
    )

    let render totals = comp ({| totals = totals |})

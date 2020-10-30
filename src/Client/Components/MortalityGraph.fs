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
            average : float
            averagePIM2 : float
            averagePIM3 : float
            averagePRISM : float
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

            let calcAverage get = 
                props.totals
                |> Utils.calcAverage (fun t -> t.Admissions) get
                |> fun x -> x * 100. |> Utils.round 2

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
                        average = calcAverage (fun t -> t.Deaths |> float)
                        averagePIM2 = calcAverage (fun t -> t.PIM2Mortality)
                        averagePIM3 = calcAverage (fun t -> t.PIM3Mortality)
                        averagePRISM = calcAverage (fun t -> t.PRISM4Mortality)
                    }
                )

            Recharts.composedChart [
                barChart.width 1100
                barChart.height 700
                barChart.data data
                barChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1)]
                    Recharts.xAxis [ xAxis.dataKey (fun p -> p.name ) ]
                    Recharts.yAxis [ yAxis.minTickGap 10 ]
//                    Recharts.yAxis [ yAxis.yAxisId "right"; yAxis.orientation.right; yAxis.tickCount 5 ]
                    Recharts.tooltip []
                    Recharts.bar [
                        bar.name "Mortaliteit"
                        bar.dataKey (fun p -> p.mortality)
                        bar.fill color.darkBlue
//                        bar.fillOpacity 0.5
                    ]
                    Recharts.bar [
                        bar.name "PIM-2"
                        bar.dataKey (fun p -> p.pim2)
                        bar.fill color.darkGreen
//                        bar.fillOpacity 0.5
                    ]
                    Recharts.bar [
                        bar.name "PIM-3"
                        bar.dataKey (fun p -> p.pim3)
                        bar.fill color.darkMagenta
//                        bar.fillOpacity 0.5
                    ]
                    Recharts.bar [
                        bar.name "PRISM-4"
                        bar.dataKey (fun p -> p.prism)
                        bar.fill color.darkCyan
//                        bar.fillOpacity 0.5
                    ]
                    Recharts.line [
                        line.name "gemiddelde"
                        line.monotone
                        line.dot false
                        line.dataKey (fun p -> p.average)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkBlue
                    ]
                    Recharts.line [
                        line.name "gem. PIM2"
                        line.monotone
                        line.dot false
                        line.dataKey (fun p -> p.averagePIM2)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkGreen
                    ]
                    Recharts.line [
                        line.name "gem. PIM3"
                        line.monotone
                        line.dot false
                        line.dataKey (fun p -> p.averagePIM3)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkMagenta
                    ]
                    Recharts.line [
                        line.name "gem. PRISM"
                        line.monotone
                        line.dot false
                        line.dataKey (fun p -> p.averagePRISM)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 10 |]
                        line.stroke color.darkCyan
                    ]

                    Recharts.legend [ legend.verticalAlign.top ]

                ]
            ]
    )

    let render totals = comp ({| totals = totals |})

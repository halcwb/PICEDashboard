namespace Pages

module Graphs = 

    open Feliz
    open Feliz.MaterialUI
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
                    }
                )
            Recharts.barChart [
                barChart.width 1100
                barChart.height 500
                barChart.data data
                barChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1)]
                    Recharts.xAxis [ xAxis.dataKey (fun p -> p.name ) ]
                    Recharts.yAxis []
                    Recharts.tooltip []
                    Recharts.bar [
                        bar.name "Mortaliteit"
                        bar.dataKey (fun p -> p.mortality)
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "PIM-2"
                        bar.dataKey (fun p -> p.pim2)
                        bar.fill color.darkGreen
                    ]
                    Recharts.bar [
                        bar.name "PIM-3"
                        bar.dataKey (fun p -> p.pim3)
                        bar.fill color.darkMagenta
                    ]
                    Recharts.bar [
                        bar.name "PRISM-4"
                        bar.dataKey (fun p -> p.prism)
                        bar.fill color.darkCyan
                    ]
                    Recharts.legend []

                ]
            ]
    )

    let render totals = comp ({| totals = totals |})
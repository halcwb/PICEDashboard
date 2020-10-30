namespace Components 

module SMRGraph =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types

    type Point = 
        { 
            name : string
            //mortality : float
            //pim2 : float
            //pim3: float 
            //prism : float
            smrPIM2 : float
            smrPIM3 : float
            smrPRISM : float
            reference : float
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
            let round = Utils.round

            let calcSMR get1 get2 tot = 
                let obs = tot |> get1
                let est = tot |> get2
                (obs |> float) / est
                |> round 2

            let calcAverage get = 
                let calc t = 
                    calcSMR (fun t -> t.Deaths) get t

                props.totals
                |> Utils.calcAverage (fun _ -> 1) calc
                |> round 2

            let data =
                props.totals
                |> List.map (fun tot ->
                    let calc = calcSMR (fun t -> t.Deaths)
                    // Browser.Dom.console.log(tot.Year, tot.Totals.PRISM4Mortality)
                    { 
                        name = tot.Period
                        smrPRISM = tot |> calc (fun t -> t.PRISM4Mortality)
                        smrPIM2 =  tot |> calc (fun t -> t.PIM2Mortality)
                        smrPIM3 =  tot |> calc (fun t -> t.PIM3Mortality)
                        reference = 1.
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
                    Recharts.yAxis [ yAxis.tickCount 5 ]
                    Recharts.tooltip []

                    Recharts.line [
                        line.name "SMR-PRISM"
                        line.monotone
                        line.dataKey (fun p -> p.smrPRISM)
                        line.strokeWidth 4
                        line.stroke color.darkCyan
                    ]
                    Recharts.line [
                        line.name "SMR-PIM2"
                        line.monotone
                        line.dataKey (fun p -> p.smrPIM2)
                        line.strokeWidth 4
                        line.stroke color.darkGreen
                    ]
                    Recharts.line [
                        line.name "SMR-PIM3"
                        line.monotone
                        line.dataKey (fun p -> p.smrPIM3)
                        line.strokeWidth 4
                        line.stroke color.darkMagenta
                    ]
                    Recharts.line [
                        line.name "referentie"
                        line.dot false
                        line.monotone
                        line.dataKey (fun p -> p.reference)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 5 |]
                        line.stroke color.black
                    ]
                    Recharts.line [
                        line.name "gem. PRISM-IV"
                        line.dot false
                        line.monotone
                        line.dataKey (fun p -> p.averagePRISM)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 5 |]
                        line.stroke color.darkCyan
                    ]
                    Recharts.line [
                        line.name "gem. PIM-2"
                        line.dot false
                        line.monotone
                        line.dataKey (fun p -> p.averagePIM2)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 5 |]
                        line.stroke color.darkGreen
                    ]
                    Recharts.line [
                        line.name "gem. PIM-3"
                        line.dot false
                        line.monotone
                        line.dataKey (fun p -> p.averagePIM3)
                        line.strokeWidth 4
                        line.strokeDasharray [| 10; 5 |]
                        line.stroke color.darkMagenta
                    ]

                    Recharts.legend [ legend.verticalAlign.top ]
                ]
            ]
    )

    let render totals = comp ({| totals = totals |})

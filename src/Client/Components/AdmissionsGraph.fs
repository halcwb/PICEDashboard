namespace Components 

module AdmissionsGraph =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared

    open System
    open Types

    type Point = 
        { 
            name : string
            admitted : int
            discharged : int
            picuDays: int 
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
                        admitted = tot.Admissions 
                        discharged = tot.Discharged
                        picuDays = tot.PICUDays
                    }
                )
            Recharts.barChart [
                barChart.width 1100
                barChart.height 700
                barChart.data data
                barChart.children [
                    Recharts.cartesianGrid [ cartesianGrid.strokeDasharray(1, 1)]
                    Recharts.xAxis [ xAxis.dataKey (fun p -> p.name ) ]
                    Recharts.yAxis []
                    Recharts.tooltip []
                    Recharts.bar [
                        bar.name "Opnames"
                        bar.dataKey (fun p -> p.admitted)
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "Ontslagen"
                        bar.dataKey (fun p -> p.discharged)
                        bar.fill color.darkGreen
                    ]
                    Recharts.bar [
                        bar.name "Ligdagen"
                        bar.dataKey (fun p -> p.picuDays)
                        bar.fill color.darkMagenta
                    ]

                    Recharts.legend [ legend.verticalAlign.top ]

                ]
            ]
    )

    let render totals = comp ({| totals = totals |})

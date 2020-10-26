namespace Components

module StackedDischargeChart =

    open Feliz
    open Feliz.Recharts
    open Informedica.PICE.Shared
    open Informedica.PICE.Shared.Utils

    open System
    open Types

//  Klaar voor niet-ICU zorg	
//  Overleden	
//  Ontslag naar palliatieve zorg	
//  Gespecialiseerde zorg op andere afdeling	
//  Huidige (PICU) zorg voortgezet op andere afdeling	
//  Ontslag ten gevolge van plaatsgebrek op PICU	
//  Onbekend	
//  Ontslag wegens uitstel ingreep
    type Point = 
        { 
            period : string 
            nonPICU : float
            passedAway : float
            palliativeCare : float
            specialicedCare : float
            otherPICU : float
            wantOfSpace : float
            unknown : float
            delayed : float
        }

    let private comp =
        React.functionComponent("stacked-discharge-chart", fun (props: {| data : Totals list |}) ->
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
                        nonPICU = tot.DischargeReasons |> find "Klaar voor niet-ICU zorg"
                        passedAway =  tot.DischargeReasons |> find "Overleden"
                        palliativeCare = tot.DischargeReasons |> find "Ontslag naar palliatieve zorg"
                        specialicedCare = tot.DischargeReasons |> find "Gespecialiseerde zorg op andere afdeling"
                        otherPICU = tot.DischargeReasons |> find "Huidige (PICU) zorg voortgezet op andere afdeling"
                        wantOfSpace =  tot.DischargeReasons |> find "Ontslag ten gevolge van plaatsgebrek op PICU"
                        unknown = tot.DischargeReasons |> find "Onbekend"
                        delayed = tot.DischargeReasons |> find "Ontslag wegens uitstel ingreep"                    
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
                        bar.name "Niet IC Zorg"
                        bar.dataKey (fun p -> p.nonPICU)
                        bar.stackId "a"
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "Overleden"
                        bar.dataKey (fun p -> p.passedAway)
                        bar.stackId "a"
                        bar.fill color.darkGreen
                    ]
                    Recharts.bar [
                        bar.name "Palliative Zorg"
                        bar.dataKey (fun p -> p.palliativeCare)
                        bar.stackId "a"
                        bar.fill color.darkCyan
                    ]
                    Recharts.bar [
                        bar.name "Gespescialiseerde Zorg"
                        bar.dataKey (fun p -> p.specialicedCare)
                        bar.stackId "a"
                        bar.fill color.darkGoldenRod
                    ]
                    Recharts.bar [
                        bar.name "Andere (P)ICU"
                        bar.dataKey (fun p -> p.otherPICU)
                        bar.stackId "a"
                        bar.fill color.darkGray
                    ]
                    Recharts.bar [
                        bar.name "Plaatsgebrek"
                        bar.dataKey (fun p -> p.wantOfSpace)
                        bar.stackId "a"
                        bar.fill color.darkOliveGreen
                    ]
                    Recharts.bar [
                        bar.name "Onbekend"
                        bar.dataKey (fun p -> p.unknown)
                        bar.stackId "a"
                        bar.fill color.darkKhaki
                    ]
                    Recharts.bar [
                        bar.name "Uitgeslelde behandeling"
                        bar.dataKey (fun p -> p.delayed)
                        bar.stackId "a"
                        bar.fill color.darkMagenta
                    ]
                ]
            ]
        )


    let render totals = comp ({| data = totals |})
namespace Components

module StackedDiagnoseChart =


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
            cardioSurgery : float
            respiratory : float
            neuroSurgery : float
            miscellaneous : float
            cardiovascular : float
            orthopedic : float
            generalSurgery : float
            neurology : float
            ent : float
            injury : float
            thoracicSurgery : float
            anesthesia : float
            craniofacial : float
            gastroEntero : float
            renal : float
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
                | Some (_, v) -> (v |> float) // / t
                | None -> 0.
//                |> Math.round 3

            let data =
                props.data
                |> List.map (fun tot ->
                    { 
                        period = tot.Period
                        cardioSurgery = tot.DiagnoseGroups |> find "hartchirurgie"
                        respiratory =  tot.DiagnoseGroups |> find "respiratoir"
                        neuroSurgery = tot.DiagnoseGroups |> find "neurochirurgie"
                        miscellaneous = tot.DiagnoseGroups |> find "diversen"
                        cardiovascular = tot.DiagnoseGroups |> find "cardiovasculair"
                        orthopedic = tot.DiagnoseGroups |> find "orthoped. chir."
                        generalSurgery =  tot.DiagnoseGroups |> find "alg. chirurgie"
                        neurology = tot.DiagnoseGroups |> find "neurologisch"
                        ent = tot.DiagnoseGroups |> find "kno chirurgie"
                        injury = tot.DiagnoseGroups |> find "letsel"
                        thoracicSurgery = tot.DiagnoseGroups |> find "thoraxchirurgie"
                        anesthesia = tot.DiagnoseGroups |> find "chir.-div/anesth"
                        craniofacial = tot.DiagnoseGroups |> find "craniofaciale chir."
                        gastroEntero = tot.DiagnoseGroups |> find "mdl"
                        renal = tot.DiagnoseGroups |> find "renaal"
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
                        bar.name "hart chir."
                        bar.dataKey (fun p -> p.cardioSurgery)
                        bar.stackId "a"
                        bar.fill color.darkBlue
                    ]
                    Recharts.bar [
                        bar.name "respiratoir"
                        bar.dataKey (fun p -> p.respiratory)
                        bar.stackId "a"
                        bar.fill color.darkGreen
                    ]
                    Recharts.bar [
                        bar.name "neuro chir."
                        bar.dataKey (fun p -> p.neuroSurgery)
                        bar.stackId "a"
                        bar.fill color.darkCyan
                    ]
                    Recharts.bar [
                        bar.name "diversen"
                        bar.dataKey (fun p -> p.miscellaneous)
                        bar.stackId "a"
                        bar.fill color.darkGoldenRod
                    ]
                    Recharts.bar [
                        bar.name "cardiovasculair"
                        bar.dataKey (fun p -> p.cardiovascular)
                        bar.stackId "a"
                        bar.fill color.darkViolet
                    ]
                    Recharts.bar [
                        bar.name "ortho. chir."
                        bar.dataKey (fun p -> p.orthopedic)
                        bar.stackId "a"
                        bar.fill color.darkGray
                    ]
                    Recharts.bar [
                        bar.name "alg. chir."
                        bar.dataKey (fun p -> p.generalSurgery)
                        bar.stackId "a"
                        bar.fill color.darkOliveGreen
                    ]
                    Recharts.bar [
                        bar.name "neurologie"
                        bar.dataKey (fun p -> p.neurology)
                        bar.stackId "a"
                        bar.fill color.darkKhaki
                    ]
                    Recharts.bar [
                        bar.name "KNO"
                        bar.dataKey (fun p -> p.ent)
                        bar.stackId "a"
                        bar.fill color.darkMagenta
                    ]
                    Recharts.bar [
                        bar.name "letsel"
                        bar.dataKey (fun p -> p.injury)
                        bar.stackId "a"
                        bar.fill color.darkOrange
                    ]
                    Recharts.bar [
                        bar.name "thorax chir."
                        bar.dataKey (fun p -> p.thoracicSurgery)
                        bar.stackId "a"
                        bar.fill color.darkOrchid
                    ]
                    Recharts.bar [
                        bar.name "anesthesie"
                        bar.dataKey (fun p -> p.anesthesia)
                        bar.stackId "a"
                        bar.fill color.darkSalmon
                    ]
                    Recharts.bar [
                        bar.name "craniofaciaal"
                        bar.dataKey (fun p -> p.craniofacial)
                        bar.stackId "a"
                        bar.fill color.darkSlateBlue
                    ]
                    Recharts.bar [
                        bar.name "MDL"
                        bar.dataKey (fun p -> p.gastroEntero)
                        bar.stackId "a"
                        bar.fill color.darkSlateGray
                    ]
                    Recharts.bar [
                        bar.name "renaal"
                        bar.dataKey (fun p -> p.renal)
                        bar.stackId "a"
                        bar.fill color.darkTurqouise
                    ]
                ]
            ]
        )


    let render totals = comp ({| data = totals |})
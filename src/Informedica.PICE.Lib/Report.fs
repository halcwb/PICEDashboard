namespace Informedica.PICE.Lib

module Report =

    open System
    open System.Text

    open Statistics
    
    module Literals = Markdown.Literals

    type Report =
        {
            Sections : Section list
            Markdown : string
        }
    and Section = 
        {
            Title : string
            Groups : Group list
            Totals : (string * Totals) list
        }
    and Group = 
        {
            Title : string
            Items : Item list
        }
    and Item =
        {
            Title : string
            Content : string
        }

    let create (stats : Statistics) =
        let addItem section group title md report =
            { report with
                Sections =
                    report.Sections 
                    |> List.map (fun s ->
                        if s.Title <> section then s
                        else 
                            { s with
                                Groups =
                                    s.Groups
                                    |> List.map (fun g ->
                                        if g.Title <> group then g
                                        else
                                            { g with
                                                Items =
                                                    {
                                                        Title = title
                                                        Content = md
                                                    }
                                                    |> List.singleton
                                                    |> List.append g.Items            
                                            }
                                    )
                            }
                    )
            }


        let addGroup section title report =
            { report with
                Sections =
                    report.Sections 
                    |> List.map (fun s ->
                        if s.Title <> section then s
                        else 
                            { s with
                                Groups =
                                    {
                                        Title = title
                                        Items = []
                                    }
                                    |> List.singleton 
                                    |> List.append s.Groups                            
                            }
                    )
            }

        let addSection title totals report =
            { report with
                Sections =
                    {
                        Title = title 
                        Groups = []
                        Totals = totals
                    }
                    |> List.singleton
                    |> List.append report.Sections
            }

        let calcPerc t n  =
            try
                if t > 0 then
                    StringBuilder.builder ""
                    |> StringBuilder.appendFormat "{0:F0} ({1:F0}%)" [ n |> box; (100. * n / (t |> float)) |> box ]
                    |> StringBuilder.toString
                else
                    sprintf "%A" n 
            with 
            | e -> sprintf "error calcPerc %A %A\n%s" t n (e.ToString())
                   |> failwith

        let printCount kvs sort =
            StringBuilder.builder ""
            |> fun sb ->
                let t =
                    kvs
                    |> List.map snd
                    |> List.sum

                kvs
                |> fun xs -> if sort then xs |> List.sortByDescending snd else xs
                |> List.fold (fun acc (s, c) ->
                    let c = calcPerc t (float c)
                    acc
                    |> StringBuilder.appendLineFormat Literals.countItem [ s |> box; c |> box ]
                ) sb
            |> StringBuilder.toString

        let countToTable title get sb =
            let sb =
                sb
                |> StringBuilder.appendLine title

            stats.YearTotals
            |> List.map (fun ytot ->
                ytot.Year, ytot.Totals |> get
            )
            |> List.sortByDescending fst
            |> List.fold (fun acc (yr, xs) ->
                let calc c = 
                    calcPerc (xs |> List.sumBy snd) (c |> float)
                    |> box

                match acc with
                | [] -> 
                    let acc =
                        [ xs |> List.map (fst >> box) |> List.append [ "Jaar" |> box; ] ]
                    [  xs |> List.map (snd >> calc) |> List.append  [ yr |> box ] ]
                    |> List.append acc
                | _ ->
                    [  xs |> List.map (snd >> calc) |> List.append  [ yr |> box ] ]
                    |> List.append acc
            ) []
            |> Markdown.createMDTable sb
            |> StringBuilder.newLine

        let allYearTotals =
            StringBuilder.builder ""
            |> StringBuilder.appendLineFormat Literals.patTot [ stats.Totals.Patients |> box ]
            |> StringBuilder.appendLineFormat Literals.adsTot [ stats.Totals.Admissions |> box ]
            |> StringBuilder.appendLineFormat Literals.disTot [ stats.Totals.Discharged |> box ]
            |> StringBuilder.appendLineFormat Literals.dayTot [ stats.Totals.PICUDays |> box ]
            |> StringBuilder.appendLineFormat Literals.dthTot [ calcPerc stats.Totals.Patients (float stats.Totals.Deaths) |> box ]
            |> StringBuilder.appendLineFormat Literals.estPIM2 [ calcPerc stats.Totals.Patients stats.Totals.PIM2Mortality |> box ]
            |> StringBuilder.appendLineFormat Literals.estPIM3 [ calcPerc stats.Totals.Patients stats.Totals.PIM3Mortality |> box ]
            |> StringBuilder.appendLineFormat Literals.estPRISM [ calcPerc stats.Totals.Patients stats.Totals.PRISM4Mortality |> box ]
            |> StringBuilder.newLine2
            |> StringBuilder.appendLine "De getoonde mortaliteit in bovenstaande lijst is de totale mortaliteit"
            |> StringBuilder.toString

        let allYearTabel (stats : Statistics) =
            let caps =
                [
                    "Jaar"
                    "Patienten"
                    "Opnames"
                    "Ontslagen"
                    "Ligdagen"
                    "Overleden"
                    "PIM2 Mortaliteit"
                    "PIM3 Mortaliteit"
                    "PRISM4 Mortaliteit"
                ]
                |> List.map box

            StringBuilder.builder ""
            |> fun sb ->
                let sb =
                    sb
                    |> StringBuilder.appendLineFormat Literals.columns9 caps
                    |> StringBuilder.appendLine Literals.headers9
                stats.YearTotals
                |> List.sortByDescending (fun t -> t.Year)
                |> List.fold (fun acc stat ->
                    let calc = calcPerc stat.Totals.Patients
                    let vals =
                        [
                            stat.Year              |> box
                            stat.Totals.Patients   |> box
                            stat.Totals.Admissions |> box
                            stat.Totals.Discharged |> box
                            stat.Totals.PICUDays   |> box
                            calc (float stat.Totals.Deaths) |> box
                            calc stat.Totals.PIM2Mortality |> box
                            calc stat.Totals.PIM3Mortality |> box
                            calc stat.Totals.PRISM4Mortality |> box
                        ]
        
                    acc
                    |> StringBuilder.appendLineFormat Literals.columns9 vals
                ) sb
                |> fun sb ->
                    let t = stats.YearTotals |> List.sumBy (fun s -> s.Totals.Patients)
                    let calc = calcPerc t
                    let vals =
                        [
                            "Totalen"            |> box
                            stats.YearTotals |> List.sumBy (fun s -> s.Totals.Patients) |> box
                            stats.YearTotals |> List.sumBy (fun s -> s.Totals.Admissions) |> box
                            stats.YearTotals |> List.sumBy (fun s -> s.Totals.Discharged) |> box
                            stats.YearTotals |> List.sumBy (fun s -> s.Totals.PICUDays)   |> box
                            calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.Deaths) |> float) |> box
                            calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.PIM2Mortality)) |> box
                            calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.PIM3Mortality)) |> box
                            calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.PRISM4Mortality)) |> box
                        ]
                
                    sb
                    |> StringBuilder.appendLineFormat Literals.columns9 vals
                    |> StringBuilder.toString

        {
            Sections = [] 
            Markdown = stats |> Statistics.toMarkdown
        }
        |> addSection "Rapportage Alle Jaren" (stats.YearTotals |> List.map (fun yt -> yt.Year |> string, yt.Totals))
        |> addGroup "Rapportage Alle Jaren" "Validatie" 
        |> addItem "Rapportage Alle Jaren" "Validatie" "Totalen" (printCount stats.Totals.InvalidPatients true)
        |> addGroup "Rapportage Alle Jaren" "Opnames en Mortaliteit"
        |> addItem "Rapportage Alle Jaren" "Opnames en Mortaliteit" "Totalen" allYearTotals 
        |> addItem "Rapportage Alle Jaren" "Opnames en Mortaliteit" "Per Jaar" (allYearTabel stats)

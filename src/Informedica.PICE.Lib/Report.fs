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
            Totals : Totals
            // string = period to which totals belong
            PeriodTotals : (string * Totals) list
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

    let addSection title totals ptotals report =
        { report with
            Sections =
                {
                    Title = title 
                    Groups = []
                    Totals = totals
                    PeriodTotals = ptotals
                }
                |> List.singleton
                |> List.append report.Sections
        }


    let totalsTabel (tots : Totals) =
        [
            [ "Patienten" |> box; tots.Patients |> box  ]
            [ "Opnames" |> box; tots.Admissions |> box ]
            [ "Ontslagen" |> box; tots.Discharged |> box ]
            [ "Verpleegdagen" |> box; tots.PICUDays |> box ]
            [ "Mortaliteit" |> box; calcPerc tots.Patients (float tots.Deaths) |> box ]
            [ "PIM-2" |> box; calcPerc tots.Patients tots.PIM2Mortality |> box ]
            [ "PIM-3" |> box; calcPerc tots.Patients tots.PIM3Mortality |> box ]
            [ "PRISM-IV" |> box; calcPerc tots.Patients tots.PRISM4Mortality |> box ]

        ]
        |> List.append [ [ "" |> box; "Aantal" ] ]
        |> Markdown.createMDTable (StringBuilder.builder "")
        |> StringBuilder.toString


    let create (stats : Statistics) =
        let calcPerc = Statistics.calcPerc

        let printCount kvs sort = 
            Statistics.printCount "" kvs sort (StringBuilder.builder "")
            |> StringBuilder.toString

        let countToTable tots get1 get2 = 
            Statistics.countToTable tots get1 get2 (StringBuilder.builder "")
            |> StringBuilder.toString

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

        let printTotals totals = 
            StringBuilder.builder ""
            |> Statistics.printTotals totals
            |> StringBuilder.toString

        let allYearTabel (stats : Statistics) =

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

        let printMonthTabel ytot = 
            StringBuilder.builder ""
            |> Statistics.printMonthTabel ytot
            |> StringBuilder.toString

        let addYearSection (yTot : YearTotals) report =
            let sectionTitle = sprintf "Rapportage %i" yTot.Year
            report
            |> addSection sectionTitle yTot.Totals (yTot.MonthTotals |> List.map (fun mt -> mt.Month |> string, mt.Totals))
            |> addGroup sectionTitle "Opnames en Mortaliteit"
            |> addItem sectionTitle "Opnames en Mortaliteit" "Totalen" (totalsTabel yTot.Totals) 
            |> addItem sectionTitle "Opnames en Mortaliteit" "Per Maand" (printMonthTabel yTot)
            |> addGroup sectionTitle "Geslacht"
            |> addItem sectionTitle "Geslacht" "Totalen"  (printCount yTot.Totals.Gender true)
            |> addGroup sectionTitle "Leeftijd"
            |> addItem sectionTitle "Leeftijd" "Totalen"  (printCount yTot.Totals.AgeGroup false)
            |> addGroup sectionTitle "PICU Ontslagreden"
            |> addItem sectionTitle "PICU Ontslagreden" "Totalen"  (printCount yTot.Totals.DischargeReasons true)
            |> addGroup sectionTitle "Diagnose Groepen"
            |> addItem sectionTitle "Diagnose Groepen" "Totalen"  (printCount yTot.Totals.DiagnoseGroups true)


        {
            Sections = [] 
            Markdown = stats |> Statistics.toMarkdown
        }
        |> addSection "Rapportage Alle Jaren" stats.Totals (stats.YearTotals |> List.map (fun yt -> yt.Year |> string, yt.Totals))
        |> addGroup "Rapportage Alle Jaren" "Validatie" 
        |> addItem "Rapportage Alle Jaren" "Validatie" "Totalen" (printCount stats.Totals.InvalidPatients true)
        |> addGroup "Rapportage Alle Jaren" "Opnames en Mortaliteit"
        |> addItem "Rapportage Alle Jaren" "Opnames en Mortaliteit" "Totalen" (stats.Totals |> totalsTabel) 
        |> addItem "Rapportage Alle Jaren" "Opnames en Mortaliteit" "Per Jaar" (allYearTabel stats)
        |> addGroup "Rapportage Alle Jaren" "Geslacht"
        |> addItem "Rapportage Alle Jaren" "Geslacht" "Totalen"  (printCount stats.Totals.Gender true)
        |> addItem "Rapportage Alle Jaren" "Geslacht" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.Gender))
        |> addGroup "Rapportage Alle Jaren" "Leeftijd"
        |> addItem "Rapportage Alle Jaren" "Leeftijd" "Totalen"  (printCount stats.Totals.AgeGroup false)
        |> addItem "Rapportage Alle Jaren" "Leeftijd" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.AgeGroup))
        |> addGroup "Rapportage Alle Jaren" "PICU Ontslagreden"
        |> addItem "Rapportage Alle Jaren" "PICU Ontslagreden" "Totalen"  (printCount stats.Totals.DischargeReasons true)
        |> addItem "Rapportage Alle Jaren" "PICU Ontslagreden" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DischargeReasons))
        |> addGroup "Rapportage Alle Jaren" "Diagnose Groepen" 
        |> addItem "Rapportage Alle Jaren" "Diagnose Groepen" "Totalen" (printCount stats.Totals.DiagnoseGroups true)
        |> addItem "Rapportage Alle Jaren" "Diagnose Groepen" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DiagnoseGroups))
        |> fun report ->
            stats.YearTotals
            |> List.sortByDescending (fun ytot -> ytot.Year)
            |> List.fold (fun report ytot ->
                addYearSection ytot report
            ) report
        
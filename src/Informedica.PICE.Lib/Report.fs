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
            |> addSection sectionTitle (yTot.MonthTotals |> List.map (fun mt -> mt.Month |> string, mt.Totals))
            |> addGroup sectionTitle "Validatie" 
            |> addItem sectionTitle "Validatie" "Totalen" (printCount yTot.Totals.InvalidPatients true)
            |> addGroup sectionTitle "Opnames en Mortaliteit"
            |> addItem sectionTitle "Opnames en Mortaliteit" "Totalen" (printTotals yTot.Totals) 
            |> addItem sectionTitle "Opnames en Mortaliteit" "Per Maand" (printMonthTabel yTot)
            |> addGroup sectionTitle "Geslacht"
            |> addItem sectionTitle "Geslacht" "Totalen"  (printCount yTot.Totals.Gender true)
            |> addGroup sectionTitle "Leeftijd"
            |> addItem sectionTitle "Leeftijd" "Totalen"  (printCount yTot.Totals.AgeGroup true)
            |> addGroup sectionTitle "PICU Ontslagreden"
            |> addItem sectionTitle "PICU Ontslagreden" "Totalen"  (printCount yTot.Totals.DischargeReasons true)


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
        |> addGroup "Rapportage Alle Jaren" "Geslacht"
        |> addItem "Rapportage Alle Jaren" "Geslacht" "Totalen"  (printCount stats.Totals.Gender true)
        |> addItem "Rapportage Alle Jaren" "Geslacht" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.Gender))
        |> addGroup "Rapportage Alle Jaren" "Leeftijd"
        |> addItem "Rapportage Alle Jaren" "Leeftijd" "Totalen"  (printCount stats.Totals.AgeGroup true)
        |> addItem "Rapportage Alle Jaren" "Leeftijd" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.AgeGroup))
        |> addGroup "Rapportage Alle Jaren" "PICU Ontslagreden"
        |> addItem "Rapportage Alle Jaren" "PICU Ontslagreden" "Totalen"  (printCount stats.Totals.DischargeReasons true)
        |> addItem "Rapportage Alle Jaren" "PICU Ontslagreden" "Per Jaar" (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DischargeReasons))
        |> fun report ->
            stats.YearTotals
            |> List.sortByDescending (fun ytot -> ytot.Year)
            |> List.fold (fun report ytot ->
                addYearSection ytot report
            ) report
        
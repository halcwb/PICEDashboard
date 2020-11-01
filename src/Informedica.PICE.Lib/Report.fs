﻿namespace Informedica.PICE.Lib

module Report =

    open System
    open System.Text

    open Statistics
    
    module Literals = Markdown.Literals

    module Literals =

        [<Literal>]
        let sectionPICE = "PICE Rapport"
        [<Literal>]
        let groupValidation = "Validatie"
        [<Literal>]
        let groupOverview = "Overzicht"
        [<Literal>]
        let groupMortality = "Mortaliteit"
        [<Literal>]
        let groupAdmission = "Opname"
        [<Literal>]
        let groupGender = "Geslacht"
        [<Literal>]
        let groupAge = "Leeftijd"
        [<Literal>]
        let groupDischargeReason = "Ontslagreden"
        [<Literal>]
        let groupDiagnoseGroup = "Diagnose Groep"
        [<Literal>]
        let paragraphTotals = "Totalen"
        [<Literal>]
        let paragraphPerYear = "Per Jaar"
        [<Literal>]
        let paragraphPerMonth = "Per Maand"
        [<Literal>]
        let capYear = "Jaar"
        [<Literal>]
        let capPatient = "Patienten"
        [<Literal>]
        let capAdmission = "Opnames"
        [<Literal>]
        let capDischarge = "Ontslagen"
        [<Literal>]
        let capBedDays = "Ligdagen"
        [<Literal>]
        let capMortality = "Mortaliteit"
        [<Literal>]
        let capPIM2 = "PIM-2"
        [<Literal>]
        let capPIM3 = "PIM-3"
        [<Literal>]
        let capPRISM = "PRISM-IV"



    type Report =
        {
            Sections : Section list
            Markdown : string
        }
    and Section = 
        {
            Title : string
            Chapters : Chapter list
            Totals : Totals
            YearTotals : Totals list
            // string = year to which totals belong
            MonthTotals : (string * (Totals list)) list
        }
    and Chapter = 
        {
            Title : string
            Paragraphs : Paragraph list
            SubChapters : SubChapter list
        }
    and SubChapter = 
        {
            Title : string
            Paragraphs : Paragraph list
        }
    and Paragraph =
        {
            Title : string
            Content : string
        }

    let addSubParagraph sectionTitle chapterTitle subTitle title md report =
        { report with
            Sections =
                report.Sections 
                |> List.map (fun s ->
                    if s.Title <> sectionTitle then s
                    else 
                        { s with
                            Chapters =
                                s.Chapters
                                |> List.map (fun c ->
                                    if c.Title <> chapterTitle then c
                                    else
                                        { c with
                                            SubChapters =
                                                c.SubChapters
                                                |> List.map (fun sc ->
                                                    if sc.Title <> subTitle then sc
                                                    else 
                                                        { sc with
                                                            Paragraphs =
                                                                {
                                                                    Title = title
                                                                    Content = md
                                                                }
                                                                |> List.singleton
                                                                |> List.append sc.Paragraphs            
                                                        }

                                                )
                                        }
                                )
                        }
                )
        }

    let addParagraph sectionTitle chapterTitle title md report =
        { report with
            Sections =
                report.Sections 
                |> List.map (fun s ->
                    if s.Title <> sectionTitle then s
                    else 
                        { s with
                            Chapters =
                                s.Chapters
                                |> List.map (fun c ->
                                    if c.Title <> chapterTitle then c
                                    else
                                        { c with
                                            Paragraphs =
                                                {
                                                    Title = title
                                                    Content = md
                                                }
                                                |> List.singleton
                                                |> List.append c.Paragraphs    
                                        }
                                )
                        }
                )
        }


    let addSubChapter sectionTitle chapterTitle title report =
        { report with
            Sections =
                report.Sections 
                |> List.map (fun c ->
                    if c.Title <> sectionTitle then c
                    else 
                        { c with
                            Chapters =
                                c.Chapters
                                |> List.map (fun c ->
                                    if c.Title <> chapterTitle then c
                                    else 
                                        {
                                            c with
                                                SubChapters =
                                                    {
                                                        Title = title
                                                        Paragraphs = []
                                                    }
                                                    |> List.singleton 
                                                    |> List.append c.SubChapters                            

                                        }
                                )
                        }
                )
        }

    let addChapter section title report =
        { report with
            Sections =
                report.Sections 
                |> List.map (fun s ->
                    if s.Title <> section then s
                    else 
                        { s with
                            Chapters =
                                {
                                    Title = title
                                    Paragraphs = []
                                    SubChapters = []
                                }
                                |> List.singleton 
                                |> List.append s.Chapters                            
                        }
                )
        }

    let addSection title totals yrTots moTots report =
        { report with
            Sections =
                {
                    Title = title 
                    Chapters = []
                    Totals = totals
                    YearTotals = yrTots
                    MonthTotals = moTots
                }
                |> List.singleton
                |> List.append report.Sections
        }


    let totalsTabel (tots : Totals) =
        [
            [ Literals.capPatient |> box; tots.Patients |> box  ]
            [ Literals.capAdmission |> box; tots.Admissions |> box ]
            [ Literals.capDischarge |> box; tots.Discharged |> box ]
            [ Literals.capBedDays |> box; tots.PICUDays |> box ]
            [ Literals.capMortality |> box; calcPerc tots.Patients (float tots.Deaths) |> box ]
            [ Literals.capPIM2 |> box; calcPerc tots.Patients tots.PIM2Mortality |> box ]
            [ Literals.capPIM3 |> box; calcPerc tots.Patients tots.PIM3Mortality |> box ]
            [ Literals.capPRISM |> box; calcPerc tots.Patients tots.PRISM4Mortality |> box ]

        ]
        |> List.append [ [ "Parameter" |> box; "Aantal" ] ]
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
                Literals.capYear
                Literals.capPatient
                Literals.capAdmission
                Literals.capDischarge
                Literals.capBedDays
                Literals.capMortality
                Literals.capPIM2
                Literals.capPIM3
                Literals.capPRISM
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
                            Literals.paragraphTotals            |> box
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
            |> addSection sectionTitle yTot.Totals (yTot.MonthTotals |> List.map (fun mt -> mt.Totals)) []
            |> addChapter sectionTitle Literals.groupOverview
            |> addParagraph sectionTitle Literals.groupOverview Literals.paragraphTotals (totalsTabel yTot.Totals) 
            |> addParagraph sectionTitle Literals.groupOverview Literals.paragraphPerMonth (printMonthTabel yTot)
            |> addChapter sectionTitle Literals.groupGender
            |> addParagraph sectionTitle Literals.groupGender Literals.paragraphTotals  (printCount yTot.Totals.Gender true)
            |> addChapter sectionTitle Literals.groupAge
            |> addParagraph sectionTitle Literals.groupAge Literals.paragraphTotals  (printCount yTot.Totals.AgeGroup false)
            |> addChapter sectionTitle Literals.groupDischargeReason
            |> addParagraph sectionTitle Literals.groupDischargeReason Literals.paragraphTotals  (printCount yTot.Totals.DischargeReasons true)
            |> addChapter sectionTitle Literals.groupDiagnoseGroup
            |> addParagraph sectionTitle Literals.groupDiagnoseGroup Literals.paragraphTotals  (printCount yTot.Totals.DiagnoseGroups true)

        let yrTots, moTots =
            stats.YearTotals
            |> List.map (fun yt -> yt.Totals) ,
            stats.YearTotals
            |> List.map (fun yt -> yt.Year |> string, yt.MonthTotals |> List.map (fun mt -> mt.Totals))


        {
            Sections = [] 
            Markdown = stats |> Statistics.toMarkdown
        }
        |> addSection Literals.sectionPICE stats.Totals yrTots moTots
        |> addChapter Literals.sectionPICE Literals.groupValidation 
        |> addParagraph Literals.sectionPICE Literals.groupValidation Literals.paragraphTotals (printCount stats.Totals.InvalidPatients true)
        |> addChapter Literals.sectionPICE Literals.groupOverview
        |> addParagraph Literals.sectionPICE Literals.groupOverview Literals.paragraphTotals (stats.Totals |> totalsTabel) 
        |> addParagraph Literals.sectionPICE Literals.groupOverview Literals.paragraphPerYear (allYearTabel stats)
        |> addChapter Literals.sectionPICE Literals.groupMortality
        |> addParagraph Literals.sectionPICE Literals.groupMortality Literals.paragraphPerYear ""
        |> addChapter Literals.sectionPICE Literals.groupAdmission
        |> addParagraph Literals.sectionPICE Literals.groupAdmission Literals.paragraphPerYear ""
        |> addChapter Literals.sectionPICE Literals.groupGender
        |> addParagraph Literals.sectionPICE Literals.groupGender Literals.paragraphTotals  (printCount stats.Totals.Gender true)
        |> addParagraph Literals.sectionPICE Literals.groupGender Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.Gender))
        |> addChapter Literals.sectionPICE Literals.groupAge
        |> addParagraph Literals.sectionPICE Literals.groupAge Literals.paragraphTotals  (printCount stats.Totals.AgeGroup false)
        |> addParagraph Literals.sectionPICE Literals.groupAge Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.AgeGroup))
        |> addChapter Literals.sectionPICE Literals.groupDischargeReason
        |> addParagraph Literals.sectionPICE Literals.groupDischargeReason Literals.paragraphTotals  (printCount stats.Totals.DischargeReasons true)
        |> addParagraph Literals.sectionPICE Literals.groupDischargeReason Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DischargeReasons))
        |> addChapter Literals.sectionPICE Literals.groupDiagnoseGroup 
        |> addParagraph Literals.sectionPICE Literals.groupDiagnoseGroup Literals.paragraphTotals (printCount stats.Totals.DiagnoseGroups true)
        |> addParagraph Literals.sectionPICE Literals.groupDiagnoseGroup Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DiagnoseGroups))
        
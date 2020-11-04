namespace Informedica.PICE.Lib

module Report =

    open System
    open System.Text

    open Statistics
    open Informedica.PICE.Shared


    module Literals = Markdown.Literals


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
            MonthTotals : (string * (Totals list)) list
        }
    and Chapter = 
        {
            Title : string
            Paragraphs : Paragraph list
            Chapters : Chapter list
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
                                            Chapters =
                                                c.Chapters
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
                                                Chapters =
                                                    {
                                                        Title = title
                                                        Chapters = []
                                                        Paragraphs = []
                                                    }
                                                    |> List.singleton 
                                                    |> List.append c.Chapters                            

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
                                    Chapters = []
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
        |> addParagraph Literals.sectionPICE Literals.groupMortality Literals.paragraphPIMandPRISM "Op de x-as staan de actuele en geschatte mortaliteit"
        |> addParagraph Literals.sectionPICE Literals.groupMortality Literals.paragraphSMR ""
        |> addChapter Literals.sectionPICE Literals.groupPatient
        |> addSubChapter Literals.sectionPICE Literals.groupPatient Literals.groupGender
        |> addSubParagraph Literals.sectionPICE Literals.groupPatient Literals.groupGender Literals.paragraphTotals  (printCount stats.Totals.Gender true)
        |> addSubParagraph Literals.sectionPICE Literals.groupPatient Literals.groupGender Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.Gender))
        |> addSubChapter Literals.sectionPICE Literals.groupPatient Literals.groupAge
        |> addSubParagraph Literals.sectionPICE Literals.groupPatient Literals.groupAge Literals.paragraphTotals  (printCount stats.Totals.AgeGroup false)
        |> addSubParagraph Literals.sectionPICE Literals.groupPatient Literals.groupAge Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.AgeGroup))
        |> addChapter Literals.sectionPICE Literals.groupAdmission
        |> addParagraph Literals.sectionPICE Literals.groupAdmission Literals.paragraphAdmDisch ""
        |> addParagraph Literals.sectionPICE Literals.groupAdmission Literals.paragraphOccupancy ""
        |> addParagraph Literals.sectionPICE Literals.groupAdmission Literals.paragraphUrgency ""
        |> addChapter Literals.sectionPICE Literals.groupDischarge
        |> addSubChapter Literals.sectionPICE Literals.groupDischarge Literals.groupDischargeReason
        |> addSubParagraph Literals.sectionPICE Literals.groupDischarge Literals.groupDischargeReason Literals.paragraphTotals  (printCount stats.Totals.DischargeReasons true)
        |> addSubParagraph Literals.sectionPICE Literals.groupDischarge Literals.groupDischargeReason Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DischargeReasons))
        |> addChapter Literals.sectionPICE Literals.groupDiagnose 
        |> addSubChapter Literals.sectionPICE  Literals.groupDiagnose Literals.groupDiagnoseGroup 
        |> addSubParagraph Literals.sectionPICE  Literals.groupDiagnose Literals.groupDiagnoseGroup Literals.paragraphTotals (printCount stats.Totals.DiagnoseGroups true)
        |> addSubParagraph Literals.sectionPICE  Literals.groupDiagnose Literals.groupDiagnoseGroup Literals.paragraphPerYear (countToTable stats.YearTotals (fun tot -> tot.Year) (fun tot -> tot.Totals.DiagnoseGroups))
        |> addChapter Literals.sectionPICE Literals.groupVentilation
        |> addSubChapter Literals.sectionPICE Literals.groupVentilation  Literals.subGroupCanule 
        |> addSubParagraph Literals.sectionPICE  Literals.groupVentilation Literals.subGroupCanule Literals.paragraphTotals ""
        |> addChapter Literals.sectionPICE  "Complicaties" 
        |> addParagraph Literals.sectionPICE "Complicaties" "Volgt nog" ""
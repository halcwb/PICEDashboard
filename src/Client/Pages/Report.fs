namespace Pages


module Report =

    open System

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop
    open Feliz.Markdown
    
    open Informedica.PICE.Shared
    open Informedica.PICE.Shared.Types
    open Types
    open Components

    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            print = styles.create [
                style.marginBottom (theme.spacing 2)
                style.paddingTop (theme.spacing 1)
                style.color (theme.palette.primary.dark)
            ]

            div = styles.create [
                style.backgroundColor theme.palette.primary.main
                style.marginBottom (theme.spacing 1)
            ]

            section = styles.create [
                style.backgroundColor theme.palette.primary.main
                style.color color.white
            ]

            group = styles.create [
                style.backgroundColor Colors.lightBlue.``50``
                style.color theme.palette.primary.main
            ]
        |}
    )

    module String =
        open System

        let split (s : string) = 
            s.Split('.') 
            |> Array.toList

        let split2 (s: String) =
            s.Split('|') 
            |> Array.toList
            

    let selectReport (s : string) (report : Report) =
        Browser.Dom.console.log("selecting", s)

        let rec selectChapter (ids : string list) (chapters : Chapter list) =
            Browser.Dom.console.log("select chapter", ids |> String.concat ", ", chapters |> List.map (fun c -> c.Title) |>  String.concat ", ")
            match ids with
            | id::tail ->
                match id |> String.split2 with
                | [s; id] when s = "C" ->
                    let chapter = chapters.[id |> int]
                    Browser.Dom.console.log("selected chapter: ", chapter.Title)
                    match tail with
                    | [] -> [ chapter ]
                    | [ id ] ->
                        Browser.Dom.console.log("Finishing with: ", id)
                        match id |> String.split2 with
                        | [s; id] when s = "C" -> 
                            [ { chapter with Chapters = [ chapter.Chapters.[id |> int] ]; Paragraphs = [] } ]
                        | [s; id] when s = "P" -> 
                            Browser.Dom.console.log("Picked paragraph", id)
                            [ { chapter with Paragraphs = [ chapter.Paragraphs.[id |> int] ]; Chapters = [] } ]
                        | _ -> 
                            sprintf "failwith couldn't get %s" id |> failwith
                    | _ ->
                        [
                            { chapter with
                                Paragraphs = []
                                Chapters = (chapter.Chapters |> selectChapter tail)
                            }
                        ]
                | _ ->
                    sprintf "failwith couldn't get %s" id |> failwith
            |  _ -> chapters

        match s |> String.split with
        | [ id ] -> 
            { report with
                Sections =
                    report.Sections.[id |> int]
                    |> List.singleton
            }
        | id::tail -> 
            { report with
                Sections =
                    report.Sections.[id |> int]
                    |> fun section -> 
                            {
                                section with 
                                    Chapters = 
                                        section.Chapters
                                        |> selectChapter tail
                            }
                    |> List.singleton
            }

        | _ -> report

    let getStackedBarChart s title get =
        let perYr =
            s.YearTotals
            |> List.map (fun t ->
                t.Period, t |> get
            )
        let perMo = 
            s.MonthTotals
            |> List.map (fun (yr, xs) ->
                yr, 
                xs
                |> List.map(fun t -> 
                    t.Period, t |> get
                )
            )

        Components.StackedBarChart.render title perYr perMo

    let layoutDetails className (dt : DisplayType) (section : Section) =

        let mapParagraph (chapter : Chapter) paragraph =
            Html.div [
                prop.style [ style.paddingBottom 20 ]

                match dt with
                | Graph when chapter.Title = Literals.groupMortality && 
                                paragraph.Title = Literals.paragraphPIMandPRISM -> 
                    prop.children [
                        paragraph.Title |> sprintf "#### %s" |> Markdown.render
                        "##### Mortaliteit" |> Markdown.render
                        section.YearTotals |> Components.MortalityGraph.render paragraph.Content
                    ]

                | Graph when chapter.Title = Literals.groupMortality && 
                                paragraph.Title = Literals.paragraphSMR -> 
                    prop.children [
                        paragraph.Title |> sprintf "#### %s" |> Markdown.render
                        "##### SMR per Jaar" |> Markdown.render
                        section.YearTotals |> Components.SMRGraph.render
                        "##### SMR Funnelplot " |> Markdown.render
                        section.YearTotals |> Components.FunnelPlot.render
                    ]

                | Graph when chapter.Title = Literals.groupAdmission && 
                                paragraph.Title = Literals.paragraphAdmDisch ->
                    prop.children [
                        "#### Opnames/ontslagen en ligdagen" |> Markdown.render
                        section.YearTotals |> Components.AdmissionsGraph.render                                    
                    ]


                | Graph when chapter.Title = Literals.groupAdmission && 
                                paragraph.Title = Literals.paragraphOccupancy ->
                    prop.children [
            
                        section.YearTotals 
                        |> List.map (fun ytot ->
                            ytot.Period
                            , ytot.Occupancy
                        )
                        |> OccupancyGraph.render paragraph.Title
                    ]
                
                | Graph when chapter.Title = Literals.groupAdmission && 
                                paragraph.Title = Literals.paragraphUrgency ->
                    prop.children [
                        (fun t -> t.Urgency)
                        |> getStackedBarChart section "Opname Urgentie"
                    ]

                | Graph when chapter.Title = Literals.subGroupTransportHospital &&
                             paragraph.Title = Literals.paragraphTotals ->
                        prop.children [
                            section.YearTotals
                            |> List.map (fun t -> t.Period, t.TransportHospital)
                            |> Components.PieChart.render paragraph.Title section.Totals.TransportHospital
                        ]

                | Graph when chapter.Title = Literals.subGroupTransportHospital &&
                             paragraph.Title = Literals.paragraphPerYear ->
                        prop.children [
                            (fun t -> t.TransportHospital)
                            |> getStackedBarChart section paragraph.Title
                        ]

                | Graph when chapter.Title = Literals.subGroupTransportTeam &&
                             paragraph.Title = Literals.paragraphTotals ->
                        prop.children [
                            section.YearTotals
                            |> List.map (fun t -> t.Period, t.TransportTeam)
                            |> Components.PieChart.render paragraph.Title section.Totals.TransportTeam
                        ]

                | Graph when chapter.Title = Literals.subGroupTransportTeam &&
                             paragraph.Title = Literals.paragraphPerYear ->
                        prop.children [
                            (fun t -> t.TransportTeam)
                            |> getStackedBarChart section paragraph.Title
                        ]

                | Graph when chapter.Title = Literals.groupGender && 
                                paragraph.Title = Literals.paragraphTotals ->
                    prop.children [ 
                        section.YearTotals
                        |> List.map (fun t ->
                            t.Period, t.Gender
                        )
                        |> Components.PieChart.render paragraph.Title section.Totals.Gender
                    ]

                | Graph when chapter.Title = Literals.groupGender && 
                                paragraph.Title = Literals.paragraphPerYear ->
                    prop.children [
                        (fun t -> t.Gender)
                        |> getStackedBarChart section paragraph.Title
                    ]

                | Graph when chapter.Title = Literals.groupAge && 
                                paragraph.Title = Literals.paragraphTotals ->
                    prop.children [ 
                        section.YearTotals
                        |> List.map (fun t ->
                            t.Period, t.AgeGroup
                        )
                        |> Components.PieChart.render paragraph.Title section.Totals.AgeGroup
                    ]

                | Graph when chapter.Title = Literals.groupAge && 
                                paragraph.Title = Literals.paragraphPerYear ->
                    prop.children [
                        (fun t -> t.AgeGroup)
                        |> getStackedBarChart section paragraph.Title
                        ]

                | Graph when chapter.Title = Literals.groupDischargeReason && 
                                paragraph.Title = Literals.paragraphTotals ->
                    prop.children [ 
                        section.YearTotals
                        |> List.map (fun t ->
                            t.Period, t.DischargeReasons
                        )
                        |> Components.PieChart.render paragraph.Title section.Totals.DischargeReasons
                    ]

                | Graph when chapter.Title = Literals.groupDischargeReason && 
                                paragraph.Title = Literals.paragraphPerYear ->
                    prop.children [
                        (fun t -> t.DischargeReasons)
                        |> getStackedBarChart section paragraph.Title
                        ]

                | Graph when chapter.Title = Literals.groupDiagnoseGroup && 
                                paragraph.Title = Literals.paragraphTotals ->
                    prop.children [ 
                        section.YearTotals
                        |> List.map (fun t ->
                            t.Period, t.DiagnoseGroups
                        )
                        |> Components.PieChart.render paragraph.Title section.Totals.DiagnoseGroups
                    ]

                | Graph when chapter.Title = Literals.groupDiagnoseGroup  && 
                                paragraph.Title = Literals.paragraphPerYear ->
                    prop.children [
                        (fun t -> t.DiagnoseGroups)
                        |> getStackedBarChart section paragraph.Title
                        ]

                | Graph when chapter.Title = Literals.groupSpecialism && 
                                paragraph.Title = Literals.paragraphTotals ->
                    prop.children [ 
                        section.YearTotals
                        |> List.map (fun t ->
                            t.Period, t.Specialisme
                        )
                        |> Components.PieChart.render paragraph.Title section.Totals.Specialisme
                    ]

                | Graph when chapter.Title = Literals.groupSpecialism  && 
                                paragraph.Title = Literals.paragraphPerYear ->
                    prop.children [
                        (fun t -> t.Specialisme)
                        |> getStackedBarChart section paragraph.Title
                        ]

                | Graph when chapter.Title = Literals.subGroupCanule &&
                            paragraph.Title = Literals.paragraphTotals ->
                    prop.children [
                        section.YearTotals 
                        |> List.map (fun t -> t.Period, t.Cannule)
                        |> Components.PieChart.render paragraph.Title section.Totals.Cannule
                    ]

                | _ ->
                    Browser.Dom.console.log("couldn't find: ", chapter.Title, paragraph.Title)
                    prop.children [
                        paragraph.Title |> sprintf "#### %s" |> Markdown.render
                        paragraph.Content |> Markdown.render
                    ]
            ]

        let rec getDetails chapter =
                chapter.Paragraphs
                |> List.map (mapParagraph chapter)
                |> fun els -> 
                    if chapter.Chapters |> List.isEmpty then els
                    else
                        let details = 
                            chapter.Chapters 
                            |> List.collect (fun chapter ->
                                [ 
                                    chapter.Title 
                                    |> sprintf "#### %s"
                                    |> Markdown.render
                                ]
                                @ (chapter |> getDetails)
                            )
                        els @ details
        
        let layoutChapters chapters =
            chapters
            |> List.map (fun chapter ->
                let details  = chapter |> getDetails

                let title = 
                    Mui.typography [
                        typography.variant.h6 
                        prop.text chapter.Title
                    ]

                {|
                    details = details
                    summary = (className, title)
                |}                    
            )
            |> Components.AccordionList.render

        section.Chapters |> layoutChapters

    let layoutReport dt (className : string) (sections : Section list) =
        sections
        |> List.map (layoutDetails className dt)
        |> Html.div

    let private comp =
        React.functionComponent("statistics", fun (props : {| displayType : DisplayType; selected : string option; report : Report |}) ->
            let classes = useStyles ()

            let report = 
                match props.selected with
                | Some s -> 
                    props.report 
                    |>  selectReport s
                | None -> props.report

            Html.div [
                prop.children [
                    
                    match props.displayType with
                    | Print -> report.Markdown |> Markdown.render 
                    | _ ->
                        report.Sections
                        |> layoutReport props.displayType classes.group
                ]
            ]
        )

    let render displayType selected report = comp ({| displayType = displayType; selected = selected; report = report |})
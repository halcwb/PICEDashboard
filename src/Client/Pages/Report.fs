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
            |> List.map int

    let selectReport s (report : Report) =
        match s |> String.split with
        | [ id1 ] -> 
            { report with
                Sections =
                    report.Sections.[id1]
                    |> List.singleton
            }
        | [ id1; id2 ] -> 
            { report with
                Sections =
                    report.Sections.[id1]
                    |> fun section -> 
                        {
                            section with 
                                Chapters = 
                                    section.Chapters.[id2]
                                    |> List.singleton
                        }
                    |> List.singleton
            }
        | [ id1; id2; id3 ] -> 
            { report with
                Sections =
                    report.Sections.[id1]
                    |> fun section -> 
                        {
                            section with 
                                Chapters = 
                                    section.Chapters.[id2]
                                    |> fun group -> 
                                        { group with
                                            Paragraphs = 
                                                group.Paragraphs.[id3]
                                                |> List.singleton
                                        }
                                    |> List.singleton
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

    let layoutDetails className (dt : DisplayType) (s : Section) =
        s.Chapters
        |> List.map (fun chapter ->
            let details =
                chapter.Paragraphs
                |> List.collect (fun paragraph ->
                    [
                        Html.div [
                            prop.style [ style.paddingBottom 20 ]

                            match dt with
                            | Graph when chapter.Title = Literals.groupMortality && 
                                         paragraph.Title = Literals.paragraphPIMandPRISM -> 
                                prop.children [
                                    paragraph.Title |> sprintf "#### %s" |> Markdown.render
                                    "##### Mortaliteit" |> Markdown.render
                                    s.YearTotals |> Components.MortalityGraph.render paragraph.Content
                                ]

                            | Graph when chapter.Title = Literals.groupMortality && 
                                         paragraph.Title = Literals.paragraphSMR -> 
                                prop.children [
                                    paragraph.Title |> sprintf "#### %s" |> Markdown.render
                                    "##### SMR per Jaar" |> Markdown.render
                                    s.YearTotals |> Components.SMRGraph.render
                                    "##### SMR Funnelplot " |> Markdown.render
                                    s.YearTotals |> Components.FunnelPlot.render
                                ]

                            | Graph when chapter.Title = Literals.groupAdmission && 
                                         paragraph.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    "#### Opnames/ontslagen en ligdagen" |> Markdown.render
                                    s.YearTotals |> Components.AdmissionsGraph.render
                                    
//                                    "#### Bed Bezetting" |> Markdown.render
                                    s.YearTotals 
                                    |> List.map (fun ytot ->
                                        ytot.Period
                                        , ytot.Occupancy
                                    )
                                    |> OccupancyGraph.render

                                    (fun t -> t.Urgency)
                                    |> getStackedBarChart s "Opname Urgentie"
                                ]


                            | Graph when chapter.Title = Literals.groupGender && 
                                         paragraph.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.Gender
                                    )
                                    |> Components.PieChart.render paragraph.Title s.Totals.Gender
                                ]

                            | Graph when chapter.Title = Literals.groupGender && 
                                         paragraph.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.Gender)
                                    |> getStackedBarChart s paragraph.Title
                                ]

                            | Graph when chapter.Title = Literals.groupAge && 
                                         paragraph.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.AgeGroup
                                    )
                                    |> Components.PieChart.render paragraph.Title s.Totals.AgeGroup
                                ]

                            | Graph when chapter.Title = Literals.groupAge && 
                                         paragraph.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.AgeGroup)
                                    |> getStackedBarChart s paragraph.Title
                                 ]

                            | Graph when chapter.Title = Literals.groupDischargeReason && 
                                         paragraph.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.DischargeReasons
                                    )
                                    |> Components.PieChart.render paragraph.Title s.Totals.DischargeReasons
                                ]

                            | Graph when chapter.Title = Literals.groupDischargeReason && 
                                         paragraph.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.DischargeReasons)
                                    |> getStackedBarChart s paragraph.Title
                                 ]

                            | Graph when chapter.Title = Literals.groupDiagnoseGroup && 
                                         paragraph.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.DiagnoseGroups
                                    )
                                    |> Components.PieChart.render paragraph.Title s.Totals.DiagnoseGroups
                                ]

                            | Graph when chapter.Title = Literals.groupDiagnoseGroup  && 
                                         paragraph.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.DiagnoseGroups)
                                    |> getStackedBarChart s paragraph.Title
                                 ]

                            | _ ->
                                prop.children [
                                    paragraph.Title |> sprintf "#### %s" |> Markdown.render
                                    paragraph.Content |> Markdown.render
                                ]
                        ]
                    ]
                )

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
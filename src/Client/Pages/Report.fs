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
        |> List.map (fun g ->
            let details =
                g.Paragraphs
                |> List.collect (fun i ->
                    [
                        Html.div [
                            prop.style [ style.paddingBottom 20 ]

                            match dt with
                            | Graph when g.Title = Literals.groupMortality && i.Title = Literals.paragraphPerYear -> 
                                prop.children [
                                    i.Title |> sprintf "#### %s" |> Markdown.render
                                    "##### Mortaliteit" |> Markdown.render
                                    s.YearTotals |> Components.MortalityGraph.render
                                    "##### SMR" |> Markdown.render
                                    s.YearTotals |> Components.SMRGraph.render
                                    "##### SMR Funnelplot " |> Markdown.render
                                    s.YearTotals |> Components.FunnelPlot.render
                                ]

                            | Graph when g.Title = Literals.groupAdmission && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    "#### Opnames/ontslagen en ligdagen" |> Markdown.render
                                    s.YearTotals |> Components.AdmissionsGraph.render

                                    (fun t -> t.Urgency)
                                    |> getStackedBarChart s "Opname Urgentie"
                                ]


                            | Graph when g.Title = Literals.groupGender && i.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.Gender
                                    )
                                    |> Components.PieChart.render i.Title s.Totals.Gender
                                ]

                            | Graph when g.Title = Literals.groupGender && i.Title = Literals.paragraphPerYear ->
                                //prop.children (s.YearTotals |> Components.StackedGenderChart.render)
                                prop.children [
                                    (fun t -> t.Gender)
                                    |> getStackedBarChart s i.Title
                                ]

                            | Graph when g.Title = Literals.groupAge && i.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.AgeGroup
                                    )
                                    |> Components.PieChart.render i.Title s.Totals.AgeGroup
                                ]

                            | Graph when g.Title = Literals.groupAge && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.AgeGroup)
                                    |> getStackedBarChart s i.Title
                                 ]

                            | Graph when g.Title = Literals.groupDischargeReason && i.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.DischargeReasons
                                    )
                                    |> Components.PieChart.render i.Title s.Totals.DischargeReasons
                                ]

                            | Graph when g.Title = Literals.groupDischargeReason && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.DischargeReasons)
                                    |> getStackedBarChart s i.Title
                                 ]

                            | Graph when g.Title = Literals.groupDiagnoseGroup && i.Title = Literals.paragraphTotals ->
                                prop.children [ 
                                    s.YearTotals
                                    |> List.map (fun t ->
                                        t.Period, t.DiagnoseGroups
                                    )
                                    |> Components.PieChart.render i.Title s.Totals.DiagnoseGroups
                                ]

                            | Graph when g.Title = Literals.groupDiagnoseGroup  && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    (fun t -> t.DiagnoseGroups)
                                    |> getStackedBarChart s i.Title
                                 ]

                            | _ ->
                                prop.children [
                                    i.Title |> sprintf "#### %s" |> Markdown.render
                                    i.Content |> Markdown.render
                                ]
                        ]
                    ]
                )

            let title = 
                Mui.typography [
                    typography.variant.h6 
                    prop.text g.Title
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
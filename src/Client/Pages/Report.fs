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

    let layoutDetails section (dt : DisplayType) (s : Section) =
        s.Chapters
        |> List.map (fun g ->
            let details =
                g.Paragraphs
                |> List.collect (fun i ->
                    [
                        i.Title |> sprintf "#### %s" |> Markdown.render
                        Html.div [
                            prop.style [ style.paddingBottom 20 ]

                            match dt with
                            | Graph when g.Title = Literals.groupOverview && i.Title = Literals.paragraphPerYear -> 
                                prop.children [
                                    "##### Mortaliteit" |> Markdown.render
                                    s.PeriodTotals |> Components.MortalityGraph.render
                                    "##### SMR" |> Markdown.render
                                    s.PeriodTotals |> Components.SMRGraph.render
                                    "##### SMR Funnelplot " |> Markdown.render
                                    s.PeriodTotals |> Components.FunnelPlot.render
                                    "##### Opnames/ontslagen en ligdagen" |> Markdown.render
                                    s.PeriodTotals |> Components.AdmissionsGraph.render
                                ]

                            | Graph when g.Title = Literals.groupGender && i.Title = Literals.paragraphTotals ->
                                prop.children [ s.Totals.Gender |> Components.PieChart.render ]

                            | Graph when g.Title = Literals.groupGender && i.Title = Literals.paragraphPerYear ->
                                prop.children (s.PeriodTotals |> Components.StackedGenderChart.render)

                            | Graph when g.Title = Literals.groupAge && i.Title = Literals.paragraphTotals ->
                                prop.children [
                                    s.Totals.AgeGroup |> Components.PieChart.render
                                 ]

                            | Graph when g.Title = Literals.groupAge && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    s.PeriodTotals |> Components.StackedAgeChart.render
                                 ]

                            | Graph when g.Title = Literals.groupDischargeReason && i.Title = Literals.paragraphTotals ->
                                prop.children [
                                    s.Totals.DischargeReasons |> PieChart.render
                                 ]

                            | Graph when g.Title = Literals.groupDischargeReason && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    s.PeriodTotals |> Components.StackedDischargeChart.render
                                 ]

                            | Graph when g.Title = Literals.groupDiagnoseGroup && i.Title = Literals.paragraphTotals ->
                                prop.children [ s.Totals.DiagnoseGroups  |> PieChart.render ]

                            | Graph when g.Title = Literals.groupDiagnoseGroup  && i.Title = Literals.paragraphPerYear ->
                                prop.children [
                                    s.PeriodTotals |> Components.StackedDiagnoseChart.render
                                 ]

                            | _ ->
                                prop.children (i.Content |> Markdown.render)
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
                summary = (section, title)
            |}                    
        )
        |> Components.AccordionList.render

    let layoutReport dt (sectionCls : string) (groupCls : string) (sections : Section list) =
        sections
        |> List.map (fun s ->
            let title = 
                Mui.typography [
                    typography.variant.h6
                    prop.text s.Title
                ]
            {|
                details = [ s |> layoutDetails groupCls dt ]
                summary = (sectionCls, title)
            |}
        )                
        |> Components.AccordionList.render 

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
                        |> layoutReport props.displayType classes.section classes.group
                ]
            ]
        )

    let render displayType selected report = comp ({| displayType = displayType; selected = selected; report = report |})
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
                                Groups = 
                                    section.Groups.[id2]
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
                                Groups = 
                                    section.Groups.[id2]
                                    |> fun group -> 
                                        { group with
                                            Items = 
                                                group.Items.[id3]
                                                |> List.singleton
                                        }
                                    |> List.singleton
                        }
                    |> List.singleton
            }
        | _ -> report

    let layoutDetails (dt : DisplayType) (s : Section) =
        s.Groups
        |> List.map (fun g ->
            let details =
                g.Items
                |> List.collect (fun i ->
                    [
                        i.Title |> sprintf "#### %s" |> Markdown.render
                        Html.div [
                            prop.style [
                                //style.backgroundColor Colors.blueGrey.``50``
                                style.paddingBottom 20
                            ]

                            match dt with
                            | Graph when g.Title = "Opnames en Mortaliteit" && i.Title = "Per Jaar" -> 
                                prop.children [
                                    "##### Mortaliteit" |> Markdown.render
                                    s.Totals |> Components.MortalityGraph.render
                                    "##### Funnelplot " |> Markdown.render
                                    s.Totals |> Components.FunnelPlot.render
                                    "##### Opnames/ontslagen en ligdagen" |> Markdown.render
                                    s.Totals |> Components.AdmissionsGraph.render
                                ]
                            | Graph when g.Title = "Geslacht" && i.Title = "Per Jaar" ->
                                prop.children (s.Totals |> Components.StackedGenderChart.render)
                            | Graph when g.Title = "Leeftijd" && i.Title = "Per Jaar" ->
                                prop.children [
                                    s.Totals |> Components.StackedAgeChart.render
                                 ]
                            | Graph when g.Title = "PICU Ontslagreden" && i.Title = "Per Jaar" ->
                                prop.children [
                                    s.Totals |> Components.StackedDischargeChart.render
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
                summary = ("", title)
            |}                    
        )
        |> Components.AccordionList.render

    let layoutReport dt (section : string) (sections : Section list) =
        sections
        |> List.map (fun s ->
            let title = 
                Mui.typography [
                    typography.variant.h6
                    prop.text s.Title
                ]
            {|
                details = [ s |> layoutDetails dt ]
                summary = (section, title)
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
                        Html.div [
                            prop.className classes.div
                            prop.style [
                                style.backgroundColor classes.div
                                style.padding 10
                            ]
                            prop.children [
                                "## PICE Rapportage"
                                |> Markdown.render 
                            ]
                        ]

                        report.Sections
                        |> layoutReport props.displayType classes.section
                ]
            ]
        )

    let render displayType selected report = comp ({| displayType = displayType; selected = selected; report = report |})
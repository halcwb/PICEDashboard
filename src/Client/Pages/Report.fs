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

    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            print = styles.create [
                style.marginBottom (theme.spacing 2)
                style.paddingTop (theme.spacing 1)
                style.color (theme.palette.primary.dark)
            ]

            form = styles.create [
            ]

            div = styles.create [
                style.marginBottom (theme.spacing 1)
            ]

            head = styles.create [
                style.backgroundColor theme.palette.primary.main
                style.color theme.palette.common.white
            ]

        |}
    )


    let createDetails (dt : DisplayType) (toMd : string -> ReactElement) (s : Section) =
        s.Groups
        |> List.map (fun g ->
            let details =
                g.Items
                |> List.collect (fun i ->
                    [
                        i.Title |> sprintf "#### %s" |> toMd
                        Html.div [
                            prop.style [
                                //style.backgroundColor Colors.blueGrey.``50``
                                style.paddingBottom 20
                            ]

                            match dt with
                            | Graph when g.Title = "Opnames en Mortaliteit" && i.Title = "Per Jaar" -> 
                                prop.children [
                                    "##### Mortaliteit" |> toMd
                                    s.Totals |> Components.MortalityGraph.render
                                    "##### Funnelplot " |> toMd
                                    s.Totals |> Components.FunnelPlot.render
                                    "##### Opnames/ontslagen en ligdagen" |> toMd
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
                                prop.children (i.Content |> toMd)
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
                summary = title
            |}                    
        )
        |> Components.AccordionList.render

    let createReport dt toMd (sections : Section list) =
        sections
        |> List.map (fun s ->
            let title = 
                Mui.typography [
                    typography.variant.h6
                    prop.text s.Title
                ]
            {|
                details = [ s |> createDetails dt toMd ]
                summary = title
            |}
        )                
        |> Components.AccordionList.render 

    let private comp =
        React.functionComponent("statistics", fun (props : {| displayType : DisplayType; report : Report |}) ->
            // let model, dispatch = React.useElmish(init, update, [||])

            let classes = useStyles ()

            let toMd = Components.Markdown.createMarkdown classes.head

            Html.div [
                prop.children [
                    
                    match props.displayType with
                    | Print -> props.report.Markdown |> toMd
                    | _ ->
                        Html.div [
                            prop.style [
                                style.paddingBottom 20
                            ]
                            prop.children [
                                "# PICE Rapportage"
                                |> toMd 
                            ]
                        ]

                        props.report.Sections
                        |> createReport props.displayType toMd
                ]
            ]
        )

    let render displayType report = comp ({| displayType = displayType; report = report |})
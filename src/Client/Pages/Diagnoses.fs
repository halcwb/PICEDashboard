namespace Pages


module Diagnoses =

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

    
    let private comp =
        React.functionComponent("diagnoses", fun (props : {| displayType : DisplayType; selected : string list; report : Report |}) ->
            let classes = useStyles ()
            let selectTxt = 
                props.selected
                |> String.concat "/"
                |> sprintf "Selectie: %s"
            Html.div [
                prop.style [ style.paddingLeft 20 ]
                prop.children [
                    match props.selected with
                    | [] -> 
                        Mui.typography [
                            typography.variant.h4
                            prop.text "Selecteer 1 of meerdere diagnoses"
                        ]
                    | _ -> 
                        Html.div [
                        ]
                        fun totals ->
                            totals.Diagnoses
                            |> List.filter (fun (k, v) ->
                                props.selected
                                |> List.exists ((=) k)
                            )
                        |> getStackedBarChart (props.report.Sections |> List.head) "Selectie"

                        Mui.typography [
                            typography.variant.body1
                            prop.style [
                                style.paddingTop 10
                            ]
                            prop.text selectTxt
                        ]
                    ]
            ]
        )

    let render displayType selected report = comp ({| displayType = displayType; selected = selected; report = report |})

module App

open Elmish
open Thoth.Fetch
open Feliz
open Feliz.UseElmish
open Fable.MaterialUI
open Feliz.MaterialUI

open Informedica.PICE.Shared.Types
open Types

type Model = 
    {   
        Report : Deferred<Result<Report, string>>
        DisplayType : DisplayType
        DisplayTypeAcknowledged : bool
    }

type Msg =
    | DisplayTypeChanged
    | DisplayTypeAcknowledged
    | LoadStatistics of AsyncOperationStatus<Result<Report, string>>


let init() =
    let model : Model = 
        {
            Report = HasNotStartedYet
            DisplayType = Graph
            DisplayTypeAcknowledged = true
        }
    model, Cmd.ofMsg (LoadStatistics Started)

let update msg model =
    match msg with
    | DisplayTypeChanged ->
        { model with 
            DisplayType = 
                match model.DisplayType with
                | Print -> Graph
                | Graph -> Table
                | Table -> Print
            DisplayTypeAcknowledged = false
        }, Cmd.none
    | DisplayTypeAcknowledged -> 
        { model with
            DisplayTypeAcknowledged = true
        }, Cmd.none
    | LoadStatistics Started ->
        let load = async {
            try
                let! stats = Server.api.GetReport ()
                return LoadStatistics(Finished stats)
            with
            | error -> 
                Log.developmentError error
                return LoadStatistics (Finished(Error "Error while retrieving stats"))
        }
        
        { model with Report = InProgress }, Cmd.fromAsync load

    | LoadStatistics (Finished report) ->
        { model with Report = Resolved report} , Cmd.none


let defaultTheme = 
    Styles.createMuiTheme ()
    |> Styles.responsiveFontSizes


let useStyles = Styles.makeStyles(fun styles theme ->
    {|

        page = styles.create [
            style.marginTop (theme.spacing 10)
            style.marginBottom (theme.spacing 5)
        ]

    |}
)

let statsView = 
    React.functionComponent("statsview", fun (props : {| model : Model; dispatch : Msg -> unit |}) ->
        let classes = useStyles ()

        let display (s : string) = 
            Mui.typography [
                typography.variant.h4 
                prop.text s
            ]

        let buttons = 
            [
                Icons.menuIcon [], (fun _ -> DisplayTypeChanged |> props.dispatch)
            ]

        Mui.themeProvider [
            themeProvider.theme defaultTheme
            themeProvider.children [
                Mui.container [
                    container.component' "main"
                    container.disableGutters true

                    if Hooks.useMediaQuery defaultTheme.breakpoints.upLg then
                        container.maxWidth.lg
                    else
                        container.maxWidth.md

                    prop.className classes.page
                    prop.children [
                        Components.AppBar.render "Informedica PICE dashboard" buttons

                        match props.model.Report with
                        | HasNotStartedYet -> display "De boel wordt opgestart ..."
                        | InProgress       -> display "Het rapport wordt opgehaald ..."
                        | Resolved (Ok report) -> 
                            if props.model.DisplayTypeAcknowledged then
                                Pages.Report.render props.model.DisplayType report
                            else 
                                let content =
                                    match props.model.DisplayType with
                                    | Print -> "Het rapport toont nu een print versie"
                                    | Graph -> "Het rapport bevat nu grafieken i.p.v. tabellen"
                                    | Table -> "Het rapport vertoont nu tabellen i.p.v. grafieken"
                                Components.Dialog.render "Verandering van rapport" content (fun _ -> DisplayTypeAcknowledged |> props.dispatch)
                        | Resolved (Error err)  ->
                            sprintf "Oeps er ging wat mis:\n%s" err
                            |> display
                    ]
                ]
            ]
        ]
    )


let render model dispatch = statsView ({| model = model; dispatch = dispatch |})
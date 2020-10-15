module App

open Elmish
open Thoth.Fetch
open Feliz
open Feliz.UseElmish
open Fable.MaterialUI.Icons
open Feliz.MaterialUI


open Informedica.PICE.Shared

type Model = Deferred<Result<Types.Statistics, string>>

type Msg =
    | LoadStatistics of AsyncOperationStatus<Result<Types.Statistics, string>>

let init() =
    let model : Model = HasNotStartedYet
    model, Cmd.ofMsg (LoadStatistics Started)

let update msg model =
    match msg with
    | LoadStatistics Started ->
        let load = async {
            try
                let! stats = Server.api.GetStatistics ()
                return LoadStatistics(Finished stats)
            with
            | error -> 
                Log.developmentError error
                return LoadStatistics (Finished(Error "Error while retrieving stats"))
        }
        
        InProgress, Cmd.fromAsync load

    | LoadStatistics (Finished stats) ->
        Resolved stats, Cmd.none


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
    React.functionComponent("statsview", fun (props : {| stats : Deferred<Result<Types.Statistics, string>>; dispatch : Msg -> unit |}) ->
        let classes = useStyles ()

        let display (s : string) = 
            Mui.typography [
                typography.variant.h4 
                prop.text s
            ]

        Mui.themeProvider [
            themeProvider.theme defaultTheme
            themeProvider.children [
                Mui.container [
                    container.component' "main"
                    if Hooks.useMediaQuery defaultTheme.breakpoints.upLg then
                        container.maxWidth.md
                    else
                        container.maxWidth.sm
                    prop.className classes.page
                    prop.children [
                        Components.AppBar.render "Informedica PICE dashboard" []

                        match props.stats with
                        | HasNotStartedYet -> display "De boel wordt opgestart ..."
                        | InProgress       -> display "Statistieken worden opgehaald ..."
                        | Resolved (Ok stats) -> Pages.Statistics.render stats
                        | Resolved (Error err)  ->
                            sprintf "Oeps er ging wat mis:\n%s" err
                            |> display
                    ]
                ]
            ]
        ]
    )


let render model dispatch = statsView ({| stats = model; dispatch = dispatch |})
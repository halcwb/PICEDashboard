module App

open Elmish
open Thoth.Fetch
open Feliz
open Feliz.UseElmish
open Fable.MaterialUI
open Feliz.MaterialUI

open Informedica.PICE.Shared.Types
open Types
open Components

type Model = 
    {   
        Report : Deferred<Result<Report, string>>
        DisplayType : DisplayType
        DisplayTypeAcknowledged : bool
        SelectedTreeItem : string option
    }

type Msg =
    | DisplayTypeChanged
    | DisplayTypeAcknowledged
    | LoadStatistics of AsyncOperationStatus<Result<Report, string>>
    | TreeItemSelected of string


let init() =
    let model : Model = 
        {
            Report = HasNotStartedYet
            DisplayType = Graph
            DisplayTypeAcknowledged = true
            SelectedTreeItem = Some "0"
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

    | TreeItemSelected s -> 
        { model with
            SelectedTreeItem = Some s
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


let mapToTreeData (sections : Section list) =
    let rec mapChapter s (chapter : Chapter) =
        let paragraphs = 
            chapter.Paragraphs
            |> List.mapi (fun i p -> 
                TreeViewDrawer.createData (sprintf "%s.P|%i" s i) p.Title []
            )

        chapter.Chapters 
        |> List.mapi (fun i chapter -> 
            chapter 
            |> mapChapter (sprintf "%s.C|%i" s i)
        ) 
        |> List.append paragraphs
        |> TreeViewDrawer.createData s chapter.Title
        
    sections
    |> List.mapi (fun i section ->
        section.Chapters 
        |> List.mapi (fun i2 chapter ->
            chapter |> mapChapter (sprintf "%i.C|%i" i i2)
        )
        |> TreeViewDrawer.createData 
            (string i)
            section.Title
            
    )

let statsView = 
    React.functionComponent("statsview", fun (props : {| model : Model; dispatch : Msg -> unit |}) ->
        let classes = useStyles ()

        let display (s : string) =
            Html.div [
                Mui.typography [
                    prop.style [
                        style.padding 50
                    ]
                    prop.text s
                    typography.variant.h4 
                ]
                Mui.linearProgress []
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
                        AppBar.render "Informedica PICE dashboard" buttons

                        match props.model.Report with
                        | HasNotStartedYet -> display "De boel wordt opgestart ..."
                        | InProgress       -> display "Het rapport wordt opgehaald ..."
                        | Resolved (Ok report) -> 
                            let treeData =
                                report.Sections
                                |> mapToTreeData
                            TreeViewDrawer.render treeData (TreeItemSelected >> props.dispatch)

                            Html.div [
                                prop.style [ style.marginLeft 100 ]
                                prop.children [
                                    if props.model.DisplayTypeAcknowledged then
                                        Pages.Report.render props.model.DisplayType props.model.SelectedTreeItem report
                                    else 
                                        let content =
                                            match props.model.DisplayType with
                                            | Print -> "Het rapport toont nu een print versie"
                                            | Graph -> "Het rapport bevat nu grafieken i.p.v. tabellen"
                                            | Table -> "Het rapport vertoont nu tabellen i.p.v. grafieken"
                                        Dialog.render "### Verandering van rapport type" content (fun _ -> DisplayTypeAcknowledged |> props.dispatch)

                                ]
                            ]
                        | Resolved (Error err)  ->
                            sprintf "Oeps er ging wat mis:\n%s" err
                            |> display
                    ]
                ]
            ]
        ]
    )


let render model dispatch = statsView ({| model = model; dispatch = dispatch |})
module App

open Fable.Core.JsInterop
open Elmish
open Thoth.Fetch
open Feliz
open Feliz.UseElmish
open Fable.MaterialUI
open Feliz.MaterialUI


open Informedica.PICE.Shared.Types
open Types
open Components


type State = 
    {   
        Report : Deferred<Result<Report, string>>
        PatientsCSV : Deferred<Result<string, string>>
        RequestPatients : bool
        DisplayType : DisplayType
        DisplayTypeAcknowledged : bool
        SideMenuIsOpen : bool
        SelectedTreeItem : string option
    }


type Msg =
    | LoadStatistics of AsyncOperationStatus<Result<Report, string>>
    | LoadPatientsCSV of AsyncOperationStatus<Result<string, string>>
    | DisplayTypeChanged
    | DisplayTypeAcknowledged
    | TreeItemSelected of string
    | PatientCSVRequested
    | PatientCSVCanceled
    | PatientListReceived of string
    | SideMenuOpenToggled


let init() =
    let state : State = 
        {
            Report = HasNotStartedYet
            PatientsCSV = HasNotStartedYet
            RequestPatients = false
            DisplayType = Graph
            DisplayTypeAcknowledged = true
            SideMenuIsOpen = true
            SelectedTreeItem = Some "0"
        }
    state, Cmd.ofMsg (LoadStatistics Started)

let update msg state =
    match msg with
    | SideMenuOpenToggled ->
        { state with SideMenuIsOpen = state.SideMenuIsOpen |> not }, Cmd.none

    | PatientCSVRequested ->
        { state with RequestPatients = true }, Cmd.none

    | PatientCSVCanceled ->
        { state with RequestPatients = false }, Cmd.none

    | PatientListReceived s ->
        let load =
            async {
                try 
                    let! csv = 
                        s.Split('\n')
                        |> Seq.toList
                        |> List.map (fun s -> s.Trim())
                        |> Server.api.GetScoresCSV
                    return LoadPatientsCSV (Finished csv)
                with
                | error ->
                    Log.developmentError error
                    return LoadPatientsCSV (Finished (Error "Kan patienten niet ophalen"))
            }
        { state with 
            PatientsCSV = InProgress 
            RequestPatients = false
        }, Cmd.fromAsync load

    | LoadPatientsCSV Started -> 
        { state with PatientsCSV = InProgress }, Cmd.none

    | LoadPatientsCSV (Finished s) ->
        match s with
        | Ok s ->
            let blob = Browser.Blob.Blob.Create ([| s |])
            FileSaver.fileSaver.fileSaver.saveAs (blob, "scores.csv")
        | _ -> ()
        
        { state with PatientsCSV = Resolved s}, Cmd.none

    | DisplayTypeChanged ->
        { state with 
            DisplayType = 
                match state.DisplayType with
                | Print -> Graph
                | Graph -> Table
                | Table -> Print
            DisplayTypeAcknowledged = false
        }, Cmd.none

    | DisplayTypeAcknowledged -> 
        { state with
            DisplayTypeAcknowledged = true
        }, Cmd.none

    | TreeItemSelected s -> 
        { state with
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
        
        { state with Report = InProgress }, Cmd.fromAsync load

    | LoadStatistics (Finished report) ->
        { state with Report = Resolved report} , Cmd.none


let defaultTheme = 
    Styles.createMuiTheme ()
    |> Styles.responsiveFontSizes


let useStyles = Styles.makeStyles(fun styles theme ->
    {|

        page = styles.create [
            style.marginTop (theme.spacing 10)
            style.marginBottom (theme.spacing 5)
            style.minHeight 1000
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


[<Literal>]
let dialogUploadText = """Selecteer een bestand met een lijst van patient nummers. 
Het bestand moet een tekst bestand zijn en de nummmers moeten 
onder elkaar staan. Na het uploaden van de patient nummers zal,
na enige tijd, een CSV bestand worden gedownload met de PIM en 
PRISM scores.
"""


let createUploadDialog dispatch =
    printfn "open dialog"
    Mui.dialog [
        dialog.open' true
        dialog.children [
            Mui.dialogTitle [ "#### Download patienten" |> Markdown.render ]
            Mui.dialogContent [
                dialogUploadText
                |> sprintf "%s"
                |> Markdown.render
            ]
            Mui.dialogActions [
                Mui.inputLabel [
                    Mui.input [ 
                        prop.style [ style.display.none ]
                        prop.multiple true
                        input.type' "file"
                        prop.accept "*"
                        prop.onInput(fun ev ->
                            let reader = Browser.Dom.FileReader.Create()
                            let file = ev.target?files?(0)

                            reader.onerror <- ignore

                            reader.onload <- fun ev ->
                                Browser.Dom.console.log("Uploaded", ev.target?result)
                                ev.target?result
                                |> string
                                |> PatientListReceived
                                |> dispatch

                            reader.readAsText file
                        )
                    ]

                    Mui.iconButton [ 
                        iconButton.children [ Icons.cancelIcon "" ]
                        prop.onClick (fun _ -> PatientCSVCanceled |> dispatch)
                    ]

                    Mui.iconButton [
                        iconButton.component' "span"
                        iconButton.children [ Icons.attachFileIcon "" ]
                    ]
                ]
            ]
        ]
    ]

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


let createMainContent report displayTypeAck displayType menuIsOpen treeItem dispatch =
    [
        match report with
        | HasNotStartedYet -> display "De boel wordt opgestart ..."
        | InProgress       -> display "Het rapport wordt opgehaald ..."
        | Resolved (Ok report) -> 
            let treeData =
                report.Sections
                |> mapToTreeData
            TreeViewDrawer.render treeData menuIsOpen (TreeItemSelected >> dispatch)

            Html.div [
                prop.style [ style.marginLeft 150 ]
                prop.children [
                    if displayTypeAck then
                        Pages.Report.render displayType treeItem report
                    else 
                        let content =
                            match displayType with
                            | Print -> "Het rapport toont nu een print versie"
                            | Graph -> "Het rapport bevat nu grafieken i.p.v. tabellen"
                            | Table -> "Het rapport vertoont nu tabellen i.p.v. grafieken"
                        Dialog.render "### Verandering van rapport type" content (fun _ -> DisplayTypeAcknowledged |> dispatch)

                ]
            ]
        | Resolved (Error err)  ->
            sprintf "Oeps er ging wat mis:\n%s" err
            |> display
    ]


let private comp = 
    React.functionComponent("statsview", fun (props : {| state : State; dispatch : Msg -> unit |}) ->
        let classes = useStyles ()

        let buttonsL = 
            [
                Icons.menuIcon [], (fun _ -> SideMenuOpenToggled |> props.dispatch)
            ]

        let buttonsR = 
            [
                Icons.publishIcon [], (fun _ -> PatientCSVRequested |> props.dispatch)
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
                        AppBar.render "Informedica PICE dashboard" buttonsL buttonsR

                        if props.state.RequestPatients then props.dispatch |> createUploadDialog
                        else
                            yield! createMainContent props.state.Report 
                                                     props.state.DisplayTypeAcknowledged 
                                                     props.state.DisplayType
                                                     props.state.SideMenuIsOpen
                                                     props.state.SelectedTreeItem
                                                     props.dispatch
                    ]
                ]
            ]
        ]
    )


let render state dispatch = comp ({| state = state; dispatch = dispatch |})
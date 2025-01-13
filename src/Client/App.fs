module App


module private Elmish =


    open Elmish
    open Shared

    type Model = {
        HelloWorld: Deferred<string>
        Report: Deferred<Result<Report, string>>
        PatientsCSV: Deferred<Result<string, string>>
        RequestPatients: bool
        DisplayType: DisplayType
        DisplayTypeAcknowledged: bool
        SideMenuIsOpen: bool
        SelectedFilter: Filter
        SelectedTreeItem: string option
        ShowDiagnoses: bool
        SelectedDiagnoses: string[]
    }


    type Msg =
        | HelloWorld of AsyncOperationStatus<Result<string, string>>
        | LoadStatistics of AsyncOperationStatus<Result<Report, string>>
        | LoadPatientsCSV of AsyncOperationStatus<Result<string, string>>
        | DisplayTypeChanged
        | DisplayTypeAcknowledged
        | ReportFilterItemSelected of Filter * string
        | PatientCSVRequested
        | PatientCSVCanceled
        | PatientListReceived of string
        | SideMenuOpenToggled
        | ShowDiagnoses
        | ShowReport
        | DiagnosesSelected of string[]
        | NoOp


    let serverApi = Server.api


    let init () =
        {
            HelloWorld = HasNotStartedYet
            Report = HasNotStartedYet
            PatientsCSV = HasNotStartedYet
            RequestPatients = false
            DisplayType = Graph
            DisplayTypeAcknowledged = true
            SideMenuIsOpen = true
            SelectedFilter = NoFilter
            SelectedTreeItem = Some "0"
            ShowDiagnoses = false
            SelectedDiagnoses = [||]

        },
        Cmd.ofMsg (HelloWorld(Started))


    let update msg state =
        match msg with
        | HelloWorld Started ->
            Logging.log "Hello World" "Started"

            let cmd =
                async {
                    let! response = serverApi.SayHello()
                    return HelloWorld(Finished response)
                }
                |> Cmd.fromAsync

            { state with HelloWorld = InProgress }, cmd
        | HelloWorld(Finished value) ->
            Logging.log "Hello World Finished" value

            {
                state with
                    HelloWorld =
                        match value with
                        | Ok value -> value
                        | Error value -> value
                        |> Resolved
            },
            Cmd.ofMsg (LoadStatistics Started)


        | SideMenuOpenToggled ->
            {
                state with
                    SideMenuIsOpen = state.SideMenuIsOpen |> not
            },
            Cmd.none

        | PatientCSVRequested -> { state with RequestPatients = true }, Cmd.none

        | PatientCSVCanceled -> { state with RequestPatients = false }, Cmd.none

        | PatientListReceived s ->
            let load =
                async {
                    try
                        let! csv =
                            s.Split('\n')
                            |> Seq.toList
                            |> List.map (fun s -> s.Trim())
                            |> serverApi.GetScoresCSV

                        return LoadPatientsCSV(Finished csv)
                    with error ->
                        Log.developmentError error
                        return LoadPatientsCSV(Finished(Error "Kan patienten niet ophalen"))
                }

            {
                state with
                    PatientsCSV = InProgress
                    RequestPatients = false
            },
            Cmd.fromAsync load

        | LoadPatientsCSV Started -> { state with PatientsCSV = InProgress }, Cmd.none

        | LoadPatientsCSV(Finished s) ->
            match s with
            | Ok s ->
                let blob = Browser.Blob.Blob.Create([| s |])
                FileSaver.fileSaver.fileSaver.saveAs (blob, "scores.csv")
            | _ -> ()

            { state with PatientsCSV = Resolved s }, Cmd.none

        | DisplayTypeChanged ->
            {
                state with
                    DisplayType =
                        match state.DisplayType with
                        | Print -> Graph
                        | Graph -> Print
                        | Table -> Print
                    DisplayTypeAcknowledged = true // need to set this to false to show the dialog
            },
            Cmd.none

        | DisplayTypeAcknowledged ->
            {
                state with
                    DisplayTypeAcknowledged = true
            },
            Cmd.none

        | ReportFilterItemSelected(filter, s) ->
            let cmd =
                if state.SelectedFilter = filter then
                    Cmd.none
                else
                    Cmd.ofMsg (LoadStatistics Started)

            {
                state with
                    Report =
                        if state.SelectedFilter = filter then
                            state.Report
                        else
                            HasNotStartedYet
                    SelectedFilter = filter
                    SelectedTreeItem = Some s
            },
            cmd

        | LoadStatistics Started ->
            let load =
                async {
                    try
                        let! stats = serverApi.GetReport state.SelectedFilter
                        return LoadStatistics(Finished stats)
                    with error ->
                        Log.developmentError error
                        return LoadStatistics(Finished(Error "Error while retrieving stats"))
                }

            { state with Report = InProgress }, Cmd.fromAsync load

        | LoadStatistics(Finished report) -> { state with Report = Resolved report }, Cmd.none

        | ShowDiagnoses -> { state with ShowDiagnoses = true }, Cmd.none
        | ShowReport -> { state with ShowDiagnoses = false }, Cmd.none
        | DiagnosesSelected dgs -> { state with SelectedDiagnoses = dgs }, Cmd.none


        | NoOp -> failwith "Not Implemented"


open Fable.Core
open Fable.Core.JsInterop
open Browser
open Fable.React

open Elmish
open Shared


[<Import("createTheme", from = "@mui/material/styles")>]
let private createTheme props = emitJsExpr props "($1, { factor : 2 })"


[<Import("responsiveFontSizes", from = "@mui/material/styles")>]
let private responsiveFontSizes theme = emitJsExpr theme "($1)"


[<JSX.Component>]
let View () =
    let state, dispatch = React.useElmish (Elmish.init, Elmish.update, [||])

    let createData id label children = {
        id = id
        label = label
        children = children

    }

    let mapToTreeData (sections: Section list) =
        let rec mapChapter s (chapter: Chapter) =
            let paragraphs =
                chapter.Paragraphs
                |> List.mapi (fun i p -> createData (sprintf "%s.P|%i" s i) p.Title [])

            chapter.Chapters
            |> List.mapi (fun i chapter -> chapter |> mapChapter (sprintf "%s.C|%i" s i))
            |> List.append paragraphs
            |> createData s chapter.Title

        sections
        |> List.mapi (fun i section ->
            section.Chapters
            |> List.mapi (fun i2 chapter -> chapter |> mapChapter (sprintf "%i.C|%i" i i2))
            |> createData (string i) section.Title)

    let display showProgress (s: string) =

        if showProgress then
            JSX.jsx
                $"""
                import Box from '@mui/material/Box';
                import LinearProgress from '@mui/material/LinearProgress';
                <Box>
                    <Typography variant="h6" gutterBottom >
                        {s}
                    </Typography>
                    <LinearProgress />
                </Box>
                """
        else
            JSX.jsx
                $"""
                import Typography from '@mui/material/Typography';

                <React.Fragment>
                    <Typography variant="h6" gutterBottom >
                        {s}
                    </Typography>
                </React.Fragment>
                """

    let createMainContent (state: Model) dispatch =
        if state.ShowDiagnoses then
            match state.Report with
            | HasNotStartedYet -> display true "De boel wordt opgestart ..."
            | InProgress -> display true "Het rapport wordt opgehaald ..."
            | Resolved(Error e) -> display false $"Oeps:\n%s{e}"
            | Resolved(Ok report) ->

                let dgs =
                    report.Sections
                    |> List.head
                    |> (fun section -> section.Totals.Diagnoses)
                    |> List.toArray

                let diagMenu =
                    let props = {|
                        isOpen = state.SideMenuIsOpen
                        toggle = fun () -> SideMenuOpenToggled |> dispatch
                        diagnoses = dgs
                        selected = state.SelectedDiagnoses
                        dispatch =
                            fun
                                (o:
                                    {|
                                        showReport: bool
                                        selected: string[]
                                    |}) ->
                                if o.showReport then
                                    ShowReport |> dispatch
                                else
                                    o.selected |> DiagnosesSelected |> dispatch
                    |}

                    Components.DiagnosesMenu.View(props)

                let diagPage =
                    let props = {|
                        displayType = state.DisplayType
                        selected = state.SelectedDiagnoses
                        report = report
                    |}

                    Pages.Diagnoses.View(props)

                JSX.jsx
                    $"""
                    import Box from '@mui/material/Box';

                    <Box>
                        {diagMenu}
                        {diagPage}
                    </Box>
                    """

        else
            match state.Report with
            | HasNotStartedYet -> display true "De boel wordt opgestart ..."
            | InProgress ->
                printfn "InProgress"
                display true "Het rapport wordt opgehaald ..."
            | Resolved(Error err) -> $"Oeps er ging wat mis:\n%s{err}" |> display false
            | Resolved(Ok report) ->

                let treeData = report.Sections |> mapToTreeData

                let reportMenu =
                    Components.ReportMenu.View {|
                        data = treeData
                        isOpen = state.SideMenuIsOpen
                        toggle = fun () -> SideMenuOpenToggled |> dispatch
                        filter = state.SelectedFilter
                        currentItem = state.SelectedTreeItem |> Option.defaultValue "0"
                        dispatch =
                            fun
                                (o:
                                    {|
                                        filter: Filter
                                        item: string
                                        showDiagnoses: bool
                                    |}) ->
                                if o.showDiagnoses then
                                    ShowDiagnoses |> dispatch
                                else
                                    (o.filter, o.item) |> ReportFilterItemSelected |> dispatch
                    |}

                let reportPage =
                    Pages.Report.View {|
                        displayType = state.DisplayType
                        selected = state.SelectedTreeItem
                        report = report
                    |}


                if state.DisplayTypeAcknowledged then

                    JSX.jsx
                        $"""
                    import Box from '@mui/material/Box';

                    <Box>
                        {reportMenu}
                        {reportPage}
                    </Box>
                    """
                else
                    let content =
                        match state.DisplayType with
                        | Print -> "Het rapport toont nu een print versie"
                        | Graph -> "Het rapport bevat nu grafieken i.p.v. tabellen"
                        | Table -> "Het rapport vertoont nu tabellen i.p.v. grafieken"

                    let dialog =
                        Components.Dialog.View {|
                            title = "### Verandering van rapport type"
                            content = content
                            dispatch = fun _ -> DisplayTypeAcknowledged |> dispatch
                        |}

                    JSX.jsx
                        $"""
                    import Box from '@mui/material/Box';

                    <Box>
                        {dialog}
                    </Box>
                    """

    let titleBar =
        Components.TitleBar.View {|
            title = "PICE Dashboard"
            toggleSideMenu = fun () -> SideMenuOpenToggled |> dispatch
            showGraph = state.DisplayType = Graph
            toggleGraph = fun () -> DisplayTypeChanged |> dispatch
        |}

    let content =
        if state.RequestPatients then
            let s = "Patienten worden opgehaald ..."
            JSX.jsx $"""<React.Fragment>{s}</React.Fragment>""" //props.dispatch |> createUploadDialog
        else
            createMainContent state dispatch

    let theme =
        createTheme {|
            components = {|
                MuiAccordionSummary = {|
                    styleOverrides = {|
                        root = {| backgroundColor = "#f5f5f5" |}
                    |}
                |}
            |}
        |}

    let contSx = {| height = "100vh"; marginLeft = 33 |}

    JSX.jsx
        $"""
        import {{ ThemeProvider }} from '@mui/material/styles';
        import {{ responsiveFontSizes, createTheme }} from '@mui/material/styles';
        import CssBaseline from '@mui/material/CssBaseline';
        import Typography from '@mui/material/Typography';
        import Container from '@mui/material/Container';
        import React from 'react';

        <React.StrictMode>
            <CssBaseline enableColorScheme />
            <ThemeProvider theme={responsiveFontSizes theme} >
                <React.Fragment>
                    <Container sx={contSx} disableGutters={true} component="main" maxWidth="xl" > 
                        {titleBar}
                        {content}
                    </Container>
                </React.Fragment>
            </ThemeProvider>
        </React.StrictMode>
    """


let app = ReactDomClient.createRoot (document.getElementById "app")
app.render (View() |> toReact)

module App


module private Elmish =


    open Elmish
    open Feliz
    open Feliz.Router
    open Fable.Remoting.Client

    open Shared


    type DisplayType =
        | Print
        | Table
        | Graph


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
        SelectedDiagnoses: string list
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
        | DiagnosesSelected of string list
        | NoOp


    let serverApi =
        Remoting.createApi ()
        |> Remoting.withRouteBuilder Api.routerPaths
        |> Remoting.buildProxy<Api.IServerApi>


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
            SelectedDiagnoses = []

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
            Cmd.none


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
                        | Graph -> Table
                        | Table -> Print
                    DisplayTypeAcknowledged = false
            },
            Cmd.none

        | DisplayTypeAcknowledged ->
            {
                state with
                    DisplayTypeAcknowledged = true
            },
            Cmd.none

        | ReportFilterItemSelected(f, s) ->
            let cmd =
                if state.SelectedFilter = f then
                    Cmd.none
                else
                    Cmd.ofMsg (LoadStatistics Started)

            {
                state with
                    Report =
                        if state.SelectedFilter = f then
                            state.Report
                        else
                            HasNotStartedYet
                    SelectedFilter = f
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


    let private createData id label children = {
        id = id
        label = label
        children = children

    }


    let private mapToTreeData (sections: Section list) =
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


open Fable.Core
open Browser
open Fable.React

open Elmish


[<Literal>]
let private themeDef =
    """
responsiveFontSizes(createTheme(), { factor : 2 })
"""


[<Import("createTheme", from = "@mui/material/styles")>]
[<Emit(themeDef)>]
let private theme: obj = jsNative


[<JSX.Component>]
let View () =
    let state, dispatch = React.useElmish (Elmish.init, Elmish.update, [||])

    Logging.log "Hello World" state.HelloWorld

    let xs = [| 1..5 |]
    let ys = [| 2..2..8 |]

    let data = [|
        for i in 0..4 do
            {| name = string i; uv = i * i |}
    |]

    let typoSx = {| flexGrow = 1 |}

    let contSx = {| height = "100vh"; mt = 3 |}

    let stckSx = {|
        display = "flex"
        justifyContent = "center"
        alignItems = "center"
        height = "87%"
    |}

    let titleBar =
        Components.TitleBar.View {|
            title = "PICE Dashboard"
            toggleSideMenu = (fun () -> ())
        |}

    let sideMenu =
        Components.SideMenu.View(
            {|
                anchor = "left"
                isOpen = false
                toggle = fun () -> ()
                menuClick = fun _ -> ()
                items = [| (None, "Home", false); (None, "About", false); (None, "Contact", false) |]
            |}
        )

    let margin = {|
        top = 5
        right = 5
        bottom = 5
        left = 0
    |}

    JSX.jsx
        $"""
        import {{ ThemeProvider }} from '@mui/material/styles';
        import {{ responsiveFontSizes }} from '@mui/material/styles';
        import CssBaseline from '@mui/material/CssBaseline';
        import Typography from '@mui/material/Typography';
        import Container from '@mui/material/Container';
        import Stack from '@mui/material/Stack';
        import React from 'react';
        import {{ LineChart, Line, CartesianGrid, XAxis, YAxis, Tooltip }} from 'recharts';

        <React.StrictMode>
            <CssBaseline enableColorScheme />
            <ThemeProvider theme={theme}>
                <React.Fragment>
                    <React.Fragment>
                        {titleBar}
                    </React.Fragment>
                    <React.Fragment>
                        {sideMenu}
                    </React.Fragment>
                    <Container sx={contSx}>
                        <Stack sx={stckSx}>
                            <Typography variant="h5" sx={typoSx}>
                                Hello World
                            </Typography>
                            <LineChart width={500} height={300} data={data} margin={margin}>
                                <Line type="monotone" dataKey="uv" stroke="#8884d8" />
                                <CartesianGrid stroke="#ccc" strokeDasharray="5 5" />
                                <XAxis dataKey="name" />
                                <YAxis />
                                <Tooltip />
                            </LineChart>
                        </Stack>
                    </Container>
                </React.Fragment>
            </ThemeProvider>
        </React.StrictMode>
    """


let app = ReactDomClient.createRoot (document.getElementById "app")
app.render (View() |> toReact)

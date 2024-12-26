module App


module private Elmish =


    open Elmish
    open Feliz
    open Feliz.Router
    open Fable.Remoting.Client


    type Model = { HelloWorld: Deferred<string> }


    type Msg =
        | HelloWorld of AsyncOperationStatus<string>
        | NoOp


    let serverApi =
        Remoting.createApi ()
        |> Remoting.withRouteBuilder Api.routerPaths
        |> Remoting.buildProxy<Api.IServerApi>


    let init () =
        { HelloWorld = HasNotStartedYet }, Cmd.ofMsg (HelloWorld(Started))


    let update msg model =
        match msg with
        | HelloWorld Started ->
            Logging.log "Hello World" "Started"

            let cmd =
                async {
                    let! response = serverApi.test ()
                    return HelloWorld(Finished response)
                }
                |> Cmd.fromAsync

            { model with HelloWorld = InProgress }, cmd
        | HelloWorld(Finished value) ->
            Logging.log "Hello World Finished" value

            {
                model with
                    HelloWorld = Resolved value
            },
            Cmd.none
        | NoOp -> failwith "Not Implemented"


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

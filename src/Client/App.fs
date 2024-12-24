module App

open Fable.Core
open Browser
open Fable.React


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

            { model with
                HelloWorld = Resolved value
            },
            Cmd.none
        | NoOp -> failwith "Not Implemented"


[<JSX.Component>]
let View () =
    let state, dispatch = React.useElmish (Elmish.init, Elmish.update, [||])

    Logging.log "Hello World" state.HelloWorld

    let data =
        [|
            {|
                x = [| 1; 2; 3 |]
                y = [| 2; 1; 2 |]
                ``type`` = "scatter"
                ``mode`` = "lines+markers"
                marker = {| color = "red" |}
            |}
            |> box
            {|
                ``type`` = "bar"
                x = [| 1; 2; 3 |]
                y = [| 2; 1; 2 |]
            |}
            |> box
        |]

    let layout =
        {|
            width = 320
            height = 240
            title = "A Fancy Plot!"
        |}

    let sx = {| flexGrow = 1 |}

    JSX.jsx
        $"""
        import CssBaseline from '@mui/material/CssBaseline';
        import Typography from '@mui/material/Typography';
        import Container from '@mui/material/Container';
        import Box from '@mui/material/Box';
        import React from 'react';
        import Plot from 'react-plotly.js';
        
        <React.Fragment>
            <CssBaseline enableColorScheme />
            <Container>
                <Typography variant="h3" component="div" sx={sx}>
                    Hello World
                </Typography>
                <Plot
                    data= {data}
                    layout= {layout}
                />            
            </Container>
        </React.Fragment>
    """


let app = ReactDomClient.createRoot (document.getElementById "app")
app.render (View() |> toReact)

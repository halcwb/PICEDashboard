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

    JSX.jsx
        """
        <div>
            <h1>Hello World</h1>
        </div>
    """


let app = ReactDomClient.createRoot (document.getElementById "app")
app.render (View() |> toReact)

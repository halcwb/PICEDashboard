namespace Components


module Dialog =


    open Fable.Core


    [<JSX.Component>]
    let View
        (props:
            {|
                title: string
                content: string
                dispatch: (unit -> unit)
            |})
        =
        JSX.jsx
            $"""
        
        """

(*

            Mui.dialog [
                dialog.open' true
                dialog.onClose (fun _ -> () |> props.dispatch)
                dialog.children [
                    Mui.dialogTitle [ props.title |> Components.Markdown.render ]
                    Mui.dialogContent [ 
                        props.content |> Components.Markdown.render 
                    ]
                    Mui.dialogActions [
                        Mui.button [
                            prop.onClick (fun _ -> () |> props.dispatch)
                            prop.text "Ok"
                        ]
                    ]
                ]
            ]
    *)

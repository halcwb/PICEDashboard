namespace Components

module Dialog =

    open Feliz
    open Feliz.MaterialUI

    let private comp =
        React.functionComponent("dialog", fun (props : {| title : string; content : string; dispatch : (unit -> unit) |}) ->
            Mui.dialog [
                dialog.open' true
                dialog.onClose (fun _ -> () |> props.dispatch)
                dialog.children [
                    Mui.dialogTitle [ prop.text props.title]
                    Mui.dialogContent [ 
                        prop.text props.content 
                    ]
                    Mui.dialogActions [
                        Mui.button [
                            prop.onClick (fun _ -> () |> props.dispatch)
                            prop.text "Ok"
                        ]
                    ]
                ]
            ]
        )

    let render title content dispatch = comp({| title = title; content = content; dispatch = dispatch |})


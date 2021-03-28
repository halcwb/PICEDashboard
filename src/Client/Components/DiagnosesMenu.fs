namespace Components

module DiagnosesMenu =

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI

    open Informedica.PICE.Shared.Types
    module Filter = Informedica.PICE.Shared.Filter

    let drawerWidth = 300


    type Msg = 
        | ShowReport


    let init () = {| showReport = false; selected = [] |}, Cmd.none


    let update dispatch msg (state : {| showReport : bool; selected : string list |}) =
        match msg with
        | ShowReport ->
            let state = {| state with showReport = true |}
            state, Cmd.ofSub(fun _ -> state |> dispatch)




    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            root = styles.create [
                style.display.flex
            ]
            label = styles.create [
                style.color theme.palette.primary.dark
            ]
            toolbar = styles.create [
                yield! theme.mixins.toolbar
            ]
            drawer = styles.create [
                style.marginTop 20
                style.padding 10
                style.flexGrow 1
                style.width drawerWidth
            ]
        |}
    )

    let private comp =
        React.functionComponent("treeview", 
            fun (props : {| isOpen : bool; diagnoses: (string * int) list; selected : string list; dispatch : {| showReport : bool; selected : string list |} -> unit |}) ->
            let classes = useStyles ()
            let state, dispatch = React.useElmish(init, update props.dispatch, [||])

            let goToReportButton =
                Mui.button [
                    prop.onClick (fun _ -> ShowReport |> dispatch)
                    prop.children [
                        Mui.typography [
                            typography.color.textPrimary
                            prop.text "ga naar rapport"
                        ]
                    ]
                ]

            let diagnosesList =
                props.diagnoses
                |> List.map (fun (k, v) -> k, sprintf "%s (%A)" k v)
                |> List.sort
                |> List.map (fun (value, label) ->
                    Mui.formControlLabel [
                        formControlLabel.control (                            
                            Mui.checkbox [
                                checkbox.value value
                                checkbox.onChange (fun b ->
                                    if b then 
                                        {| showReport = false
                                           selected = value::props.selected |} 
                                        |> props.dispatch
                                    else
                                        {| 
                                            showReport = false
                                            selected = 
                                                props.selected
                                                |> List.filter ((<>) value)
                                        |}
                                        |> props.dispatch
                                ) 
                            ]
                        )
                        formControlLabel.label label
                    ]
                    |> fun el -> Mui.listItem [ el ]
                )
                |> fun els ->
                    Mui.list [
                        prop.style [ 
                            style.maxHeight 1000
                            style.overflow.auto
                        ]
                        list.children els
                    ]

            Html.div [
                prop.className classes.root
                prop.children [
                    Mui.drawer [
                        drawer.open' props.isOpen
                        drawer.variant.persistent
                        drawer.anchor.left
//                        prop.className classes.drawer
                        drawer.classes.paper classes.drawer
                        drawer.children [
                            // this makes sure that the content of the drawer is
                            // below the app bar
                            Html.div [ prop.className classes.toolbar ]
                            goToReportButton
                            diagnosesList
                        ]
                    ]

                ]

            ]
        )

    let render isOpen diagnoses selected dispatch = comp ({| isOpen = isOpen; diagnoses = diagnoses; selected = selected; dispatch = dispatch |})
namespace Components

module AppBar =


    open Elmish
    open Feliz
    open Feliz.MaterialUI


    let createButton (name : string) dispatch (el : ReactElement) =
        Mui.iconButton [
            prop.className name
            prop.onClick (fun _ -> dispatch())

            iconButton.color.inherit'
            iconButton.children [
                el
            ]
        ]


    let useStyles = Styles.makeStyles (fun styles theme ->
        {|
            appBar = styles.create [
                style.display.flex
                style.flexDirection.row
                style.padding (theme.spacing 1)
                style.zIndex (theme.zIndex.drawer + 1)
            ]

            menuButton = styles.create [
                    style.padding (theme.spacing 1)
            ]

            title = styles.create [
                style.padding (theme.spacing 1)
            ]
        |}
    )


    type Props =
        {|
            title : string
            buttonsLeft : {| button : ReactElement; dispatch : unit -> unit |} list
        |}


    let private comp =
        React.functionComponent("titlebar", fun (props: Props) ->
            let classes = useStyles()

            Mui.appBar [
                appBar.classes.root classes.appBar
                appBar.position.fixed'
                appBar.children [
                    // buttons on the left side
                    for b in props.buttonsLeft do
                        createButton classes.menuButton b.dispatch b.button
                    // title
                    Mui.typography [
                        prop.className classes.title
                        typography.variant.h6
                        prop.text props.title
                    ]
                ]
            ]
        )


    let render title buttons =
            let buttons =
                buttons
                |> List.map (fun (b, d) -> {| button = b; dispatch = d |})
            comp ({| title = title; buttonsLeft = buttons |})
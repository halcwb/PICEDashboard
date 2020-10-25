namespace Components

module ExpansionPanelList =

    open Feliz
    open Elmish
    open Fable.MaterialUI.Icons
    open Feliz.MaterialUI

    let comp =
        React.functionComponent("expansionpanellist", fun (props : {| items : {| summary : ReactElement; details : ReactElement list |} list |}) ->
            Mui.container [
                prop.style [
                    style.display.flex
                    style.flexDirection.column
                ]
                container.disableGutters true
                container.children [
                    for item in props.items do
                        Mui.expansionPanel [
                            Mui.expansionPanelSummary [

                                expansionPanelSummary.expandIcon (expandMoreIcon "")

                                prop.style [
                                    style.backgroundColor Colors.green.``100``
                                ]

                                expansionPanelSummary.children [ item.summary ]
                            ]

                            Mui.expansionPanelDetails [
                                prop.style [ 
                                    style.flexDirection.column
                                ]
                                expansionPanelDetails.children item.details
                            ]
                        ]
                    ]
                ]
            
        )

    let render items = comp({| items = items |})

namespace Components

module AccordionList =

    open Feliz
    open Elmish
    open Fable.MaterialUI.Icons
    open Feliz.MaterialUI

    let comp =
        React.functionComponent("accordionlist", fun (props : {| items : {| summary : ReactElement; details : ReactElement list |} list |}) ->
            Mui.container [
                prop.style [
                    style.display.flex
                    style.flexDirection.column
                ]
                container.disableGutters true
                container.children [
                    for item in props.items do
                        Mui.accordion [
                            Mui.accordionSummary [

                                accordionSummary.expandIcon (expandMoreIcon "")

                                prop.style [
                                    style.backgroundColor Colors.green.``100``
                                ]

                                accordionSummary.children [ item.summary ]
                            ]

                            Mui.accordionDetails [
                                prop.style [ 
                                    style.flexDirection.column
                                ]
                                accordionDetails.children item.details
                            ]
                        ]
                    ]
                ]
            
        )

    let render items = comp({| items = items |})

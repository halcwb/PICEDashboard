namespace Components

module AccordionList =

    open Feliz
    open Elmish
    open Fable.MaterialUI.Icons
    open Feliz.MaterialUI

    let comp =
        React.functionComponent("accordionlist", fun (props : {| items : {| summary : string * ReactElement; details : ReactElement list |} list |}) ->
            
            Mui.container [
                prop.style [
                    style.display.flex
                    style.flexDirection.column
                ]
                container.disableGutters true
                container.children [
                    for item in props.items do
                        Mui.accordion [
                            accordion.defaultExpanded true
                            accordion.children [
                                Mui.accordionSummary [
                                    accordionSummary.expandIcon (expandMoreIcon "")
                                    item.summary |> fst |> prop.className 
                                    accordionSummary.children [ 
                                        item.summary |> snd
                                    ]
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
                ]           
        )

    let render items = comp({| items = items |})

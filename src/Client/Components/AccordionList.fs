namespace Components

open Fable.Core

module AccordionList =

    open Feliz
    open Elmish

    [<JSX.Component>]
    let View
        (props:
            {|
                items:
                    {|
                        summary: string * ReactElement
                        details: ReactElement list
                    |} list
            |})
        =

        JSX.jsx
            $"""
        import Accorion from '@mui/Accorion';
        import Container from '@mui/Container';

        <Container>
            
        </Container>
        """
(*
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
                            accordion.square true
                            accordion.elevation 0
                            accordion.children [
                                Mui.accordionSummary [
//                                    accordionSummary.expandIcon (expandMoreIcon "")
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
            *)

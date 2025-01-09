namespace Components

open Fable.Core

module Accordion =

    open Feliz
    open Elmish

    [<JSX.Component>]
    let View
        (props:
            {|
                item:
                    {|
                        summary: ReactElement
                        details: ReactElement list
                    |}
                isOpen: bool
            |})
        =
        let (value, setValue) = React.useState (false)
        let expanded = props.isOpen

        if expanded && value <> expanded then
            setValue (expanded)

        let toggle = fun _ -> setValue (not value)

        JSX.jsx
            $"""
        import Accordion from '@mui/material/Accordion';
        import AccordionDetails from '@mui/material/AccordionDetails';
        import AccordionSummary from '@mui/material/AccordionSummary';
        
        <Accordion
            expanded={value}
            onChange={toggle}
            square={true}
            elevation={0}
        >
            <AccordionSummary>
                {props.item.summary}
            </AccordionSummary>
            <AccordionDetails>
                {props.item.details}
            </AccordionDetails>
        </Accordion>
        """

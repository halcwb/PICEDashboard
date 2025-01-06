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
                        summary: ReactElement
                        details: ReactElement list
                    |} list
            |})
        =

        let accordion summary details =
            JSX.jsx
                $"""
            import Accordion from '@mui/material/Accordion';
            import AccordionDetails from '@mui/material/AccordionDetails';
            import AccordionSummary from '@mui/material/AccordionSummary';
            
            <Accordion
                defaultExpanded={false}
                square={true}
                elevation={0}
            >
                <AccordionSummary>
                    {summary}
                </AccordionSummary>
                <AccordionDetails>
                    {details}
                </AccordionDetails>
            </Accordion>
            """

        let items =
            props.items |> List.map (fun item -> accordion item.summary item.details)

        JSX.jsx
            $"""
        import Stack from '@mui/material/Stack';

        <Stack >
            {items}
        </Stack>
        """

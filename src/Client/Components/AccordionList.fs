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
        let (value, setValue) = React.useState (false)
        let expanded = props.items.Length = 1

        if value <> expanded then
            setValue (expanded)

        Logging.log "value" value

        let accordion summary details =
            JSX.jsx
                $"""
            import Accordion from '@mui/material/Accordion';
            import AccordionDetails from '@mui/material/AccordionDetails';
            import AccordionSummary from '@mui/material/AccordionSummary';
            
            <Accordion
                expanded={value}
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

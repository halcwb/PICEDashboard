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
                        details: ReactElement[]
                    |}[]
            |})
        =

        let accordion summary details =
            let props =
                {|
                    item =
                        {|
                            summary = summary
                            details = details
                        |}
                    isOpen = props.items.Length = 1
                |}

            Accordion.View(props)

        let items =
            props.items |> Array.map (fun item -> accordion item.summary item.details)

        JSX.jsx
            $"""
        import Stack from '@mui/material/Stack';

        <Stack >
            {items}
        </Stack>
        """

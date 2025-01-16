namespace Components

open Fable.Core
open Feliz

module ColoredList =


    module private Utils =


        let createListItem color background text =
            {|
                text = text
                color = color
                background = background
            |}


        let coloredItemList xs =
            xs |> List.mapi (fun i s -> createListItem color.white (Colors.getColor i) s)

        let keyValueListToColoredItems kvs =
            let t = kvs |> List.map snd |> List.sum |> float

            kvs
            |> List.map (fun (k, v) -> sprintf "%s: %i (%.0f" k v (100. * (v |> float) / t) |> sprintf "%s%%)")
            |> coloredItemList


    [<JSX.Component>]
    let View
        (props:
            {|
                text: string
                color: string
                background: string
            |}[])
        =

        let items =
            props
            |> Array.mapi (fun i p ->
                let sx =
                    {|
                        fontWeight = "bold"
                        color = p.color
                        backgroundColor = p.background
                    |}

                let key = $"%i{i}-%s{p.text}"

                JSX.jsx
                    $"""
                import Typography from '@mui/material/Typography';
                import ListItem from '@mui/material/ListItem';

                <ListItem 
                    divider="true"
                    sx={sx}
                    button="true"
                    key={key}>
                    <Typography 
                        variant="body1"
                        >
                        {p.text}
                    </Typography>                  
                </ListItem>
                """)


        JSX.jsx
            $"""
        import List from '@mui/material/List';

        <List>
            {items}
        </List>        
        """

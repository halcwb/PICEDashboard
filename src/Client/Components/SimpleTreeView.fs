namespace Components

module SimpleTreeView =

    open Elmish
    open Fable.Core
    open Fable.React


    [<JSX.Component>]
    let View (props: {| items: JSX.Element[] |}) =

        JSX.jsx
            $"""
        import {{ SimpleTreeView }} from '@mui/x-tree-view/SimpleTreeView';

        <SimpleTreeView >
            {props.items}
        </SimpleTreeView>
        """

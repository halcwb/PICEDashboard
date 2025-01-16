namespace Components

module TreeItem =

    open Fable.Core


    [<JSX.Component>]
    let View
        (props:
            {|
                id: string
                label: string
                children: JSX.Element[]
                dispatch: unit -> unit
            |})
        =
        JSX.jsx
            $""" 
        import {{ TreeItem }} from '@mui/x-tree-view/TreeItem';

        <TreeItem key={props.id} itemId={props.id} id={props.id} label= {props.label} onClick={props.dispatch} >
            {props.children}
        </TreeItem>
        """

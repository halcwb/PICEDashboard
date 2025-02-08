namespace Components

module SimpleTreeView =

    open Elmish
    open Fable.Core
    open Fable.React
    open Types

    module private TreeItem =


        let private renderTreeItem id label children dispatch =
            JSX.jsx
                $"""
            import React from 'react';
            import {{ TreeItem }} from '@mui/x-tree-view/TreeItem';
        
            <TreeItem key={id} itemId={id} id={id} label= {label} onClick={dispatch} >
                {children}
            </TreeItem>
            """


        let rec treeItems dispatch (data: TreeData[]) =
            let map d =
                let children =
                    if d.children |> Array.isEmpty then
                        [||]
                    else
                        d.children |> treeItems dispatch

                renderTreeItem d.id d.label children (dispatch d.id)

            data |> Array.map (fun d -> d.id, d) |> Array.mapKeyEls map


    open TreeItem


    [<JSX.Component>]
    let View
        (props:
            {|
                data: TreeData[]
                dispatch: string -> unit -> unit
            |})
        =

        JSX.jsx
            $"""
        import {{ SimpleTreeView }} from '@mui/x-tree-view/SimpleTreeView';

        <SimpleTreeView >
            {props.data |> treeItems props.dispatch}
        </SimpleTreeView>
        """

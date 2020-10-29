namespace Components

module ColoredList =

    open Feliz
    open Feliz.MaterialUI

    type ListItem = { text : string; color : string ; background : string }


    let createListItem color background text = { text = text; color = color; background = background }


    let coloredItemList xs =
        xs
        |> List.mapi (fun i s ->
            createListItem color.white (Utils.getColor i) s
        )

    let keyValueListToColoredItems kvs =
        let t = kvs |> List.map snd |> List.sum |> float
        kvs
        |> List.map (fun (k, v) ->
            sprintf "%s: %i (%.0f" k v (100. * (v |> float) / t)
            |> sprintf "%s%%)"
        )
        |> coloredItemList

    let private comp =
        React.functionComponent("colored-list", fun (props: {| items : ListItem list|}) ->
            let items =
                props.items
                |> List.map (fun item ->
                    Mui.listItem [
                        listItem.divider true
                        listItem.button true
                        // prop.style [ style.backgroundColor item.background ]
                        let children = 
                            Mui.typography [
                                typography.variant.body1
                                prop.style [ 
                                    style.fontWeight.bold 
                                    style.color item.background
//                                    style.backgroundColor item.background
                                ]
                                prop.text item.text
                            ]
                        listItem.children children
                    ]

                )

            Mui.list [
                list.children items
            ]
        )

    let render items = comp({| items = items |})
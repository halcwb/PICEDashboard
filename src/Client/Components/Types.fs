namespace Components


module Types =


    type TreeData =
        {
            id: string
            label: string
            children: TreeData[]
        }


    type DropDownItems =
        {
            Value: string
            FirstIsNone: bool
            Items: string[]
            Label: string
            Dispatch: string -> unit
        }

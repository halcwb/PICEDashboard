namespace Components

module DropDownBox =
    open System

    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fetch.Types
    open Thoth.Fetch
    open Thoth.Json
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop


    let createMenuItems items =
        items
        |> List.mapi (fun i (s: string) ->
            Mui.menuItem [
                prop.value i
                prop.children
                    [
                        Mui.typography [
                            typography.color.primary
                            typography.variant.body1
                            prop.text (if s |> String.IsNullOrEmpty then "Geen" else s)
                        ]
                    ]
            ])


    type Props =
        {
            Value: string
            FirstIsNone: bool
            Items: string list
            Label: string
            Dispatch: string -> unit
        }


    let private comp =
        React.functionComponent
            ("dropdownbox",
             (fun (props: Props) ->

                 Mui.formControl [
                     Mui.inputLabel props.Label
                     Mui.select [
                         match props.Items |> List.tryFindIndex ((=) props.Value) with
                         // when first is none display nothing
                         | Some i when i = 0 && props.FirstIsNone -> select.value ""
                         // otherwise display the value
                         | Some i -> select.value i
                         // display nothing of no value can be found
                         | None -> select.value ""

                         select.onChange (fun (e: int) -> props.Items.[e] |> props.Dispatch)

                         props.Items |> createMenuItems |> prop.children
                     ]
                 ]))


    let renderWithProps props = comp props

    let render value firstIsNone label dispatch items =
        {
            Value = value
            FirstIsNone = firstIsNone
            Items = items
            Label = label
            Dispatch = dispatch
        }
        |> renderWithProps
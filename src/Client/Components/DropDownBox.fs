namespace Components

module DropDownBox =

    open System

    open Elmish
    open Elmish.React
    open Fable.React
    open Feliz
    open Fable.React
    open Fable.Core.JsInterop
    open Fable.Core

    open Shared


    module private Elmish =

        let createMenuItems items = []
    (*
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
        *)


    [<JSX.Component>]
    let View (props: DropDownItems) =


        JSX.jsx
            $"""
        
        """

(*
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
        *)

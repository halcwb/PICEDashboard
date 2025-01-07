[<AutoOpen>]
module Utils

open System


module String =

    let replace (oldS: string) newS (s: string) = s.Replace(oldS, newS)


    let split (s: string) = s.Split('.') |> Array.toList

    let split2 (s: String) = s.Split('|') |> Array.toList


module Math =

    let round (n: int) (c: float) = Math.Round(c, n)

    let calcAverage getTotal getCount tots =
        let t =
            tots
            |> List.sumBy (fun t ->
                if t |> getTotal = 0 then
                    0.
                else
                    (t |> getCount) / (t |> getTotal |> float))
            |> float

        t / (tots |> List.length |> float)


open Fable.Core
open Feliz
open Browser.Types

let inline toJsx (el: ReactElement) : JSX.Element = unbox el
let inline toReact (el: JSX.Element) : ReactElement = unbox el

/// Enables use of Feliz styles within a JSX hole
let inline toStyle (styles: IStyleAttribute list) : obj = JsInterop.createObj (unbox styles)


let toClass (classes: (string * bool) list) : string =
    classes
    |> List.choose (fun (c, b) ->
        match c.Trim(), b with
        | "", _
        | _, false -> None
        | c, true -> Some c)
    |> String.concat " "


let onEnterOrEscape dispatchOnEnter dispatchOnEscape (ev: KeyboardEvent) =
    let el = ev.target :?> HTMLInputElement

    match ev.key with
    | "Enter" ->
        dispatchOnEnter el.value
        el.value <- ""
    | "Escape" ->
        dispatchOnEscape ()
        el.value <- ""
        el.blur ()
    | _ -> ()


module Logging =

    open Browser.Dom

    let log (msg: string) a = console.log (box msg, [| box a |])

    let error (msg: string) e = console.error (box msg, [| box e |])

    let warning (msg: string) a = console.warn (box msg, [| box a |])


module GoogleDocs =

    open System
    open Fable.SimpleHttp

    let inline getUrl parseResponse msg url =
        async {
            let! (statusCode, responseText) = Http.get url

            let result =
                match statusCode with
                | 200 ->
                    responseText
                    //                    |> Csv.parseCSV
                    |> parseResponse
                    |> Ok
                    |> Finished
                | _ -> Finished(Error $"Status {statusCode} => {responseText}")
                |> msg

            return result
        }


    let createUrl sheet id =
        $"https://docs.google.com/spreadsheets/d/{id}/gviz/tq?tqx=out:csv&sheet={sheet}"


module Colors =


    let bgColors = [|
        color.darkBlue
        color.darkGreen
        color.darkRed
        color.darkOrange
        color.darkOrchid
        color.darkSeaGreen
        color.darkCyan
        color.darkGoldenRod
        color.darkViolet
        color.darkGray
        color.darkOliveGreen
        color.darkKhaki
        color.darkMagenta
        color.darkSalmon
        color.darkSlateBlue
        color.darkSlateGray
        color.darkTurqouise
    |]

    let getColor i = bgColors.[i % bgColors.Length]

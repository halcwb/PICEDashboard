namespace Informedica.PICE.Lib

module Markdown =

    open System
    open System.Text
    
    open Markdig


    module Literals =

        [<Literal>]
        let line = "---"
        [<Literal>]
        let headers2 = "|:---:|:---:|"
        [<Literal>]
        let columns2 = "|{0}|{1}|"
        [<Literal>]
        let headers3 = "|:---:|:---:|:---:|"
        [<Literal>]
        let columns3 = "|{0}|{1}|{2}|"
        [<Literal>]
        let headers4 = "|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns4 = "|{0}|{1}|{2}|{3}|"
        [<Literal>]
        let headers5 = "|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns5 = "|{0}|{1}|{2}|{3}|{4}|"
        [<Literal>]
        let headers6 = "|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns6 = "|{0}|{1}|{2}|{3}|{4}|{5}|"
        [<Literal>]
        let headers7 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:"
        [<Literal>]
        let columns7 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|"
        [<Literal>]
        let headers8 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns8 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|"
        [<Literal>]
        let headers9 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns9 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|"
        [<Literal>]
        let headers16 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns16 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|{9}|{10}|{11}|{12}|{13}|{14}|{15:F0}|"
        [<Literal>]
        let headers17 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns17 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|{9}|{10}|{11}|{12}|{13}|{14}|{15:F0}|{16:F0}|"



    let createMDTable (sb : StringBuilder)  (xs : obj list list) =
        match xs with
        | h::tail ->
            let hdr, clmn =
                match h with
                | _ when h |> List.length = 2 -> Literals.headers2, Literals.columns2
                | _ when h |> List.length = 3 -> Literals.headers3, Literals.columns3
                | _ when h |> List.length = 4 -> Literals.headers4, Literals.columns4
                | _ when h |> List.length = 5 -> Literals.headers5, Literals.columns5
                | _ when h |> List.length = 6 -> Literals.headers6, Literals.columns6
                | _ when h |> List.length = 7 -> Literals.headers7, Literals.columns7
                | _ when h |> List.length = 8 -> Literals.headers8, Literals.columns8
                | _ when h |> List.length = 9 -> Literals.headers9, Literals.columns9
                | _ when h |> List.length = 16 -> Literals.headers17, Literals.columns16
                | _ when h |> List.length = 17 -> Literals.headers17, Literals.columns17
                | _ ->
                    h 
                    |> List.length
                    |> sprintf "unsupported columns count (min 2, max 9): %i" 
                    |> failwith
            sb
            |> StringBuilder.appendLineFormat clmn h
            |> StringBuilder.appendLine hdr
            |> fun sb ->
                tail
                |> List.fold (fun sb row ->
                    sb
                    |> StringBuilder.appendLineFormat clmn row
                ) sb

        | _ -> 
            "not a valid table"
            |> failwith


    let toHtml s =
        let pipeline = Markdig.MarkdownPipelineBuilder().UseAdvancedExtensions().Build()
        Markdown.ToHtml(s, pipeline)


    let htmlToBrowser html =
        let proc = new System.Diagnostics.Process()
        proc.EnableRaisingEvents <- false

        let tmp = IO.Path.GetTempPath() + "/temp.html"

        html
        |> File.writeTextToFile tmp

        proc.StartInfo.FileName <- tmp

        proc.Start() |> ignore
        proc.Close()


    let toBrowser s =
        s
        |> toHtml
        |> htmlToBrowser



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
        [<Literal>]
        let headers42 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns42 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|{9}|{10}|{11}|{12}|{13}|{14}|{15:F0}|{16:F0}|{17:F0}|{18:F0}|{19:F0}|{20}|{21}|{22}|{23}|{24}|{25}|{26:F0}|{27:F0}|{28}|{29}|{30}|{31}|{32}|{33}|{34:F0}|{35:F0}|{36:F0}|{37}|{38}|{39}|{40}|{41}|"
        [<Literal>]
        let headers47 = "|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns47 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|{9}|{10}|{11}|{12}|{13}|{14}|{15:F0}|{16:F0}|{17:F0}|{18:F0}|{19:F0}|{20}|{21}|{22}|{23}|{24}|{25}|{26:F0}|{27:F0}|{28}|{29}|{30}|{31}|{32}|{33}|{34:F0}|{35:F0}|{36:F0}|{37}|{38}|{39}|{40}|{41}|{42}|{43:F0}|{44:F0}|{45:F0}|{46:F0}|"



    let createMDTable (sb : StringBuilder)  (xs : obj list list) =
        match xs with
        | h::tail ->
            let c = xs |> List.head |> List.length
            if xs |> List.forall (List.length >> ((=) c)) |> not then 
                failwith "all rows should have equal length"
            let hdr, clmn =
                let hdr = "|:--:"
                let clm = "|{x}"
                let c = h |> List.length 
                [0..(c - 1)]
                |> List.fold (fun (h, c) i ->
                    h + hdr, c + (clm |> String.replace "x" (i |> string))
                ) ("", "")
                |> fun (hdr, clm) ->
                    hdr + "|", clm + "|"
            try 
                sb
                |> StringBuilder.appendLineFormat clmn h
                |> StringBuilder.appendLine hdr
                |> fun sb ->
                    tail
                    |> List.fold (fun sb row ->
                        sb
                        |> StringBuilder.appendLineFormat clmn row
                    ) sb
            with 
            | e -> 
                sprintf "%s\n with hdr: %s\n clmn: %s" (e.ToString()) hdr clmn
                |> failwith
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



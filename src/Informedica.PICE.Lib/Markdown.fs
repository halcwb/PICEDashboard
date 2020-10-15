namespace Informedica.PICE.Lib

module Markdown =

    open System
    
    open Markdig


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

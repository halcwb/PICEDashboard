namespace Pages

open Shared

module Report =

    module private Utils =

        let selectReport (s: string) (report: Report) =
            Browser.Dom.console.log ("selecting", s)

            let rec selectChapter (ids: string list) (chapters: Chapter list) =
                Browser.Dom.console.log (
                    "select chapter",
                    ids |> String.concat ", ",
                    chapters |> List.map (fun c -> c.Title) |> String.concat ", "
                )

                match ids with
                | id :: tail ->
                    match id |> String.split2 with
                    | [ s; id ] when s = "C" ->
                        let chapter = chapters.[id |> int]
                        Browser.Dom.console.log ("selected chapter: ", chapter.Title)

                        match tail with
                        | [] -> [ chapter ]
                        | [ id ] ->
                            Browser.Dom.console.log ("Finishing with: ", id)

                            match id |> String.split2 with
                            | [ s; id ] when s = "C" ->
                                [
                                    { chapter with
                                        Chapters = [ chapter.Chapters.[id |> int] ]
                                        Paragraphs = []
                                    }
                                ]
                            | [ s; id ] when s = "P" ->
                                Browser.Dom.console.log ("Picked paragraph", id)

                                [
                                    { chapter with
                                        Paragraphs = [ chapter.Paragraphs.[id |> int] ]
                                        Chapters = []
                                    }
                                ]
                            | _ -> sprintf "failwith couldn't get %s" id |> failwith
                        | _ ->
                            [
                                { chapter with
                                    Paragraphs = []
                                    Chapters = (chapter.Chapters |> selectChapter tail)
                                }
                            ]
                    | _ -> sprintf "failwith couldn't get %s" id |> failwith
                | _ -> chapters

            match s |> String.split with
            | [ id ] ->
                { report with
                    Sections = report.Sections.[id |> int] |> List.singleton
                }
            | id :: tail ->
                { report with
                    Sections =
                        report.Sections.[id |> int]
                        |> fun section ->
                            { section with
                                Chapters = section.Chapters |> selectChapter tail
                            }
                        |> List.singleton
                }

            | _ -> report

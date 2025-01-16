namespace Pages

open Shared
open Types
open Fable.Core
open Fable.React
open Feliz


module Report =


    module private Utils =


        let renderMarkdown (md: string) =
            let props = {| md = md |}
            Components.Markdown.View(props) |> toReact


        let selectReport (s: string) (report: Report) =
            Browser.Dom.console.log ("selecting", s)

            let rec selectChapter (ids: string[]) (chapters: Chapter[]) =
                Browser.Dom.console.log (
                    "select chapter",
                    ids |> String.concat ", ",
                    chapters |> Array.map (fun c -> c.Title) |> String.concat ", "
                )

                if ids |> Array.length > 0 then
                    let id = ids[0]
                    let tail = ids |> Array.skip 1

                    match id |> String.split2 with
                    | [ s; id ] when s = "C" ->
                        let chapter = chapters[id |> int]
                        Browser.Dom.console.log ("selected chapter: ", chapter.Title)

                        match tail with
                        | [||] -> [| chapter |]
                        | [| id |] ->
                            Browser.Dom.console.log ("Finishing with: ", id)

                            match id |> String.split2 with
                            | [ s; id ] when s = "C" ->
                                [|
                                    { chapter with
                                        Chapters = [| chapter.Chapters.[id |> int] |]
                                        Paragraphs = [||]
                                    }
                                |]
                            | [ s; id ] when s = "P" ->
                                Browser.Dom.console.log ("Picked paragraph", id)

                                [|
                                    { chapter with
                                        Paragraphs = [| chapter.Paragraphs[id |> int] |]
                                        Chapters = [||]
                                    }
                                |]
                            | _ -> sprintf "failwith couldn't get %s" id |> failwith
                        | _ ->
                            [|
                                { chapter with
                                    Paragraphs = [||]
                                    Chapters = (chapter.Chapters |> selectChapter tail)
                                }
                            |]
                    | _ -> sprintf "failwith couldn't get %s" id |> failwith
                else
                    chapters

            match s |> String.split with
            | [ id ] ->
                { report with
                    Sections = report.Sections.[id |> int] |> Array.singleton
                }
            | id :: tail ->
                let tail = tail |> Array.ofList

                { report with
                    Sections =
                        report.Sections.[id |> int]
                        |> fun section ->
                            { section with
                                Chapters = section.Chapters |> selectChapter tail
                            }
                        |> Array.singleton
                }

            | _ -> report


        let getPieChart title (section: Section) get =
            let props =
                {|
                    title = title
                    data = section.Totals |> get
                    periods = section.YearTotals |> Array.map (fun t -> t.Period, t |> get)
                |}

            Components.PieChart.View(props)


        let getStackedBarChart title section get =
            let perYr =
                section.YearTotals
                |> Array.map (fun t -> t.Period, t |> get)
                |> Array.filter (fun (_, tots) -> tots |> Array.sumBy snd > 0)

            let perMo =
                section.MonthTotals
                |> Array.map (fun (yr, xs) ->
                    yr,
                    xs
                    |> Array.map (fun t -> t.Period, t |> get)
                    |> Array.filter (fun (p, tots) -> tots |> Array.sumBy snd > 0))

            let props =
                {|
                    title = title
                    perYear = perYr
                    perMonth = perMo
                |}

            Components.StackedBarChart.View(props)


        let layoutDetails (dt: DisplayType) (section: Section) =

            let mapParagraph (chapter: Chapter) paragraph =
                Html.div
                    [
                        prop.style [ style.paddingBottom 20 ]

                        match dt with
                        | Graph when
                            chapter.Title = Literals.groupDeathMode
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children [ (fun t -> t.DeathMode) |> getPieChart paragraph.Title section |> toReact ]

                        | Graph when
                            chapter.Title = Literals.groupDeathMode
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [
                                    (fun t -> t.DeathMode)
                                    |> getStackedBarChart "Reden van Overlijden" section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupMortality
                            && paragraph.Title = Literals.paragraphPIMandPRISM
                            ->
                            prop.children
                                [
                                    paragraph.Title |> sprintf "#### %s" |> renderMarkdown
                                    "##### Mortaliteit" |> renderMarkdown

                                    let props =
                                        {|
                                            totals = section.YearTotals
                                            content = paragraph.Content
                                        |}

                                    Views.MortalityGraph.View(props) |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupSMR
                            && paragraph.Title = Literals.paragraphSMRperYear
                            ->
                            prop.children
                                [
                                    "##### SMR per Jaar" |> renderMarkdown

                                    let props = {| totals = section.YearTotals |}

                                    Views.SMRGraph.View(props) |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupSMR
                            && paragraph.Title = Literals.paragraphSMRfunnel
                            ->
                            prop.children
                                [
                                    "##### SMR Funnelplot " |> renderMarkdown

                                    let props = {| totals = section.YearTotals |}

                                    Views.FunnelPlot.View(props) |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupAdmission
                            && paragraph.Title = Literals.paragraphAdmDisch
                            ->
                            prop.children
                                [
                                    "#### Opnames/ontslagen en ligdagen" |> renderMarkdown

                                    let props = {| totals = section.YearTotals |}

                                    Views.AdmissionsGraph.View(props) |> toReact
                                ]


                        | Graph when
                            chapter.Title = Literals.groupAdmission
                            && paragraph.Title = Literals.paragraphOccupancy
                            ->
                            prop.children
                                [

                                    let props =
                                        {|
                                            title = paragraph.Title
                                            data =
                                                section.YearTotals
                                                |> Array.map (fun ytot -> ytot.Period, ytot.Occupancy)
                                        |}

                                    Views.OccupancyGraph.View props |> toReact

                                    """Ga met de muis over de labels onderaan om de x-as om
                                    het gemiddelde of een jaar uit te lichten. Gebruik de bovenste 
                                    knoppen om door de jaren heen te lopen en een specifiek jaar te bekijken.
                                    """
                                    |> renderMarkdown
                                ]

                        | Graph when
                            chapter.Title = Literals.groupAdmission
                            && paragraph.Title = Literals.paragraphUrgency
                            ->
                            prop.children
                                [
                                    (fun t -> t.Urgency) |> getStackedBarChart "Opname Urgentie" section |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupAdmission
                            && paragraph.Title = Literals.paragraphReadmission
                            ->
                            prop.children
                                [
                                    (fun t -> t.Readmission) |> getStackedBarChart "Heropnames" section |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupAdmission
                            && paragraph.Title = Literals.paragraphLengthOfStay
                            ->
                            prop.children
                                [
                                    (fun t -> t.LengthOfStay) |> getStackedBarChart "Opname duur" section |> toReact
                                ]


                        | Graph when
                            chapter.Title = Literals.subGroupTransportHospital
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children
                                [
                                    (fun t -> t.TransportHospital) |> getPieChart paragraph.Title section |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.subGroupTransportHospital
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [
                                    (fun t -> t.TransportHospital)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.subGroupTransportTeam
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children
                                [ (fun t -> t.TransportTeam) |> getPieChart paragraph.Title section |> toReact ]

                        | Graph when
                            chapter.Title = Literals.subGroupTransportTeam
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [
                                    (fun t -> t.TransportTeam)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupGender
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children [ (fun t -> t.Gender) |> getPieChart paragraph.Title section |> toReact ]

                        | Graph when
                            chapter.Title = Literals.groupGender
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [ (fun t -> t.Gender) |> getStackedBarChart paragraph.Title section |> toReact ]

                        | Graph when chapter.Title = Literals.groupAge && paragraph.Title = Literals.paragraphTotals ->
                            prop.children [ (fun t -> t.AgeGroup) |> getPieChart paragraph.Title section |> toReact ]

                        | Graph when chapter.Title = Literals.groupAge && paragraph.Title = Literals.paragraphPerYear ->
                            prop.children
                                [
                                    (fun t -> t.AgeGroup) |> getStackedBarChart paragraph.Title section |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupDischargeReason
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children
                                [
                                    (fun t -> t.DischargeReasons) |> getPieChart paragraph.Title section |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupDischargeReason
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [
                                    (fun t -> t.DischargeReasons)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupDiagnoseGroup
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children
                                [

                                    (fun t -> t.DiagnoseGroups) |> getPieChart paragraph.Title section |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupDiagnoseGroup
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [
                                    (fun t -> t.DiagnoseGroups)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.groupSpecialism
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children [ (fun t -> t.Specialisme) |> getPieChart paragraph.Title section |> toReact ]

                        | Graph when
                            chapter.Title = Literals.groupSpecialism
                            && paragraph.Title = Literals.paragraphPerYear
                            ->
                            prop.children
                                [
                                    (fun t -> t.Specialisme)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.subGroupCanule
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children [ (fun t -> t.Cannule) |> getPieChart paragraph.Title section |> toReact ]

                        | Graph when
                            chapter.Title = Literals.subGroupVentilationDays
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children
                                [
                                    (fun t -> t.VentilationDays) |> getPieChart paragraph.Title section |> toReact
                                ]
                        | Graph when
                            chapter.Title = Literals.subGroupVentilationDays
                            && paragraph.Title = Literals.paragraphPerYear
                            ->

                            prop.children
                                [
                                    (fun t -> t.VentilationDays)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.subGroupVentilationDuration
                            && paragraph.Title = Literals.paragraphTotals
                            ->
                            prop.children
                                [
                                    (fun t -> t.VentilationDuration)
                                    |> getPieChart paragraph.Title section
                                    |> toReact
                                ]

                        | Graph when
                            chapter.Title = Literals.subGroupVentilationDuration
                            && paragraph.Title = Literals.paragraphPerYear
                            ->

                            prop.children
                                [
                                    (fun t -> t.VentilationDuration)
                                    |> getStackedBarChart paragraph.Title section
                                    |> toReact
                                ]

                        | _ ->
                            Logging.log "couldn't find graphs for " (chapter.Title, paragraph.Title)

                            prop.children
                                [
                                    //paragraph.Title |> sprintf "#### %s" |> Markdown.render
                                    //paragraph.Content |> Markdown.render
                                    Components.Markdown.View({| md = paragraph.Title |}) |> toReact
                                    Components.Markdown.View({| md = paragraph.Content |}) |> toReact
                                ]
                    ]

            let rec getDetails chapter =
                chapter.Paragraphs
                |> Array.map (mapParagraph chapter)
                |> fun els ->
                    if chapter.Chapters |> Array.isEmpty then
                        els
                    else
                        let details =
                            chapter.Chapters
                            |> Array.collect (fun chapter ->
                                Array.append
                                    [|
                                        let props =
                                            {|
                                                md = chapter.Title |> sprintf "#### %s"
                                            |}

                                        Components.Markdown.View(props) |> toReact
                                    |]
                                    (chapter |> getDetails))

                        Array.append els details

            let layoutChapters chapters =
                let props =
                    chapters
                    |> Array.map (fun chapter ->
                        let details = chapter |> getDetails

                        let summary =
                            JSX.jsx
                                $""" 
                                import React from 'react';
                                import Typography from '@mui/material/Typography';
                                <React.Fragment key={chapter.Title}>
                                    <Typography variant="h6" color="primary">
                                        {chapter.Title} 
                                    </Typography>
                                </React.Fragment>
                                """

                        {|
                            details = details
                            summary = summary |> toReact
                        |})

                Components.AccordionList.View({| items = props |})

            section.Chapters |> layoutChapters


        let layoutReport dt (sections: Section[]) =
            let sections =
                sections
                |> Array.map (fun sect ->
                    let det = layoutDetails dt sect

                    JSX.jsx
                        $"""
                    import React from 'react';

                    <React.Fragment key={sect.Title}>
                        {det}
                    </React.Fragment>
                    """)

            JSX.jsx
                $"""
            import React from 'react';
            <React.Fragment>
                {sections}
            </React.Fragment>
            """


    [<JSX.Component>]
    let View
        (props:
            {|
                displayType: DisplayType
                selected: string option
                report: Report
            |})
        =
        let report =
            match props.selected with
            | Some s -> Utils.selectReport s props.report
            | None -> props.report

        match props.displayType with
        | Print ->
            let sx = {| marginTop = 2; padding = 2 |}
            let md = Components.Markdown.View({| md = report.Markdown |})

            JSX.jsx
                $"""
            import Box from '@mui/material/Box';

            <Box sx={sx}>        
                {md}        
            </Box>
            """

        | _ -> Utils.layoutReport props.displayType report.Sections

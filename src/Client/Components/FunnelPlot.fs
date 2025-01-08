namespace Components

open System
open Fable.Core
open Shared

module FunnelPlot =


    type private Point =
        {
            name: string
            mortality: float
            smr: float
            reference: float
            upper: float
            lower: float
        }


    [<JSX.Component>]
    let View (props: {| totals: Totals list |}) =

        let pim2 =
            props.totals
            |> List.map (fun tot ->
                {
                    name = tot.Period
                    mortality = tot.PICUDeaths |> float
                    smr = (tot.PICUDeaths |> float) / tot.PIM2Mortality
                    reference = 1.
                    upper = 0.
                    lower = 0.
                })
            |> List.toArray

        let pim3 =
            props.totals
            |> List.map (fun tot ->
                {
                    name = tot.Period
                    mortality = tot.PICUDeaths |> float
                    smr = (tot.PICUDeaths |> float) / tot.PIM3Mortality
                    reference = 1.
                    upper = 0.
                    lower = 0.
                })
            |> List.toArray

        let prism =
            props.totals
            |> List.map (fun tot ->
                {
                    name = tot.Period
                    mortality = tot.PICUDeaths |> float
                    smr = (tot.PICUDeaths |> float) / tot.PRISM4Mortality
                    reference = 1.
                    upper = 0.
                    lower = 0.
                })
            |> List.toArray

        let data =
            let calcSD (p: Point) = Math.Sqrt(1. / (p.mortality |> float))


            prism
            |> Array.append pim2
            |> Array.append pim3
            |> Array.map (fun p ->
                { p with
                    name = "reference"
                    smr = 1.
                    upper = 1. + 1.96 * (calcSD p)
                    lower = 1. - 1.96 * (calcSD p)
                })
            |> Array.distinct

        JSX.jsx
            $"""
        import React from "react";
        import {{ ComposedChart, CartesianGrid, XAxis, YAxis, Legend, Scatter, Line }} from "recharts";

        <ComposedChart width={1100} height={600} data={data}>
            <CartesianGrid strokeDasharray="5 5" />
            <XAxis type="number" dataKey={fun p -> p.mortality} name="mortiliteit" tickCount={5} />
            <YAxis type="number" dataKey={fun p -> p.smr} name="smr" />
            <Legend />
            <Scatter name="PIM-2" data={pim2} fill="#006400" />
            <Scatter name="PIM-3" data={pim3} fill="#8B008B" />
            <Scatter name="PRISM" data={prism} fill="#008B8B" />
            <Line name="referentie" type="monotone" dataKey={fun p -> p.reference} strokeWidth={4} dot={false} strokeDasharray={[ 10, 10 ]} stroke="#00008B" />
            <Line name="boven grens" type="monotone" dataKey={fun p -> p.upper} strokeWidth={4} dot={false} strokeDasharray={[ 10, 10 ]} stroke="#8B0000" />
            <Line name="onder grens" type="monotone" dataKey={fun p -> p.lower} strokeWidth={4} dot={false} activeDot={false} strokeDasharray={[ 10, 10 ]} stroke="#006400" />
        </ComposedChart>
        """

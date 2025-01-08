namespace Components

open Fable.Core
open Shared

module SMRGraph =


    type private Point =
        {
            name: string
            smrPIM2: float
            smrPIM3: float
            smrPRISM: float
            reference: float
            averagePIM2: float
            averagePIM3: float
            averagePRISM: float
        }


    [<JSX.Component>]
    let View (props: {| totals: Totals list |}) =
        let round = Math.round

        let calcSMR get1 get2 tot =
            let obs = tot |> get1
            let est = tot |> get2
            (obs |> float) / est |> round 2

        let calcAverage get =
            let calc t = calcSMR (fun t -> t.PICUDeaths) get t

            props.totals |> Math.calcAverage (fun _ -> 1) calc |> round 2

        let data =
            props.totals
            |> List.map (fun tot ->
                let calc = calcSMR (fun t -> t.PICUDeaths)

                {
                    name = tot.Period
                    smrPRISM = tot |> calc (fun t -> t.PRISM4Mortality)
                    smrPIM2 = tot |> calc (fun t -> t.PIM2Mortality)
                    smrPIM3 = tot |> calc (fun t -> t.PIM3Mortality)
                    reference = 1.
                    averagePIM2 = calcAverage (fun t -> t.PIM2Mortality)
                    averagePIM3 = calcAverage (fun t -> t.PIM3Mortality)
                    averagePRISM = calcAverage (fun t -> t.PRISM4Mortality)
                })
            |> List.toArray

        JSX.jsx
            $"""
        import React from 'react';
        import {{ ComposedChart, Line, CartesianGrid, XAxis, YAxis, Tooltip, Legend }} from 'recharts';

        <ComposedChart
            width={1100}
            height={700}
            data={data}
        >
            <CartesianGrid strokeDasharray={[| 1; 1 |]} />
            <XAxis dataKey={fun p -> p.name} />
            <YAxis tickCount={5} />
            <Tooltip />
            <Line name="SMR-PRISM" dataKey={fun p -> p.smrPRISM} strokeWidth={4} stroke="#008b8b" />
            <Line name="SMR-PIM2" dataKey={fun p -> p.smrPIM2} strokeWidth={4} stroke="#006400" />
            <Line name="SMR-PIM3" dataKey={fun p -> p.smrPIM3} strokeWidth={4} stroke="#8b008b" />
            <Line name="referentie" dot={false} dataKey={fun p -> p.reference} strokeWidth={4} strokeDasharray={[ 10, 5 ]} stroke="#000000" />
            <Line name="gem. PRISM-IV" dot={false} dataKey={fun p -> p.averagePRISM} strokeWidth={4} strokeDasharray={[ 10, 5 ]} stroke="#008b8b" />
            <Line name="gem. PIM-2" dot={false} dataKey={fun p -> p.averagePIM2} strokeWidth={4} strokeDasharray={[ 10, 5 ]} stroke="#006400" />
            <Line name="gem. PIM-3" dot={false} dataKey={fun p -> p.averagePIM3} strokeWidth={4} strokeDasharray={[ 10, 5 ]} stroke="#8b008b" />
            <Legend verticalAlign="top" />
        </ComposedChart>
        """

namespace Components


open Fable.Core


module MortalityGraph =

    open Feliz
    open System
    open Shared

    type private Point =
        {
            name: string
            mortality: float
            pim2: float
            pim3: float
            prism: float
            average: float
            averagePIM2: float
            averagePIM3: float
            averagePRISM: float
        }


    [<JSX.Component>]
    let View
        (props:
            {|
                totals: Totals list
                content: string
            |})
        =

        let calcAverage get =
            props.totals
            |> Math.calcAverage (fun t -> t.Admissions) get
            |> fun x -> x * 100. |> Math.round 2

        let data =
            props.totals
            |> List.toArray
            |> Array.map (fun tot ->

                let perc c =
                    Math.Round(100. * c / float tot.Admissions, 1)
                // Browser.Dom.console.log(tot.Year, tot.Totals.PRISM4Mortality)
                {
                    name = tot.Period
                    mortality = tot.PICUDeaths |> float |> perc
                    pim2 = tot.PIM2Mortality |> perc
                    pim3 = tot.PIM3Mortality |> perc
                    prism = tot.PRISM4Mortality |> perc
                    average = calcAverage (fun t -> t.Deaths |> float)
                    averagePIM2 = calcAverage (fun t -> t.PIM2Mortality)
                    averagePIM3 = calcAverage (fun t -> t.PIM3Mortality)
                    averagePRISM = calcAverage (fun t -> t.PRISM4Mortality)
                })

        let getDataKey = fun p -> p.name

        JSX.jsx
            $"""
        import React from 'react';
        import {{ ComposedChart, CartesianGrid, XAxis, YAxis, Tooltip, Legend, Bar, Line }} from 'recharts';

        <ComposedChart
            width={1100}
            height={700}
            data={data}
        >
            <CartesianGrid strokeDasharray="1 1" />
            <XAxis dataKey={getDataKey} />
            <YAxis minTickGap={10} />
            <Tooltip />
            <Bar name="Mortaliteit" dataKey={fun p -> p.mortality} fill={color.darkBlue} />
            <Bar name="PIM-2" dataKey={fun p -> p.pim2} fill={color.darkGreen} />
            <Bar name="PIM-3" dataKey={fun p -> p.pim3} fill={color.darkMagenta} />
            <Bar name="PRISM-4" dataKey={fun p -> p.prism} fill={color.darkCyan} />

            <Line name="gemiddelde" monotone dot={false} dataKey={fun p -> p.average} strokeWidth={4} strokeDasharray={[| 10; 10 |]} stroke={color.darkBlue} />
            <Line name="gem. PIM2" monotone dot={false}  dataKey={fun p -> p.averagePIM2} strokeWidth={4} strokeDasharray={[| 10; 10 |]} stroke={color.darkGreen} />
            <Line name="gem. PIM3" monotone dot={false}  dataKey={fun p -> p.averagePIM3} strokeWidth={4} strokeDasharray={[| 10; 10 |]} stroke={color.darkMagenta} />
            <Line name="gem. PRISM" monotone dot={false}  dataKey={fun p -> p.averagePRISM} strokeWidth={4} strokeDasharray={[| 10; 10 |]} stroke={color.darkCyan} />

            <Legend verticalAlign="top" />
        </ComposedChart>
        """

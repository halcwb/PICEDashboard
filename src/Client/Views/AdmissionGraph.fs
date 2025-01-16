namespace Views


module AdmissionsGraph =

    open Feliz
    open Shared
    open Fable.Core
    open System

    type private Point =
        {
            name: string
            admitted: int
            discharged: int
            picuDays: int
        }


    [<JSX.Component>]
    let View (props: {| totals: Totals[] |}) =

        let data =
            props.totals
            |> Array.map (fun tot ->
                {
                    name = tot.Period
                    admitted = tot.Admissions
                    discharged = tot.Discharged
                    picuDays = tot.PICUDays
                })

        JSX.jsx
            $"""
            import {{ BarChart, CartesianGrid, YAxis, XAxis, Tooltip, Bar, Legend }} from 'recharts';

            <BarChart
                width={1100}
                height={700}
                data={data}>
                <CartesianGrid strokeDasharray="1 1" />
                <XAxis dataKey={fun p -> p.name} />
                <YAxis />
                <Tooltip />
                <Bar name="Opnames" dataKey={fun p -> p.admitted} fill={color.darkBlue} />
                <Bar name="Ontslagen" dataKey={fun p -> p.discharged} fill={color.darkGreen} />
                <Bar name="Ligdagen" dataKey={fun p -> p.picuDays} fill={color.darkMagenta} />
                <Legend verticalAlign="top" />
            </BarChart>
        """

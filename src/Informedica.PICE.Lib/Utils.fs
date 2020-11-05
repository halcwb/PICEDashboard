namespace Informedica.PICE.Lib

module Utils =

    open System


    let kiloPascalToMmHg n = n * 7.50061683
    
    
    let calcRiskFromScore score = Math.Exp(score) / (1. + Math.Exp(score))


    let intToMonth i = 
        [
            "jan"
            "feb"
            "mrt"
            "apr"
            "mei"
            "jun"
            "jul"
            "aug"
            "sep"
            "okt"
            "nov"
            "dec"
        ] |> List.item (i - 1)

    module DataOption  =
        
        open Types

        let create id label = { Id = id; Label = label }

        let toString (d : DataOption) = d.Label

        let optToString = function
        | Some d -> d |> toString
        | None   -> ""

        let EqsId id (d : DataOption) = d.Id = id

        let EqsIdOpt id = function
        | Some d -> d |> EqsId id
        | None   -> false


    module List =

        let countByList xs1 xs2 =
            xs2
            |> List.append xs1
            |> List.countBy id
            |> List.map (fun (k, v) -> k, v - 1)
            |> List.sortBy (fun (k, _) ->
                try
                    xs1 |> List.findIndex ((=) k)
                with
                | _ ->
                    xs1 |> String.concat ", "
                    |> sprintf "countByList couldn't find %s in %s" k
                    |> failwith
            )
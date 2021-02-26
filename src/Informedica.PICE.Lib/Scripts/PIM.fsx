
#I __SOURCE_DIRECTORY__

#r "System.Data.Linq"
#load "../../../.paket/load/net472/PICELib/picelib.group.fsx"

open System
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"/../."

#load "../Types.fs"
#load "../Utils.fs"
#load "../PIM.fs"

open Informedica.PICE.Lib
open Types

// basic PIM only the intercept applies
let pim = 
    {
        Urgency= PIM.NotElective
        Recovery= PIM.NoRecovery
        RiskDiagnosis= []
        Ventilated= false
        AdmissionPupils= PIM.UnknownPupils
        PaO2= None
        FiO2= None
        BaseExcess= None
        SystolicBloodPressure= None
        PIM2Score = None
        PIM2Mortality = None
        PIM3Score = None
        PIM3Mortality = None
    }


let printMort label (pim : PIM) = 
    pim.PIM3Mortality 
    |> Option.map (((*) 100.) >> (sprintf "%.1f %%"))
    |> Option.defaultValue ""
    |> printfn "%s: %s" label
    pim


// PIM3 score =
//  (3.8233 × pupillary reaction) +
//  (−0.5378 × elective admission) +
//  (0.9763 × mechanical ventilation) +
//  (0.0671 × [absolute {base excess}]) +
//  (−0.0431 × SBP) + (0.1716 × [SBP^2/1,000]) +
//  (0.4214 × [{FiO2 × 100}/PaO2]) +
//  (-1.2246 × bypass cardiac procedure) +
//  (-0.8762 × non-bypass cardiac procedure) +
//  (-1.5164 × noncardiac procedure) +
//  (1.6225 × very high-risk diagnosis) +
//  (1.0725 × high-risk diagnosis)
//  (-2.1766 × low-risk diagnosis) +
//  −1.7928


// test basic pim
// manual
let score = 
    -0.0431 * 120. + 0.1716 * ((120.**2.)/1000.) + 
    -1.7928
score 
|> PIM.calcRiskFromScore
|> fun x -> x * 100.
|> (sprintf "%.1f %%")
|> printfn "basic, manual: %s"
pim 
|> PIM.calculatePIM3
|> printMort "basic"
|> fun _ ->
// manual
    let score = 
        -0.5378 +
        -0.0431 * 120. + 0.1716 * ((120.**2.)/1000.) + 
        -1.7928
    score 
    |> PIM.calcRiskFromScore
    |> fun x -> x * 100.
    |> (sprintf "%.1f %%")
    |> printfn "elective, manual: %s"
    // test elective pim
    { pim 
        with Urgency = PIM.Elective
    }
    |> PIM.calculatePIM3
    |> printMort "elective"
|> fun _ ->
    // manual
    let score = 
        -0.0431 * 60. + 0.1716 * ((60.**2.)/1000.) + 
        -1.7928
    score 
    |> PIM.calcRiskFromScore
    |> fun x -> x * 100.
    |> (sprintf "%.1f %%")
    |> printfn "sbp = 60, manual: %s"
    // test sbp pim
    { pim with
        SystolicBloodPressure = Some 60.
    }
    |> PIM.calculatePIM3
    |> printMort "sbp = 60"
|> fun _ ->
    // manual
    let score = 
        -0.0431 * 120. + 0.1716 * ((120.**2.)/1000.) + 
        -1.2246 +
        -1.7928
    score 
    |> PIM.calcRiskFromScore
    |> fun x -> x * 100.
    |> (sprintf "%.1f %%")
    |> printfn "post bypass, manual: %s"
    // test sbp pim
    { pim with
        Recovery = PIM.PostCardiacByPass
    }
    |> PIM.calculatePIM3
    |> printMort "post bypass"
|> fun _ ->
    // manual
    let score = 
        -0.0431 * 120. + 0.1716 * ((120.**2.)/1000.) + 
        1.6225 +
        -1.7928
    score 
    |> PIM.calcRiskFromScore
    |> fun x -> x * 100.
    |> (sprintf "%.1f %%")
    |> printfn "very high risk, manual: %s"
    // test sbp pim
    { pim with
        RiskDiagnosis = [ PIM.pim3VeryHighRisk |> List.head ]
    }
    |> PIM.calculatePIM3
    |> printMort "very high risk"
|> fun _ ->
    // manual
    let score = 
        -0.0431 * 120. + 0.1716 * ((120.**2.)/1000.) + 
        0.4214 * ((0.6 * 100.)/ (8. * 7.50061683)) +
        -1.7928
    score 
    |> PIM.calcRiskFromScore
    |> fun x -> x * 100.
    |> (sprintf "%.1f %%")
    |> printfn "paf ratio 100, manual: %s"
    // test sbp pim
    { pim with
        PaO2 = Some 8.
        FiO2 = Some 0.6
    }
    |> PIM.calculatePIM3
    |> printMort "paf ratio 100"
|> fun _ ->
    // manual
    let score = 
        -0.0431 * 120. + 0.1716 * ((120.**2.)/1000.) + 
        0.9763 +
        -1.7928
    score 
    |> PIM.calcRiskFromScore
    |> fun x -> x * 100.
    |> (sprintf "%.1f %%")
    |> printfn "mechanical ventilation, manual: %s"
    // test sbp pim
    { pim with
        Ventilated = true
    }
    |> PIM.calculatePIM3
    |> printMort "mechanical ventilation"
|> ignore


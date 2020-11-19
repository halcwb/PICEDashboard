namespace Informedica.PICE.Lib


module Export =

    open System
    open Types

    let getPatientState (pa : PICUAdmission) (pat : Patient) =
        match pa.AdmissionDate, pa.DischargeDate with
        | Some dt1, Some dt2 ->
            match pat.DateOfDeath with
            | Some dt -> 
                if dt >= dt1 && dt <= dt2 then "Death"
                else
                    match pa.DischargeReason with
                    | None -> "Alive"
                    | Some dr ->
                        if dr.Id = "100" then "Death" else "Alive"
            | None -> "Alive"
        | _ -> "Alive"


    let optDateDiff fSome defVal (dt1 : DateTime option) (dt2 : DateTime option) =
        match dt1, dt2 with
        | Some d1, Some d2 -> (d1 - d2).TotalDays |> fSome
        | _, _             -> defVal

    let export (pats: Result<Patient[] * string[], _>) =
        let optToString = Option.map string >> Option.defaultValue ""
        let optDateDiff = optDateDiff string ""

        let headers =
            [
                "HospitalNumber"
                "AdmissionDate"
                "Age(days)"
                "RiskDiagnoses"
                "Urgency"
                "Recovery"
                "Ventilated"
                "AdmissionPupils"
                "SystolicBP"
                "BaseExcess"
                "FiO2"
                "PaO2"
                "PIM2Score"
                "PIM2Mort"
                "PIM3Score"
                "PIM3Mort"
                "AdmissionSource"
                "Cancer"
                "CPR24HourBefore"
                "CreatinineMax"
                "GlucoseMax"
                "HeartRateMax"
                "LowRiskPrimary"
                "MentalStatus"
                "PaO2Min"
                "PCO2Max"
                "PHMin"
                "PHMax"
                "PlateletsMin"
                "PotassiumMax"
                "PTMax "
                "PTTMax"
                "PupilsFixed"
                "SystolicBloodPressureMin"
                "TemperatureMin"
                "TemperatureMax"
                "BicarbonateMin"
                "BicarbonateMax"
                "UreaMax"
                "WhiteBloodCountMin"
                "PRISM3Neuro"
                "PRISM3Score"
                "PRISM4Mortality"
                "Status"
            ]
            |> List.singleton

        pats
        |> Result.valueOrDefault (fun _ -> [||])
        |> Array.toList
        |> List.collect (fun pat ->
            pat.HospitalAdmissions
            |> List.collect (fun ha ->
                ha.PICUAdmissions
                |> List.map (fun pa ->
                    let optPRISM p =
                        match p with
                        | None -> []
                        | Some prism ->
                            [
                                prism.AdmissionSource |> string
                                prism.Cancer |> string
                                prism.CPR24HourBefore |> string
                                prism.CreatinineMax |> optToString
                                prism.GlucoseMax |> optToString
                                prism.HeartRateMax |> Option.map float |> optToString
                                prism.LowRiskPrimary |> string
                                prism.MentalStatus |> Option.map float |> optToString
                                prism.PaO2Min |> optToString
                                prism.PCO2Max |> optToString
                                prism.PHMin |> optToString
                                prism.PHMax |> optToString
                                prism.PlateletsMin |> optToString
                                prism.PotassiumMax |> optToString
                                prism.PTMax  |> optToString
                                prism.PTTMax |> optToString
                                prism.PupilsFixed |> Option.map float |> optToString
                                prism.SystolicBloodPressureMin |> optToString
                                prism.TemperatureMin |> optToString
                                prism.TemperatureMax |> optToString
                                prism.BicarbonateMin |> optToString
                                prism.BicarbonateMax |> optToString
                                prism.UreaMax |> optToString
                                prism.WhiteBloodCountMin |> optToString
                                prism.PRISM3Neuro |> Option.map float |> optToString
                                prism.PRISM3Score |> Option.map float |> optToString
                                prism.PRISM4Mortality |> optToString
                            ]


                    [
                        pat.HospitalNumber
                        pa.AdmissionDate |> Option.map string |> Option.defaultValue ""
                        optDateDiff pa.AdmissionDate pat.BirthDate
                        pa.PIM.RiskDiagnosis |> List.map string |> String.concat ", "
                        pa.PIM.Urgency |> string
                        pa.PIM.Recovery |> string
                        pa.PIM.Ventilated |> string
                        pa.PIM.AdmissionPupils |> PIM.pupilsToString
                        pa.PIM.SystolicBloodPressure |> optToString
                        pa.PIM.BaseExcess |> optToString
                        pa.PIM.FiO2 |> optToString
                        pa.PIM.PaO2 |> optToString
                        pa.PIM.PIM2Score |> optToString
                        pa.PIM.PIM2Mortality |> optToString
                        pa.PIM.PIM3Score |> optToString
                        pa.PIM.PIM3Mortality |> optToString
                        match pa.PRISM4, pa.PRISM12, pa.PRISM24 with
                        | Some prism, _, _
                        | None, Some prism, _ 
                        | None, None, Some prism -> yield! (prism |> Some |> optPRISM)
                        | _ -> ()
                        getPatientState pa pat
                    ]
                )
            )
        )
        |> List.append headers


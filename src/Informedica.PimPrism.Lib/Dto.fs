namespace Informedica.PimPrism.Lib


module Dto =

    open System
    open Types
    open Utils

    module PIM =

        let diagnoses =
            [
                Asthma, "Asthma"
                BoneMarrowTransplant, "Bone Marrow Transplant"
                Bronchiolitis, "Bronchiolitis"
                CardiacArrestInHospital, "Cardiac Arrest InHospital"
                CardiacArrestPreHospital, "Cardiac Arrest PreHospital"
                CardiomyopathyOrMyocarditis, "Cardiomyopathy Or Myocarditis"
                CerebralHemorrhage, "Cerebral Hemorrhage"
                Croup, "Croup"
                DiabeticKetoacidosis, "Diabetic Ketoacidosis"
                HIVPositive, "HIV Positive"
                HypoplasticLeftHeartSyndrome, "Hypoplastic Left Heart Syndrome"
                LeukemiaorLymphoma, "Leukemia Or Lymphoma"
                LiverFailure, "Liver Failure"
                NecrotizingEnterocolitis, "Necrotizing Enterocolitis"
                NeurodegenerativeDisorder, "Neurodegenerative Disorder"
                ObstructiveSleepApnea, "Obstructive Sleep Apnea"
                SeizureDisorder, "Seizure Disorder"
                SevereCombinedImmuneDeficiency, "Severe Combined Immune Deficiency"
            ]

        let diagnosesFromString s =
            let eqs s1 s2 =
                let s1 = s1 |> String.toLower |> String.replace " " ""
                let s2 = s2 |> String.toLower |> String.replace " " ""
                s1 = s2

            diagnoses |> List.filter (snd >> (eqs s)) |> List.map fst


        type Dto() =
            member val Elective = false with get, set
            member val ElectiveUnknown = true with get, set
            member val Recovery = false with get, set
            member val ByPass = false with get, set
            member val Cardiac = false with get, set
            member val RiskDiagnosis: string list = [] with get, set
            member val Ventilated = false with get, set
            member val AdmissionPupils = false with get, set
            member val PaO2kP: float option = None with get, set
            member val PaO2mmHg: float option = None with get, set
            member val FiO2: float option = None with get, set
            member val BaseExcess: float option = None with get, set
            member val SystolicBloodPressure: float option = None with get, set
            member val Scores: (string * float) list = [] with get, set
            member val PIM2Score: float option = None with get, set
            member val PIM2Mortality: float option = None with get, set
            member val PIM3Score: float option = None with get, set
            member val PIM3Mortality: float option = None with get, set


        let fromDto (dto: Dto) =
            {
                Urgency =
                    match dto.Elective, dto.ElectiveUnknown with
                    | true, _ -> Elective
                    | false, true -> UnknownUrgency
                    | false, false -> NotElective
                Recovery =
                    match dto.Recovery, dto.ByPass, dto.Cardiac with
                    | _, true, _ -> PostCardiacByPass
                    | _, false, true -> PostCardiacNonByPass
                    | true, false, false -> PostNonCardiacProcedure
                    | false, false, _ -> NoRecovery
                RiskDiagnosis = dto.RiskDiagnosis |> List.collect diagnosesFromString
                Ventilated = dto.Ventilated
                AdmissionPupils =
                    if dto.AdmissionPupils = true then
                        FixedDilated
                    else
                        UnknownPupils
                PaO2 =
                    match dto.PaO2kP, dto.PaO2mmHg with
                    | Some _, _ -> dto.PaO2kP
                    | None, _ -> dto.PaO2mmHg |> Option.map calcmmHgToKiloPascal
                FiO2 = dto.FiO2
                BaseExcess = dto.BaseExcess
                SystolicBloodPressure = dto.SystolicBloodPressure
                PIM2Scores = []
                PIM2Score = None
                PIM2Mortality = None
                PIM3Scores = []
                PIM3Score = None
                PIM3Mortality = None
            }


    module PRISM =

        type Dto() =
            member val Age: DateTime option = None with get, set
            member val SystolicBloodPressureMin: float option = None with get, set
            member val TemperatureMin: float option = None with get, set
            member val TemperatureMax: float option = None with get, set
            member val MentalStatus: int option = None with get, set
            member val HeartRateMax: int option = None with get, set
            member val PupilsFixed: int option = None with get, set
            member val PHMin: float option = None with get, set
            member val PHMax: float option = None with get, set
            member val BicarbonateMin: float option = None with get, set
            member val BicarbonateMax: float option = None with get, set
            member val PCO2Max: float option = None with get, set
            member val PaO2Min: float option = None with get, set
            member val GlucoseMax: float option = None with get, set
            member val PotassiumMax: float option = None with get, set
            member val CreatinineMax: float option = None with get, set
            member val UreaMax: float option = None with get, set
            member val WhiteBloodCountMin10p9PerL: float option = None with get, set
            member val PTMax: float option = None with get, set
            member val PTTMax: float option = None with get, set
            member val PlateletsMin10p9PerL: float option = None with get, set
            member val AdmissionSource = "" with get, set
            member val CPR24HourBefore: bool = false with get, set
            member val Cancer: bool = false with get, set
            member val LowRiskPrimary: bool = false with get, set
            member val PRISM3ScoreList: (string * int) list = [] with get, set
            member val PRISM3Score: int option = None with get, set
            member val PRISM3Neuro: int option = None with get, set
            member val PRISM4ScoreList: (string * float) list = [] with get, set
            member val PRISM4Mortality: float option = None with get, set


        let fromDto (dto: Dto) =
            {
                Age = dto.Age
                SystolicBloodPressureMin = dto.SystolicBloodPressureMin
                TemperatureMin = dto.TemperatureMin
                TemperatureMax = dto.TemperatureMax
                MentalStatus = dto.MentalStatus
                HeartRateMax = dto.HeartRateMax
                PupilsFixed = dto.PupilsFixed
                PHMin = dto.PHMin
                PHMax = dto.PHMax
                BicarbonateMin = dto.BicarbonateMin
                BicarbonateMax = dto.BicarbonateMax
                PCO2Max = dto.PCO2Max
                PaO2Min = dto.PaO2Min
                GlucoseMax = dto.GlucoseMax
                PotassiumMax = dto.PotassiumMax
                CreatinineMax = dto.CreatinineMax
                UreaMax = dto.UreaMax
                WhiteBloodCountMin = dto.WhiteBloodCountMin10p9PerL |> Option.map (fun v -> v * 1000.)
                PTMax = dto.PTMax
                PTTMax = dto.PTTMax
                PlateletsMin = dto.PlateletsMin10p9PerL |> Option.map (fun v -> v * 1000.)
                AdmissionSource =
                    match dto.AdmissionSource with
                    | s when s = "recovery" -> Recovery
                    | s when s = "another hospital" -> AnotherHospital
                    | s when s = "in hospital" -> InHospital
                    | s when s = "emergency unit" -> EmergencyUnit
                    | _ -> UnknownAdmissionSource
                CPR24HourBefore = dto.CPR24HourBefore
                Cancer = dto.Cancer
                LowRiskPrimary = dto.LowRiskPrimary
                PRISM3ScoreList = []
                PRISM3Score = None
                PRISM3Neuro = None
                PRISM4ScoreList = []
                PRISM4Mortality = None
            }

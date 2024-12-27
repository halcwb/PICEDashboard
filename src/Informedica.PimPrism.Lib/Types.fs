namespace Informedica.PimPrism.Lib

module Types =

    open System

    [<AutoOpen>]
    module PRISM =

        type Item =
            | BloodPressure
            | Temperature
            | MentalStatus
            | HeartRate
            | Creatinine
            | Urea
            | ProthPT
            | ProthPTT
            | Pupils
            | Ph
            | TotalCO2
            | PCO2
            | PaO2
            | Glucose
            | Potassium
            | WBC
            | Platelets

        type AgePRISM3 =
            | Neonate
            | Infant
            | Child
            | Adolescent
            | AllMinNeonate
            | AnyAge

        type AgePRISM4 =
            | TwoWeeks
            | OneMonth
            | OneYear
            | EightTeen
            | UnknownAge

        type AdmissionSource =
            | Recovery
            | AnotherHospital
            | InHospital
            | EmergencyUnit
            | UnknownAdmissionSource


    type PRISM =
        {
            Age: DateTime option
            SystolicBloodPressureMin: float option
            TemperatureMin: float option
            TemperatureMax: float option
            MentalStatus: int option
            HeartRateMax: int option
            PupilsFixed: int option
            PHMin: float option
            PHMax: float option
            BicarbonateMin: float option
            BicarbonateMax: float option
            PCO2Max: float option
            PaO2Min: float option
            GlucoseMax: float option
            PotassiumMax: float option
            CreatinineMax: float option
            UreaMax: float option
            WhiteBloodCountMin: float option
            PTMax: float option
            PTTMax: float option
            PlateletsMin: float option
            AdmissionSource: AdmissionSource
            CPR24HourBefore: bool
            Cancer: bool
            LowRiskPrimary: bool
            PRISM3ScoreList: (string * int) list
            PRISM3Score: int option
            PRISM3Neuro: int option
            PRISM4ScoreList: (string * float) list
            PRISM4Mortality: float option
        }


    [<AutoOpen>]
    module PIM =

        type AdmissionUrgency =
            | Elective
            | NotElective
            | UnknownUrgency

        type PupilResponse =
            | FixedDilated
            | NormalPupils
            | UnknownPupils

        type RiskDiagnosis =
            | Asthma
            | BoneMarrowTransplant
            | Bronchiolitis
            | CardiacArrestInHospital
            | CardiacArrestPreHospital
            | CardiomyopathyOrMyocarditis
            | CerebralHemorrhage
            | Croup
            | DiabeticKetoacidosis
            | HIVPositive
            | HypoplasticLeftHeartSyndrome
            | LeukemiaorLymphoma
            | LiverFailure
            | NecrotizingEnterocolitis
            | NeurodegenerativeDisorder
            | ObstructiveSleepApnea
            | SeizureDisorder
            | SevereCombinedImmuneDeficiency

        type Recovery =
            | NoRecovery
            | PostCardiacByPass
            | PostCardiacNonByPass
            | PostNonCardiacProcedure


    type PIM =
        {
            Urgency: AdmissionUrgency
            Recovery: PIM.Recovery
            RiskDiagnosis: RiskDiagnosis list
            Ventilated: bool
            AdmissionPupils: PupilResponse
            PaO2: float option
            FiO2: float option
            BaseExcess: float option
            SystolicBloodPressure: float option
            PIM2Scores: (string * float) list
            PIM2Score: float option
            PIM2Mortality: float option
            PIM3Scores: (string * float) list
            PIM3Score: float option
            PIM3Mortality: float option
        }

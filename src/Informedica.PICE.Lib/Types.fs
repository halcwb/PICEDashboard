namespace Informedica.PICE.Lib

module Types =

    open System

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

        type AgePRISM3 = | Neonate | Infant | Child | Adolescent | AllMinNeonate | AnyAge

        type AgePRISM4 = | TwoWeeks | OneMonth | OneYear | EightTeen | UnknownAge

        type AdmissionSource =
            | Recovery
            | AnotherHospital
            | InHospital
            | EmergencyUnit
            | UnknownAdmissionSource

    type PRISM =
        {
            Age : DateTime option
            SystolicBloodPressureMin : float option
            TemperatureMin : float option
            TemperatureMax : float option
            MentalStatus : int option
            HeartRateMax : int option
            PupilsFixed : int option
            PHMin : float option
            PHMax : float option
            BicarbonateMin : float option
            BicarbonateMax : float option
            PCO2Max : float option
            PaO2Min : float option
            GlucoseMax : float option
            PotassiumMax : float option
            CreatinineMax : float option
            UreaMax : float option
            WhiteBloodCountMin : float option
            PTMax : float option
            PTTMax : float option
            PlateletsMin : float option
            AdmissionSource : PRISM.AdmissionSource
            CPR24HourBefore : bool
            Cancer : bool
            LowRiskPrimary : bool
            PRISM3Score : int option
            PRISM3Neuro : int option
            PRISM4Mortality : float option
        }

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
            | PostCariacNonByPass
            | PostNonCardiacProcedure

    type PIM =
        {
            Urgency: PIM.AdmissionUrgency
            Recovery: PIM.Recovery
            RiskDiagnosis: PIM.RiskDiagnosis list
            Ventilated: bool
            AdmissionPupils: PIM.PupilResponse
            PaO2: float option
            FiO2: float option
            BaseExcess: float option
            SystolicBloodPressure: float option
            PIM2Score : float option
            PIM2Mortality : float option
            PIM3Score : float option
            PIM3Mortality : float option
        }

    type Patient =
        {
            Id : string
            HospitalNumber : string
            BirthDate : DateTime option
            Gender : Gender
            BirthWeight : float option
            GestationalAge : int option
            PatientState : PatientState
            DateOfDeath : DateTime option
            DeathLocation : DataOption option
            DeathMode : DataOption option
            HospitalAdmissions : HospitalAdmission list
        }
    and PatientState = Alive | Dead | UnknownPatientState
    and HospitalAdmission =
        {
            Id : string
            HospitalNumber : string
            AdmissionDate : DateTime option
            DischargeDate : DateTime option
            DischargeDestination : DataOption option
            PICUAdmissions : PICUAdmission list
        }
    and PICUAdmission =
        {
            Id : string
            HospitalAdmissionId : string
            ClickId : string
            HospitalNumber : string
            AdmissionDate : DateTime option
            DischargeDate : DateTime option
            DischargeReason : DataOption option
            AdmissionType : AdmissionType
            AdmissionIndication : DataOption option
            ReferingSpecialism : DataOption option
            PrimaryDiagnosis : Diagnose list
            SecondaryDiagnosis : Diagnose list
            Diagnoses : Diagnose list
            AdmissionWeight : float option
            AdmissionLength : int option
            ContinuousReanimation : bool
            PIM : PIM
            PRISM24 : PRISM option
            PRISM12 : PRISM option
            PRISM4 : PRISM option
        }
    and AdmissionType =
        | Medical
        | Surgery
        | DOA
        | UnknownAdmissionType
    and Diagnose =
        {
            Id: string
            Group : string
            Name : string
        }
    and Gender = 
        | Male
        | Female
        | UnknownGender
    and DataOption = { Id : string; Label : string }


    type ParsingError =
        | ParseError of string
        | NoParseError

    type ValidationError =
        | NotValid of Patient * string
        | IsValid


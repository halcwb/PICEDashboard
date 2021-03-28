namespace Informedica.PICE.Shared

module Route =
    let hello = "/api/hello"


module Utils =

    module Math =

        open System

        let round (n : int) (c : float) = Math.Round(c, n)
    
/// Utility functions to apply memoization
module Memoization =

    open System.Collections.Generic
    
    /// Memoize a function `f` according
    /// to its parameter
    let memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some r -> r
            | None ->
                let r = f x
                cache := (!cache).Add(x, r)
                r

module Literals =

    [<Literal>]
    let sectionPICE = "PICE Rapport"
    [<Literal>]
    let groupValidation = "Validatie"
    [<Literal>]
    let groupOverview = "Overzicht"
    [<Literal>]
    let groupMortality = "Mortaliteit"
    [<Literal>]
    let groupAdmission = "Opname"
    [<Literal>]
    let groupPatient = "Patient"
    [<Literal>]
    let groupGender = "Geslacht"
    [<Literal>]
    let groupAge = "Leeftijd"
    [<Literal>]
    let groupDischarge = "Ontslag"
    [<Literal>]
    let groupDischargeReason = "Ontslagreden"
    [<Literal>]
    let groupDiagnose = "Diagnose"
    [<Literal>]
    let groupDiagnoseGroup = "Diagnose Groep"
    [<Literal>]
    let groupSpecialism = "Opname specialisme"
    [<Literal>]
    let groupVentilation = "Beademing"
    [<Literal>]
    let subGroupCanule = "Canule"
    [<Literal>]
    let paragraphTotals = "Totalen"
    [<Literal>]
    let paragraphPerYear = "Per jaar"
    [<Literal>]
    let paragraphPerMonth = "Per maand"
    [<Literal>]
    let paragraphPIMandPRISM = "PIM en PRISM"
    [<Literal>]
    let groupDeathMode = "Reden van Overlijden"
    [<Literal>]
    let groupSMR = "Standardized Mortality Ratio"
    [<Literal>]
    let paragraphSMRperYear = "SMR per jaar"
    [<Literal>]
    let paragraphSMRfunnel = "SMR funnel plot"
    [<Literal>]
    let paragraphPICUDays = "Ligdagen"
    [<Literal>]
    let paragraphAdmDisch = "Opnames/Ontslagen"
    [<Literal>]
    let paragraphUrgency = "Urgentie"
    [<Literal>]
    let paragraphOccupancy = "Bed bezetting"
    [<Literal>]
    let paragraphReadmission = "Heropname"
    [<Literal>]
    let paragraphLengthOfStay = "Opname duur"
    [<Literal>]
    let subGroupTransportHospital = "Transport door"
    [<Literal>]
    let subGroupTransportTeam = "Transport team"
    [<Literal>]
    let capYear = "Jaar"
    [<Literal>]
    let capPatient = "Patienten"
    [<Literal>]
    let capAdmission = "Opnames"
    [<Literal>]
    let capDischarge = "Ontslagen"
    [<Literal>]
    let capBedDays = "Ligdagen"
    [<Literal>]
    let capAllTimeMortality = "Totale Mortaliteit"
    [<Literal>]
    let capHospitalMortality = "Ziekenhuis Mortaliteit"
    [<Literal>]
    let capPICUMortality = "PICU Mortaliteit"
    [<Literal>]
    let capPIM2 = "PIM-2"
    [<Literal>]
    let capPIM3 = "PIM-3"
    [<Literal>]
    let capPRISM = "PRISM-IV"


module Types =

    open System

    type Totals =
        {
            Period : string
            InvalidPatients : (string * int) list
            Patients : int
            Admissions : int
            Admitted : int
            Discharged : int
            PICUDays : int
            Deaths : int
            PICUDeaths : int
            DeathMode : (string * int) list
            PIM2Mortality : float
            PIM3Mortality : float
            PRISM4Mortality : float
            Urgency : (string * int) list
            Gender : (string * int) list
            AgeGroup : (string * int) list
            DischargeReasons : (string * int) list
            HospitalDischargeDestinations : (string * int) list
            DiagnoseGroups : (string * int) list
            Diagnoses : (string * int) list
            Specialisme : (string * int) list
            Occupancy : (DateTime * int) list
            Cannule : (string * int) list
            TransportHospital : (string * int) list
            TransportTeam : (string * int) list
            Readmission : (string * int) list
            LengthOfStay : (string * int) list
        }


    type Report =
        {
            Sections : Section list
            Markdown : string
        }
    and Section = 
        {
            Title : string
            Chapters : Chapter list
            Totals : Totals
            YearTotals : Totals list
            // string = year to which totals belong
            MonthTotals : (string * (Totals list)) list
        }
    and Chapter = 
        {
            Title : string
            Chapters : Chapter list
            Paragraphs : Paragraph list
        }
    and Paragraph =
        {
            Title : string
            Content : string
        }

    type Filter =
        | NoFilter
        | AgeFilter of AgeGroup
        | DiagnoseFilter of DiagnoseGroup
    and DiagnoseGroup =
        | Cardiac
        | Oncology
        | OtherDiagnoses
    and AgeGroup =
        | Neonate
        | Infant
        | Toddler 
        | EarlyChildhood
        | MiddleChildhood
        | Adolescence

module Filter =

    open Types

    //"0 dagen - 4 weken"
    //"1 maand - 1 jaar"
    //"1 jaar - 4 jaar"
    //"4 jaar - 12 jaar"
    //"12 jaar - 16 jaar"
    //"16 jaar - 18 jaar"
    //"ouder dan 18 jaar"
    //"onbekende leeftijd"            
    let mapping =
        [
            NoFilter, ""
            AgeFilter Neonate, "0 dagen - 4 weken"
            AgeFilter Infant, "1 maand - 1 jaar"
            AgeFilter Toddler, "1 jaar - 4 jaar"
            AgeFilter EarlyChildhood, "4 jaar - 12 jaar"
            AgeFilter MiddleChildhood, "12 jaar - 18 jaar"
            AgeFilter Adolescence, "ouder dan 18 jaar"
            DiagnoseFilter Cardiac, "cardiaal"
            DiagnoseFilter Oncology, "oncologie"
            DiagnoseFilter OtherDiagnoses, "overige diagnoses"            
        ]


    let stringToFilter s =
        mapping
        |> List.tryFind (snd >> (=) s)


    let filterToString f =
        mapping
        |> List.tryFind (fst >> (=) f)

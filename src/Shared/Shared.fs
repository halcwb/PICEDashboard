namespace Informedica.PICE.Shared

module Route =
    let hello = "/api/hello"


module Utils =

    module Math =

        open System

        let round (n : int) (c : float) = Math.Round(c, n)
    

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
    let paragraphSMR = "Standardized Mortality Ratio"
    [<Literal>]
    let paragraphPICUDays = "Ligdagen"
    [<Literal>]
    let paragraphAdmDisch = "Opnames/Ontslagen"
    [<Literal>]
    let paragraphUrgency = "Urgentie"
    [<Literal>]
    let paragraphOccupancy = "Bed bezetting"
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
    let capMortality = "Mortaliteit"
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
            PIM2Mortality : float
            PIM3Mortality : float
            PRISM4Mortality : float
            Urgency : (string * int) list
            Gender : (string * int) list
            AgeGroup : (string * int) list
            DischargeReasons : (string * int) list
            HospitalDischargeDestinations : (string * int) list
            DiagnoseGroups : (string * int) list
            Occupancy : (DateTime * int) list
            Cannule : (string * int) list
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



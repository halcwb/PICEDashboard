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
        let groupGender = "Geslacht"
        [<Literal>]
        let groupAge = "Leeftijd"
        [<Literal>]
        let groupDischargeReason = "Ontslagreden"
        [<Literal>]
        let groupDiagnoseGroup = "Diagnose Groep"
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
            Paragraphs : Paragraph list
        }
    and Paragraph =
        {
            Title : string
            Content : string
        }



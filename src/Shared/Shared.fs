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
        let paragraphPerYear = "Per Jaar"
        [<Literal>]
        let paragraphPerMonth = "Per Maand"

module Types =

    //member val InvalidPatients : (string * int) list = [] with get, set
    //member val Patients = 0 with get, set
    //member val Admissions = 0 with get, set
    //member val Admitted = 0 with get, set
    //member val Discharged = 0 with get, set
    //member val PICUDays = 0 with get, set
    //member val Deaths = 0 with get, set
    //member val HospitalDeaths = 0 with get, set
    //member val PICUDeaths = 0 with get, set
    //member val PIM2Mortality = 0. with get, set
    //member val PIM3Mortality = 0. with get, set
    //member val PRISM4Mortality = 0. with get, set
    //member val Gender : (string * int) list = [] with get, set
    //member val AgeGroup : (string * int) list = [] with get, set
    //member val DischargeReasons : (string * int) list = [] with get, set
    //member val HospitalDischargeDestinations : (string * int) list = [] with get, set
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



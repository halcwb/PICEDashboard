namespace Informedica.PICE.Shared

module Route =
    let hello = "/api/hello"


module Types =

    type Totals =
        {
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
            DischargeReasons : (string * int) list
            HospitalDischargeReasons : (string * int) list
        }


    type MonthTotals =
        {
            Month : int
            Totals : Totals
        }


    type YearTotals =
        {
            Year : int
            Totals : Totals
            MonthTotals : MonthTotals list
        }


    type Statistics =
        {
            Totals : Totals 
            YearTotals : YearTotals list 
            InvalidPatients : (string * int) list 
            Html : string
        }


module Statistics =

    open Types

    let totals = 
        {
            Patients = 0
            Admissions = 0
            Admitted = 0
            Discharged = 0
            PICUDays = 0
            Deaths = 0
            PICUDeaths = 0
            PIM2Mortality = 0.
            PIM3Mortality = 0.
            PRISM4Mortality = 0.
            DischargeReasons = []
            HospitalDischargeReasons = []
        }

    let monthTotals = 
        {
            Month = 0
            Totals = totals
        }

    let yearTotals = 
        {
            Year = 0
            Totals = totals
            MonthTotals = []
        }

    let statistics =
        {
            Totals = totals
            YearTotals = []
            InvalidPatients = []
            Html = ""
        }
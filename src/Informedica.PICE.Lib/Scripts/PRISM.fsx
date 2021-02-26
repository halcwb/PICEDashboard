
#I __SOURCE_DIRECTORY__

#r "System.Data.Linq"
#load "../../../.paket/load/net472/PICELib/picelib.group.fsx"

open System
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"/../."

#load "../Types.fs"
#load "../Utils.fs"
#load "../PRISM.fs"

open Informedica.PICE.Lib
open Types

let prism =
    {
        Age = None
        SystolicBloodPressureMin = None
        TemperatureMin = None
        TemperatureMax = None
        MentalStatus = None
        HeartRateMax = None
        PupilsFixed = None
        PHMin = None
        PHMax = None
        BicarbonateMin = None
        BicarbonateMax = None
        PCO2Max = None
        PaO2Min = None
        GlucoseMax = None
        PotassiumMax = None
        CreatinineMax = None
        UreaMax = None
        WhiteBloodCountMin = None
        PTMax = None
        PTTMax = None
        PlateletsMin = None
        AdmissionSource = PRISM.InHospital
        CPR24HourBefore = false
        Cancer = false
        LowRiskPrimary = false
        PRISM3Score = None
        PRISM3Neuro = None
        PRISM4Mortality = None
    }


{
    prism with
        Age = DateTime(2009, 1, 20) |> Some
        SystolicBloodPressureMin = Some 88.
        TemperatureMin = Some 36.2
        TemperatureMax = Some 36.6
        MentalStatus = Some 3
        HeartRateMax = Some 104
        PHMin = Some 7.44
        PHMax = Some 7.45
        BicarbonateMin = Some 27.6
        BicarbonateMax = Some 31.3
        PCO2Max = Some 6.12
        PaO2Min = Some 11.7
        GlucoseMax = Some 11.2
        PotassiumMax = Some 5.8
}
|> PRISM.mapPRISMtoInput
|> PRISM.calculate (DateTime(2020, 7, 4))

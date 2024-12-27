#r "nuget: Expecto, 9.0.2"


#load "../Utils.fs"
#load "../Types.fs"
#load "../Pim.fs"
#load "../Prism.fs"
#load "../Dto.fs"


open System
open Expecto
open Expecto.Flip
open Expecto.Logging


open Informedica.PimPrism.Lib

open Dto

fsi.AddPrinter<DateTime> _.ToString("dd-MM-yyyy")

// checked with: https://www.cpccrn.org/calculators/prismiiicalculator/


let now = DateTime.Now
let newborn = now.AddDays(-1.) |> Some
let neonate = now.AddDays(-15.) |> Some
let infant = now.AddDays(-50.) |> Some
let child = now.AddDays(-3. * 365.) |> Some
let adolescent = now.AddYears(-13) |> Some


let createTests case neuro nonneuro mort (f: PRISM.Dto -> unit) =
    //    Console.WriteLine($"testing {case}")
    // Baseline newBorn
    // Note admission source unknown = recovery
    PRISM.Dto()
    |> fun dto ->
        dto |> f
        dto
    |> PRISM.fromDto
    |> PRISM.mapPRISMtoInput
    |> PRISM.calculate DateTime.Now
    |> fun prims ->
        testParam
            prims
            [
                $"{case} PRISM III Non-neuro", fun prism () -> prism.PRISM3Score |> Expect.equal "should equal" nonneuro
                $"{case} PRISM III Neuro", fun prism () -> prism.PRISM3Neuro |> Expect.equal "should equal" neuro
                $"{case} PRISM IV mort",
                fun prism () ->
                    prism.PRISM4Mortality
                    |> function
                        | Some v -> Math.Round(100. * v, 1) |> Some = mort || Math.Round(100. * v, 0) |> Some = mort
                        | None -> mort |> Option.isNone
                    |> Expect.isTrue "should equal"

            ]


let tests =
    testList
        "PRISM"
        [
            // Baseline newBorn
            // Note admission source unknown = recovery
            yield!
                fun (dto: PRISM.Dto) -> dto.Age <- newborn
                |> createTests "baseline newborn" (Some 0) (Some 0) (Some 1.1)
            // Neuro: 0
            // NonNeuro: 0
            // Mort 1.1%

            // Baseline neonate
            // Note admission source unknown = recovery
            yield!
                fun (dto: PRISM.Dto) -> dto.Age <- neonate
                |> createTests "baselne neonate" (Some 0) (Some 0) (Some 0.8)
            // Neuro: 0
            // NonNeuro: 0
            // Mort 0.8%

            // Baseline infant
            // Note admission source unknown = recovery
            yield!
                fun (dto: PRISM.Dto) -> dto.Age <- infant
                |> createTests "baseline infant" (Some 0) (Some 0) (Some 0.4)
            // Neuro: 0
            // NonNeuro: 0
            // Mort 0.4%

            // // Baseline child
            // // Note admission source unknown = recovery
            yield!
                fun (dto: PRISM.Dto) -> dto.Age <- child
                |> createTests "baseline child" (Some 0) (Some 0) (Some 0.3)
            // // Neuro: 0
            // // NonNeuro: 0
            // // Mort 0.3%


            // // Baseline adolescent
            // // Note admission source unknown = recovery
            yield!
                fun (dto: PRISM.Dto) -> dto.Age <- adolescent
                |> createTests "baseline adolescent" (Some 0) (Some 0) (Some 0.3)
            // // Neuro: 0
            // // NonNeuro: 0
            // // Mort 0.3%


            // // NewBorn
            // // pupil reaction one dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- newborn
                    dto.PupilsFixed <- Some 1
                |> createTests "newborn pupil" (Some 7) (Some 0) (Some 4.)
            // // Neuro: 7
            // // NonNeuro: 0
            // // Mort 4%


            // // Neonate
            // // pupil reaction one dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- neonate
                    dto.PupilsFixed <- Some 1
                |> createTests "neonate pupil" (Some 7) (Some 0) (Some 3.1)
            // // Neuro: 7
            // // NonNeuro: 0
            // // Mort 3.1%


            // // Infant
            // // pupil reaction one dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- infant
                    dto.PupilsFixed <- Some 1
                |> createTests "infant pupil" (Some 7) (Some 0) (Some 1.7)
            // // Neuro: 7
            // // NonNeuro: 0
            // // Mort 1.7%


            // // Child
            // // pupil reaction one dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- child
                    dto.PupilsFixed <- Some 1
                |> createTests "child pupil" (Some 7) (Some 0) (Some 1.2)
            // // Neuro: 7
            // // NonNeuro: 0
            // // Mort 1.2%


            // // NewBorn
            // // pupil reaction two dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- newborn
                    dto.PupilsFixed <- Some 2
                |> createTests "newborn 2 pupils" (Some 11) (Some 0) (Some 9.1)
            // // Neuro: 11
            // // NonNeuro: 0
            // // Mort 9.1%


            // // Neonate
            // // pupil reaction two dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- neonate
                    dto.PupilsFixed <- Some 2
                |> createTests "neonate 2 pupils" (Some 11) (Some 0) (Some 7.)
            // // Neuro: 11
            // // NonNeuro: 0
            // // Mort 7%


            // // Infant
            // // pupil reaction two dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- infant
                    dto.PupilsFixed <- Some 2
                |> createTests "infant 2 pupils" (Some 11) (Some 0) (Some 3.7)
            // // Neuro: 11
            // // NonNeuro: 0
            // // Mort 3.7%


            // // Adolescent
            // // pupil reaction two dilated
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- adolescent
                    dto.PupilsFixed <- Some 2
                |> createTests "adolescent 2 pupils" (Some 11) (Some 0) (Some 2.6)
            // // Neuro: 11
            // // NonNeuro: 0
            // // Mort 2.6%


            // // Newborn
            // // SBP = 30
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- newborn
                    dto.SystolicBloodPressureMin <- (Some 30.)
                |> createTests "newborn sbp=30" (Some 0) (Some 7) (Some 3.5)
            // // Neuro: 0
            // // NonNeuro: 7
            // // Mort 3.5%


            // // Neonate
            // // SBP = 30
            yield!
                fun (dto: PRISM.Dto) ->
                    dto.Age <- neonate
                    dto.SystolicBloodPressureMin <- (Some 30.)
                |> createTests "neonate sbp=30" (Some 0) (Some 7) (Some 2.5)
        // // Neuro: 0
        // // NonNeuro: 7
        // // Mort 2.5%
        ]


tests
|> runTestsWithCLIArgs [ CLIArguments.Verbosity LogLevel.Verbose; CLIArguments.Sequenced ] [||]

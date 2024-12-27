#r "nuget: Expecto, 9.0.2"

#load "../Utils.fs"
#load "../Types.fs"
#load "../Pim.fs"
#load "../Prism.fs"
#load "../Dto.fs"


open Informedica.PimPrism.Lib
open Dto

open System
open Expecto
open Expecto.Flip
open Expecto.Logging

// results checked with
// PIM2: https://qxmd.com/calculate/calculator_368/pim2
// PIM3: https://espnic-online.org/Education/Professional-Resources/Paediatric-Index-of-Mortality-3


let run (f: PIM.Dto -> unit) =
    PIM.Dto()
    |> fun dto ->
        dto |> f
        dto
    |> PIM.fromDto
    |> PIM.calculatePIM2
    |> PIM.calculatePIM3


let createTests case pim2 pim3 (f: PIM.Dto -> unit) =
    //    Console.WriteLine($"testing {case}")
    run f
    |> fun pim ->
        testParam
            pim
            [
                if pim2 |> Option.isSome then
                    $"{case} PIM2",
                    fun (pim: Types.PIM) () ->
                        pim.PIM2Mortality
                        |> Option.map (fun v -> Math.Round(100. * v, 1))
                        |> Expect.equal "should equal" pim2

                if pim3 |> Option.isSome then
                    $"{case} PIM3",
                    fun (pim: Types.PIM) () ->
                        pim.PIM3Mortality
                        |> Option.map (fun v -> Math.Round(100. * v, 1))
                        |> Expect.equal "should equal" pim3
            ]


let tests =
    testList
        "PIM"
        [
            // Base line
            // PIM2 mort = 0.8%
            // PIM3 mort = 1.2%
            yield! ignore |> createTests "base line" (Some 0.8) (Some 1.2)

            // Case pupils
            // PIM 2 factor = 3.0791
            // PIM 3 factor = 3,8233
            // PIM2 mort = 14.1%
            // PIM3 mort = 36%
            yield!
                fun (dto: PIM.Dto) -> dto.AdmissionPupils <- true
                |> createTests "pupils" (Some 14.1) (Some 36.)

            // Case Elective
            // PIM2 factor = -0,9282
            // PIM3 factor = -0,5378
            // PIM2 mort = 0.3%
            // PIM3 mort = 0.7%
            yield!
                fun (dto: PIM.Dto) -> dto.Elective <- true
                |> createTests "elective" (Some 0.3) (Some 0.7)

            yield!
                // Case Ventilated
                // PIM2 factor = 1,3352
                // PIM3 factor = 0,9763
                // PIM2 mort = 2.8%
                // PIM3 mort = 3.2%
                fun (dto: PIM.Dto) -> dto.Ventilated <- true
                |> createTests "ventilated" (Some 2.8) (Some 3.2)

            // Case BE = 1
            // PIM2 factor = 0,104
            // PIM3 factor = 0,0671
            // PIM2 mort = 0.8%
            // PIM3 mort = 1.3%
            yield!
                fun (dto: PIM.Dto) -> dto.BaseExcess <- Some 1.
                |> createTests "base excess" (Some 0.8) (Some 1.3)

            // Case SBP = 100
            // PIM2 factor = 0,279
            // PIM3 factor = -2.594
            // PIM2 mort = 1.0%
            // PIM3 mort = 1.4%
            yield!
                fun (dto: PIM.Dto) -> dto.SystolicBloodPressure <- Some 100.
                |> createTests "SBP" (Some 1.0) (Some 1.4)

            // Case fiO2/paO2 = 1
            // PIM2 factor = 0,2888
            // PIM3 factor = 0,4214
            // PIM2 mort = 1.0%
            // PIM3 mort = 1.7%
            yield!
                fun (dto: PIM.Dto) ->
                    dto.FiO2 <- Some 1.
                    dto.PaO2mmHg <- Some 100.
                |> createTests "fiO2/paO2" (Some 1.0) (Some 1.7)

            // Case cardiac bypass
            // PIM2 factor = -1.0244 + 0,7507
            // PIM3 factor = -1,2246
            // PIM2 mort = 0.6%
            // PIM3 mort = 0.4%
            yield!
                fun (dto: PIM.Dto) -> dto.ByPass <- true
                |> createTests "cardiac bypass" (Some 0.6) (Some 0.4)

            // Case cardiac non bypass
            // PIM3 factor = -0.8762
            // PIM3 mort = 0.5%
            yield!
                fun (dto: PIM.Dto) -> dto.Cardiac <- true
                |> createTests "cardiac non-bypass" (Some 0.3) (Some 0.5)

            // Case non cardiac recovery
            // PIM2 factor = -1,0244
            // PIM3 factor = -1,5164
            // PIM2 mort = 0.3%
            // PIM3 mort = 0.3%
            yield!
                fun (dto: PIM.Dto) -> dto.Recovery <- true
                |> createTests "non cardiac recovery" (Some 0.3) (Some 0.3)

            // Case low risk
            // PIM2 factor = -1,577
            // PIM3 factor = -2,1766
            // PIM2 mort = 0.2%
            // PIM3 mort = 0.1%
            yield!
                fun (dto: PIM.Dto) -> dto.RiskDiagnosis <- [ "asthma" ]
                |> createTests "low risk" (Some 0.2) (Some 0.1)

            // Case high risk
            // PIM2 factor = 1,6829
            // PIM3 factor = 1,0725
            // PIM2 mort = 3.9%
            // PIM3 mort = 3.5%
            yield!
                fun (dto: PIM.Dto) -> dto.RiskDiagnosis <- [ "hypoplastic left heart syndrome" ]
                |> createTests "high risk" (Some 3.9) (Some 3.5)

            // Case high and low risk
            // PIM2 factor = 1,6829 high
            // PIM2 factor = -1,577 low
            // PIM3 factor = 1,0725
            // PIM2 mort = 0.8%
            // PIM3 mort = 3.5%
            yield!
                fun (dto: PIM.Dto) -> dto.RiskDiagnosis <- [ "hypoplastic left heart syndrome"; "asthma" ]
                |> createTests "low and high risk" (Some 0.8) (Some 3.5)

            // Case very high risk (only PIM3)
            // PIM2 factor = 1,6829
            // PIM3 factor = 1,6225
            // PIM2 mort = 3.9%
            // PIM3 mort = 5.9%
            yield!
                fun (dto: PIM.Dto) -> dto.RiskDiagnosis <- [ "Leukemia Or Lymphoma" ]
                |> createTests "very high risk" (Some 3.9) (Some 5.9)

            // Case	Leukemia or lymphoma after first induction	None of the above	None of the above	100	NB	NB	NB	0	0
            yield!
                fun (dto: PIM.Dto) ->
                    dto.RiskDiagnosis <- [ "Leukemia Or Lymphoma" ]
                    dto.SystolicBloodPressure <- Some 100.
                |> createTests "very high risk SBP 100" None (Some 6.5)

            // Case Bone marrow transplant recipient	None of the above	None of the above	129	106	21	-7	0	0
            yield!
                fun (dto: PIM.Dto) ->
                    dto.RiskDiagnosis <- [ "Bone Marrow Transplant" ]
                    dto.SystolicBloodPressure <- Some 129.
                    dto.PaO2mmHg <- Some 106.
                    dto.FiO2 <- Some 0.21
                    dto.BaseExcess <- Some -7.
                |> createTests "very high risk SBP=129 PaO2=106 FiO2=.21 BE=-7" None (Some 8.9)

            yield!
                fun (dto: PIM.Dto) ->
                    dto.RiskDiagnosis <- [ "Bone Marrow Transplant" ]
                    dto.SystolicBloodPressure <- Some 104.
                    dto.PaO2mmHg <- Some 80.
                    dto.FiO2 <- Some 0.39
                    dto.BaseExcess <- Some 6.
                    dto.Ventilated <- true
                |> createTests "very high risk ventilated SBP=104 PaO2=80 FiO2=.39 BE=6" None (Some 22.9)

            // Case surgical	Leukemia or lymphoma after first induction	None of the above	None of the above	117	68	NB	-3	0	0
            yield!
                fun (dto: PIM.Dto) ->
                    dto.Recovery <- true
                    dto.RiskDiagnosis <- [ "Leukemia Or Lymphoma" ]
                    dto.SystolicBloodPressure <- Some 117.
                    dto.PaO2mmHg <- Some 68.
                    dto.BaseExcess <- Some -3.
                |> createTests "recovery, very high risk SBP=117 PaO2=68 BE=-3" None (Some 1.7)


        ]


tests
|> runTestsWithCLIArgs [ CLIArguments.Verbosity LogLevel.Verbose; Sequenced ] [||]


fun (dto: PIM.Dto) ->
    dto.RiskDiagnosis <- [ "Bone Marrow Transplant" ]
    dto.SystolicBloodPressure <- Some 104.
    dto.PaO2mmHg <- Some 80.
    dto.FiO2 <- Some 0.39
    dto.BaseExcess <- Some 6.
    dto.Ventilated <- true
|> run

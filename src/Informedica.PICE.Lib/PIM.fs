namespace Informedica.PICE.Lib

module PIM =

    open System
    open Types
    open PIM

    let kiloPascalToMmHg = Utils.kiloPascalToMmHg
    let calcRiskFromScore = Utils.calcRiskFromScore

    let pupilsToString = function
        | PIM.FixedDilated  -> "1"
        | PIM.NormalPupils  -> "0"
        | PIM.UnknownPupils -> ""


    // PIM2
    //High Risk Diagnoses include:
    //Cardiac Arrest
    //Cardiomyopathy or Myocarditis
    //Severe Combined Immunodeficiency (SCID)
    //Hypoplastic Left Heart Syndrome (HLHS)
    //Leukemia or Lymphoma after first induction of chemotherapy
    //Liver Failure as primary ICU admission reason
    //HIV Positive
    //Spontaneous Cerebral Hemorrhage (from any cause, except intracranial bleeding related to head trauma)
    //Neurodegenerative Disorder
    let pim2HighRisk =
        [ CardiacArrestPreHospital
          CardiacArrestInHospital
          CardiomyopathyOrMyocarditis
          SevereCombinedImmuneDeficiency
          HypoplasticLeftHeartSyndrome
          LeukemiaorLymphoma
          LiverFailure
          HIVPositive
          CerebralHemorrhage
          NeurodegenerativeDisorder ]
    //Low Risk Diagnoses include:
    //Asthma
    //Bronchiolitis
    //Croup
    //Obstructive Sleep Apnea (OSA)
    //Diabetic Ketoacidosis (DKA)
    let pim2LowRisk =
        [ Asthma
          Bronchiolitis
          Croup
          ObstructiveSleepApnea
          DiabeticKetoacidosis ]
    // PIM3
    // Low-risk diagnosis:
    //  asthma,
    //  bronchiolitis,
    //  croup,
    //  obstructive sleep apnea,
    //  diabetic ketoacidosis,
    //  seizure disorder.
    let pim3LowRisk =
        [ Asthma
          Bronchiolitis
          Croup
          ObstructiveSleepApnea
          DiabeticKetoacidosis
          SeizureDisorder ]
    // High-risk diagnosis:
    //  spontaneous cerebral hemorrhage,
    //  cardiomyopathy or myocarditis,
    //  hypoplastic left heart syndrome,
    //  neurodegenerative disorder,
    //  necrotizing enterocolitis.
    let pim3HighRisk =
        [ CerebralHemorrhage
          CardiomyopathyOrMyocarditis
          HypoplasticLeftHeartSyndrome
          NeurodegenerativeDisorder
          NecrotizingEnterocolitis ]
    // Very high-risk diagnosis:
    //  cardiac arrest preceding ICU admission,
    //  severe combined immune deficiency,
    //  leukemia or lymphoma after first induction,
    //  bone marrow transplant recipient,
    //  liver failure.
    let pim3VeryHighRisk =
        [ CardiacArrestInHospital
          CardiacArrestPreHospital
          SevereCombinedImmuneDeficiency
          LeukemiaorLymphoma
          BoneMarrowTransplant
          LiverFailure ]

    // PIM2 score =
    //    -0.9282(Elective) +
    //    -1.0244(Recovery) +
    //    0.7507(Bypass) +
    //    1.6829(High-Risk) +
    //    -1.577(Low-Risk) +
    //    3.0791(Pupils) +
    //    1.3352(Ventilator) +
    //    0.01395(absolute value of SBP-120) +
    //    0.1040(absolute value of base excess) +
    //    0.2888(100 x FiO2 / PaO2) +
    //    -4.8841
    let calculatePIM2 (pim: PIM) =
        let mapHighRisk rd =
            pim2HighRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then 1.6829 |> Some else None

        let mapLowRisk rd =
            pim2LowRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then -1.577 |> Some else None

        let paO2 =
            pim.PaO2
            |> Option.defaultValue 0.
            |> kiloPascalToMmHg

        let recovScore = function
            | PIM.NoRecovery        -> 0.
            | PIM.PostCardiacByPass -> 0.7507
            | _                     -> -1.0244

        let score =
            [
                "elective",
                if pim.Urgency = Elective then -0.9282 else 0.
                "recovery",
                pim.Recovery |> recovScore

                "risk diagnosis",
                [
                    pim.RiskDiagnosis |> mapHighRisk
                    // lowRisc
                    pim.RiskDiagnosis |> mapLowRisk
                ]
                |> List.filter Option.isSome
                |> List.map Option.get
                |> function
                | [] -> 0.
                | xs -> xs |> List.max

                "pupils",
                pim.AdmissionPupils
                |> function
                | FixedDilated -> 3.0791
                | _ -> 0.
                "ventilator",
                if pim.Ventilated then 1.3352 else 0.
                "SBP",
                (((pim.SystolicBloodPressure
                 |> Option.defaultValue 120.)
                - 120.)
                |> Math.Abs)
                * 0.01395
                "base excess",
                (pim.BaseExcess
                |> Option.defaultValue 0.
                |> Math.Abs)
                * 0.1040
                "fiO2",
                if paO2 > 0. then
                  (((pim.FiO2 |> Option.defaultValue 0.) * 100.)
                   / paO2)
                  * 0.2888
                else
                  0.
                "baseline",
                -4.8841
            ]
            |> List.mapi (fun i (l, v) ->
                //printfn "%i. %s: %f" (i + 1) l v
                v)
            |> List.reduce (+)

        let mort =
            calcRiskFromScore score

        { pim with
            PIM2Score = Some score
            PIM2Mortality = Some mort
        }


    // PIM3 score =
    //  (3.8233 × pupillary reaction) +
    //  (−0.5378 × elective admission) +
    //  (0.9763 × mechanical ventilation) +
    //  (0.0671 × [absolute {base excess}]) +
    //  (−0.0431 × SBP) + (0.1716 × [SBP^2/1,000]) +
    //  (0.4214 × [{FiO2 × 100}/PaO2]) +
    //  (-1.2246 × bypass cardiac procedure) +
    //  (-0.8762 × non-bypass cardiac procedure) +
    //  (-1.5164 × noncardiac procedure) +
    //  (1.6225 × very high-risk diagnosis) +
    //  (1.0725 × high-risk diagnosis)
    //  (-2.1766 × low-risk diagnosis) +
    //  −1.7928
    let calculatePIM3 (pim: PIM) =
        let mapVeryHighRisk rd =
            pim3VeryHighRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then 1.6225 |> Some else None

        let mapHighRisk rd =
            pim3HighRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then 1.0725 |> Some else None

        let mapLowRisk rd =
            pim3LowRisk
            |> List.exists (fun d -> rd |> List.exists ((=) d))
            |> fun b -> if b then -2.1766 |> Some else None

        let paO2 =
            pim.PaO2
            |> Option.defaultValue 0.
            |> kiloPascalToMmHg

        let sbp =
            pim.SystolicBloodPressure
            |> Option.defaultValue 120.

        let recovScore = function
            | PIM.NoRecovery              -> 0.
            | PIM.PostCardiacByPass       -> -1.2246
            | PIM.PostCardiacNonByPass    -> -0.8762
            | PIM.PostNonCardiacProcedure -> -1.5164

        let score =
            [
                "urgency"
                , if pim.Urgency = Elective then -0.5378 else 0.

                "recovery"
                , pim.Recovery |> recovScore

                "risk diagnosis",
                [
                    pim.RiskDiagnosis |> mapVeryHighRisk
                    // highRisc
                    pim.RiskDiagnosis |> mapHighRisk
                    // lowRisc
                    pim.RiskDiagnosis |> mapLowRisk
                ]
                |> List.filter Option.isSome
                |> List.map Option.get
                |> function
                | [] -> 0.
                | xs -> xs |> List.max

                "pupils",
                pim.AdmissionPupils
                |> function
                | FixedDilated -> 3.8233
                | _ -> 0.
                "vent"
                , if pim.Ventilated then 0.9763 else 0.
                "SBP"
                , (-0.0431 * sbp) + (0.1716 * ((sbp ** 2.) / 1000.))
                "base excess",
                (pim.BaseExcess
                |> Option.defaultValue 0.
                |> Math.Abs)
                * 0.0671
                "fiO2",
                if paO2 > 0. then
                  (((pim.FiO2 |> Option.defaultValue 0.) * 100.)
                   / paO2)
                  * 0.4214
                else
                  0.
                "baseline",
                -1.7928
            ]
            |> List.mapi (fun i (l, v) ->
//                    printfn "%i. %s: %f" (i + 1) l v
                v)
            |> List.reduce (+)

        { pim with
            PIM3Score = Some score
            PIM3Mortality = score |> calcRiskFromScore |> Some
        }


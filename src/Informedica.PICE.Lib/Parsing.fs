namespace Informedica.PICE.Lib



module Parsing =

    open System
    open System.Diagnostics
    open System.Globalization

    open Types
    open MRDM
    open Patient

    open Result.Operators

    let isNullOrWhiteSpace (s : String) = s |> String.IsNullOrWhiteSpace
    let notNullOrWhiteSpace = isNullOrWhiteSpace >> not


    module Parsers =

        let parseInt s =
            match Int32.TryParse(s) with
            | true,  x -> Some x
            | false, _ -> None


        let parseFloat s =
            match Double.TryParse(s,NumberStyles.Any,  CultureInfo.InvariantCulture) with
            | true, x  -> Some x
            | false, _ -> None


        let parseDate s =
            try
                DateTime.Parse(s, DateTimeFormatInfo.InvariantInfo)
            with
            | _ ->
                sprintf "could not parse date %s" s
                |> failwith  


        let parseDateOpt (s : string) =
            if s |> String.IsNullOrWhiteSpace then None
            else
                s
                |> parseDate
                |> Some


        let mapRiscDiagnosis d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs =
            match d with
            | s when s = "0" -> []
            | s when s = "1" -> [ PIM.Croup ]
            | s when s = "2" -> [ PIM.ObstructiveSleepApnea ]
            | s when s = "3" -> [ PIM.Bronchiolitis ]
            | s when s = "4" -> [ PIM.Asthma ]
            | s when s = "5" -> [ PIM.LiverFailure ]
            | s when s = "6" -> [ PIM.SeizureDisorder ]
            | s when s = "7" -> [ PIM.DiabeticKetoacidosis ]
            | s when s = "8" -> [ PIM.LiverFailure ]
            | s when s = "9" -> [ PIM.NecrotizingEnterocolitis ]
            | s when s = "10" -> [ PIM.DiabeticKetoacidosis ]
            | s when s = "11" -> [ PIM.CardiomyopathyOrMyocarditis ]
            | _ -> []
            |> List.append [
                if leukemia = "1" then PIM.LeukemiaorLymphoma
                if bmt = "1"      then PIM.BoneMarrowTransplant
                if cva = "1"      then PIM.CerebralHemorrhage
                if card = "1"     then PIM.CardiomyopathyOrMyocarditis
                if scid = "1"     then PIM.SevereCombinedImmuneDeficiency
                if hiv = "1"      then PIM.HIVPositive
                if neuro = "1"    then PIM.NeurodegenerativeDisorder
                if hlhs = "1"     then PIM.HypoplasticLeftHeartSyndrome
                if cprpre = "1"   then PIM.CardiacArrestPreHospital
                if cprin = "1"    then PIM.CardiacArrestInHospital
            ]


        let mapUrgency = function
            | s when s = "10" -> PIM.NotElective
            | s when s = "11" -> PIM.Elective
            | _ -> PIM.UnknownUrgency


        let mapPupils = function
            | s when s = "1" -> PIM.FixedDilated
            | s when s = "0" -> PIM.NormalPupils
            | _ -> PIM.UnknownPupils


        let mapPatientState = function
            | s when s = "0" -> Alive
            | s when s = "1" -> Dead
            | _ -> UnknownPatientState


        let mapAdmissionType = function
            | s when s = "1" -> Medical
            | s when s = "2" -> Surgery
            | s when s = "3" -> DOA
            | _ -> UnknownAdmissionType


        let mapGender = function
            | s when s = "1" -> Male
            | s when s = "2" -> Female
            | _ -> UnknownGender


        //129	Directe opname van buiten eigen UMC
        //103	Volwassen-IC /CCU
        //114	Zorgafdeling via OK
        //106	(Zorg)Afdeling (zonder extra bewaking)
        //115	SEH via OK
        //107	SEH
        //109	Recovery
        //105	Afdeling met extra bewaking (HDU/HighCare)
        //110	Kraamafdeling
        //77	Overig
        //108	OK
        //104	Longstay-ic
        //102	NICU (IC-Neonatologie)
        //99	Onbekend
        let mapAdmissionSource = function
            | s when s = "109"  -> PRISM.Recovery
            | s when s = "129"  -> PRISM.AnotherHospital
            | s when s = "115" || s = "107" -> PRISM.EmergencyUnit
            | s when s = "99"  || s = "77"  -> PRISM.UnknownAdmissionSource
            | _ -> PRISM.InHospital


        let mapLowRiskPRISM s =
            [
                "7"
                "10"
                "11"
            ]
            |> List.exists ((=) s)


        let parseDiagnose n id =
            id
            |> MRDM.Codes.find n
            |> function
            | None -> []
            | Some s ->
                s.Label
                |> String.replace "(" ""
                |> String.replace ")" "|"
                |> String.split "|"
                |> function
                | [g;d] -> [ { Id = id; Group = g |> String.trim |> String.toLower; Name = d |> String.trim |> String.toLower} ]
                | _     -> []


    let findOk n d = 
        MRDM.Codes.find n d
        |> function
        | Some s -> s |> Some  
        | None   -> None       
        |> Result.ok


    let parseDateOpt s =
        if s |> isNullOrWhiteSpace then None |> Result.ok
        else s |> Result.tryWithOk Parsers.parseDate

    let parseBool = ((=) "1") >> Result.ok


    let parseFloat s =
        if String.IsNullOrWhiteSpace(s) then Result.ok None
        else Result.okIfNone [| sprintf "couldn't parse float %s" s |] (Parsers.parseFloat s)


    let parseInt s =
        if String.IsNullOrWhiteSpace(s) then Result.ok None
        else Result.okIfNone [| sprintf "couldn't parse int %s" s |] (Parsers.parseInt s)


    let parsePatient (hospData : MRDMHospital.Row[]) (d : MRDMPatient.Row) =
        let getHospNum (data : MRDMHospital.Row[]) =
            let errs, hn =
                data
                |> Array.filter (fun hd -> hd.idcode = d.idcode)
                |> Array.map (fun hd -> hd.``ziekenhuis-episode-upn``)
                |> Array.distinct
                |> function
                | [||]   ->
                    [| sprintf "no hospitalnumber for: %A" d |],
                    ""
                | [|hn|] -> [||], hn
                | xs ->
                    let msg =
                        xs
                        |> Array.map (sprintf "%s")
                        |> Array.append [| sprintf "multiple hospitalnumbers for %s:" d.idcode |]
                        |> String.concat "\n"

                    [| msg |] , ""
            if errs |> Array.isEmpty then hn |> Result.ok
            else errs |> Result.error

        let mapPatientState = Parsers.mapPatientState >> Result.ok

        Patient.create
        <!> Result.ok d.idcode 
        <*> getHospNum hospData
        <*> parseDateOpt d.gebdat
        <*> (Parsers.mapGender >> Result.ok) d.geslacht
        <*> parseFloat d.``pat-weight-of-birth``
        <*> parseInt d.``pat-zwduur``
        <*> mapPatientState d.status
        <*> parseDateOpt d.datovl
        <*> findOk "adm-deathmodeid" d.``adm-deathmodeid``
        <*> findOk "adm-deceasedwhereid" d.``adm-deceasedwhereid``


    let parseHospAdm (hospData: MRDMHospital.Row[]) =
        let fErr msgs = msgs |> Result.Error
        let fOk ((p : Patient), msgs1) =
            hospData
            |> Array.filter (fun d -> d.``ziekenhuis-episode-upn`` = p.HospitalNumber)
            |> Array.map (fun d ->
                Patient.createHospitalAdmission
                <!> Result.ok d.``ziekenhuis-episode-upn``
                <*> parseDateOpt d.``adm-hosp-admdate``
                <*> (findOk "adm-desthospunitid" d.``adm-desthospunitid``)
                <*> parseDateOpt d.``adm-hosp-disdate`` 
            )
            |> Result.foldOk 
            |> function
            | Result.Ok (adms, msgs2) ->
                msgs1 |> Array.append msgs2 |> Result.okMsg ({ p with HospitalAdmissions = adms |> Array.toList })
            | Result.Error msgs2  -> msgs1 |> Array.append msgs2 |> Result.error 

        Result.either fOk fErr


    let addPICUAdmissions (admissions : Result<PICUAdmission[] * string[], _>)
                          (diagnoses : {| hn : string; ad: DateTime option; dd : DateTime option; dn : string |}[]) =
        let inPeriod dt1 dt2 dt3 dt4 =
            match dt1, dt2, dt3, dt4 with
            | Some d1, Some d2, Some d3, Some d4 -> d1 <= d3 && d2 >= d4
            | Some d1, _      , Some d3, None
            | Some d1, None   , Some d3, _  -> d1 <= d3
            | _ -> false

        let calcPRISM bdt adt prism =
            match prism with
            | None -> None
            | Some prism ->
                {
                    prism with
                        Age = bdt
                }
                |> fun prism ->
                    match adt with
                    | Some dt ->
                        prism
                        |> PRISM.mapPRISMtoInput
                        |> PRISM.calculate dt
                        |> PRISM.mapInputToPRISM prism
                        |> Some
                    | None    -> prism |> Some

        let fErr msgs = msgs |> Result.Error
        let fOk ((p : Patient), msgs1) =
            match admissions with
            | Result.Ok (xs , msgs2) ->
                let p =
                    {
                        p with
                            HospitalAdmissions =
                                p.HospitalAdmissions
                                |> List.map (fun ha ->
                                    { ha with
                                        PICUAdmissions =
                                            xs
                                            |> Array.filter (fun pa ->
                                                
                                                pa.HospitalNumber = ha.HospitalNumber &&
                                                inPeriod ha.AdmissionDate ha.DischargeDate pa.AdmissionDate pa.DischargeDate  
                                            )
                                            |> Array.map (fun pa ->
                                                let diagnoses =
                                                    diagnoses
                                                    |> Array.filter (fun d ->
                                                        d.hn = pa.HospitalNumber &&
                                                        d.ad = pa.AdmissionDate &&
                                                        d.dd = pa.DischargeDate
                                                    )
                                                { pa with
                                                    PRISM24 =
                                                        pa.PRISM24
                                                        |> calcPRISM p.BirthDate pa.AdmissionDate
                                                    PRISM12 =
                                                        pa.PRISM12
                                                        |> calcPRISM p.BirthDate pa.AdmissionDate
                                                    PRISM4 =
                                                        pa.PRISM4
                                                        |> calcPRISM p.BirthDate pa.AdmissionDate
                                                    Diagnoses =
                                                        diagnoses
                                                        |> Array.toList
                                                        |> List.collect (fun d ->
                                                            d.dn
                                                            |> Parsers.parseDiagnose "bijkomende-diagnose" 
                                                        )
                                                }
                                            )
                                            |> Array.toList
                                    }
                                )
                    }

                Result.okMsg p (msgs1 |> Array.append msgs2)
            | Result.Error msgs2 -> Result.okMsg p (msgs1 |> Array.append msgs2)

        Result.either fOk fErr

    let validateWithClickData (clickData : Click.PIMPRISMHist.Row []) (patResult: Result<Patient * string[], _>) =
        let cmpValue n oc1 oc2 pv =
            match pv with
            | PRISM.OneValue p1 ->
                match oc1 with
                | Some c1 when c1 <> 0. ->
                    if c1 = p1 then "", pv
                    else
                        sprintf "%s click value: %A <> MRDM value: %A" n oc1 p1, pv
                | _ -> "", pv
            | PRISM.TwoValues (p1, p2) ->
                match oc1, oc2 with
                | Some c1, Some c2 when c1 <> 0. && c2 <> 0. ->
                    if c1 = p1 && c2 = p2 then "", pv
                    else
                        sprintf "%s click values: %A, %A <> MRDM value: %A" n oc1 oc2 pv, pv
                | _, _ -> "", pv
            | _ ->
                match oc1, oc2 with
                | Some c1, None  ->
                    let pv = PRISM.OneValue c1
                    sprintf "", pv
                | Some c1, Some c2 ->
                    let pv = PRISM.TwoValues (c1, c2)
                    sprintf "", pv
                | _ -> "", pv

        let cmpPRISM (prism: PRISM option) (d : Click.PIMPRISMHist.Row) =
            match prism with
            | Some prism -> prism
            | None       -> PRISM.prism
            |> fun prism ->
                let input = prism |> PRISM.mapPRISMtoInput
                [
                    let oc1 =
                        d.sbp_min
                        |> Parsers.parseFloat
                        |> Option.map ((*) 7.5)
                    input.SystolicBloodPressure |> cmpValue "bloodpressure" oc1 None

                    let oc1 = d.T_min |> Parsers.parseFloat
                    let oc2 = d.T_max |> Parsers.parseFloat
                    input.Temperature |> cmpValue "temperature" oc1 oc2

                    let oc1 = d.emv |> Parsers.parseFloat
                    input.MentalStatus |> cmpValue "emv score" oc1 None

                    let oc1 = d.hr_max |> Parsers.parseFloat
                    input.HeartRate |> cmpValue "heart rate" oc1 None

                    let oc1 = d.pupreaction3 |> Parsers.parseFloat
                    input.PupilsFixed |> cmpValue "number of pupils" oc1 None

                    let oc1 = d.pH_min |> Parsers.parseFloat
                    let oc2 = d.pH_max|> Parsers.parseFloat
                    input.PH |> cmpValue "ph" oc1 oc2

                    let oc1 = d.bicarbonate_min |> Parsers.parseFloat
                    let oc2 = d.bicarbonate_max |> Parsers.parseFloat
                    input.TotalCO2 |> cmpValue "bicarbonate" oc1 oc2

                    let oc1 = d.paco2_max |> Parsers.parseFloat
                    input.PCO2 |> cmpValue "pCO2" oc1 None

                    let oc1 = d.pao2_min |> Parsers.parseFloat
                    input.PaO2 |> cmpValue "paO2" oc1 None

                    let oc1 = d.glucose_max |> Parsers.parseFloat
                    input.Glucose |> cmpValue "glucose" oc1 None

                    let oc1 = d.kalium_max |> Parsers.parseFloat
                    input.Potassium |> cmpValue "potassium" oc1 None

                    let oc1 = d.creatinine_max |> Parsers.parseFloat
                    input.Creatinine |> cmpValue "creatinine" oc1 None

                    let oc1 = d.ureum_max |> Parsers.parseFloat
                    input.Urea |> cmpValue "urea" oc1 None

                    let oc1 = d.leuco_min |> Parsers.parseFloat
                    input.WhiteBloodCount |> cmpValue "white blood cells" oc1 None

                    let oc1 = d.PTT_max |> Parsers.parseFloat
                    input.PT |> cmpValue "PT" oc1 None

                    let oc1 = d.PTT_max |> Parsers.parseFloat
                    input.PTT |> cmpValue "aPTT" oc1 None

                    let oc1 = d.thrombo_min |> Parsers.parseFloat
                    input.Platelets |> cmpValue "platelets" oc1 None
                ]
                |> List.fold (fun (i, (input : PRISM.Input), msgs) (msg, v) ->
                    match i with
                    | _ when i = 0  -> i + 1, {  input with SystolicBloodPressure = v }, (msg::msgs)
                    | _ when i = 1  -> i + 1, {  input with Temperature = v }, (msg::msgs)
                    | _ when i = 2  -> i + 1, {  input with MentalStatus = v }, (msg::msgs)
                    | _ when i = 3  -> i + 1, {  input with HeartRate = v }, (msg::msgs)
                    | _ when i = 4  -> i + 1, {  input with PupilsFixed = v }, (msg::msgs)
                    | _ when i = 5  -> i + 1, {  input with PH = v }, (msg::msgs)
                    | _ when i = 6  -> i + 1, {  input with TotalCO2 = v }, (msg::msgs)
                    | _ when i = 7  -> i + 1, {  input with PCO2 = v }, (msg::msgs)
                    | _ when i = 8  -> i + 1, {  input with PaO2 = v }, (msg::msgs)
                    | _ when i = 9  -> i + 1, {  input with Glucose = v }, (msg::msgs)
                    | _ when i = 10 -> i + 1, {  input with Potassium = v }, (msg::msgs)
                    | _ when i = 11 -> i + 1, {  input with Creatinine = v }, (msg::msgs)
                    | _ when i = 12 -> i + 1, {  input with Urea = v }, (msg::msgs)
                    | _ when i = 13 -> i + 1, {  input with WhiteBloodCount = v }, (msg::msgs)
                    | _ when i = 14 -> i + 1, {  input with PT = v }, (msg::msgs)
                    | _ when i = 15 -> i + 1, {  input with PTT = v }, (msg::msgs)
                    | _ when i = 16 -> i + 1, {  input with Platelets = v }, (msg::msgs)
                    | _ -> i, input, msgs
                ) (0, input, [])
                |> fun (_, input, msgs) ->
                    input |> PRISM.mapInputToPRISM prism |> Some ,
                    msgs

        patResult
        |> function
        | Result.Error _        -> patResult
        | Result.Ok (pat, msgs) ->
            let vmsgs, adms =
                pat.HospitalAdmissions
                |> List.collect (fun ha -> ha.PICUAdmissions)
                |> List.fold (fun (msgs, adms) adm ->
                    match clickData |> Array.tryFind (fun d -> d.adm_ic_id = adm.ClickId) with
                    | None   ->  msgs, adms
                    | Some d ->
                        d
                        |> cmpPRISM adm.PRISM24
                        |> fun (prism, vmsgs) ->
                            vmsgs
                            |> List.filter (isNullOrWhiteSpace >> not)
                            |> List.map (sprintf "%s: %s" pat.HospitalNumber)
                            |> List.append msgs, { adm with PRISM24 = prism }::adms
                ) ([], [])

            let pat =
                {
                    pat with
                        HospitalAdmissions =
                            pat.HospitalAdmissions
                            |> List.map (fun ha ->
                                { ha with
                                    PICUAdmissions =
                                        ha.PICUAdmissions
                                        |> List.map (fun pa ->
                                            match adms |> List.tryFind (fun x -> x.ClickId = pa.ClickId) with
                                            | Some a -> a
                                            | None   -> pa
                                        )
                                }
                            )
                }

            Result.okMsg pat (vmsgs |> List.toArray |> Array.append msgs)

    let filterDuplicateOrMore (results : Result<Patient * string[], string[]> array) =
        results
        |> Result.foldOk 
        |> function
        | Result.Error msgs       -> msgs |> Result.error
        | Result.Ok (pats, msgs1) ->
            printfn "start detecting duplicates"
            // Detect records with the same hospital number
            let pats, msgs2 =
                let distPats =
                    pats
                    |> Array.distinctBy (fun p -> p.HospitalNumber)

                let msgs =
                    pats
                    |> Array.filter (fun p ->
                        distPats
                        |> Array.exists ((=) p) |> not
                    )
                    |> Array.mapi (fun i p -> sprintf "%i. dupuplicate patient %s\n" i p.HospitalNumber)

                distPats, msgs

            Result.okMsg pats (msgs1 |> Array.append msgs2)


    let parsePICUAdmissions (picuData : MRDMPicu.Row[]) =
        let mapAdmType = Parsers.mapAdmissionType >> Result.ok
        let mapUrgency = Parsers.mapUrgency >> Result.ok
        let mapRisk d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs =
            Parsers.mapRiscDiagnosis d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs
            |> Result.ok
        let mapPupils  = Parsers.mapPupils >> Result.ok
        let mapAdmissionSource = Parsers.mapAdmissionSource >> Result.ok
        let mapLowRiskPRISM = Parsers.mapLowRiskPRISM >> Result.ok
        let getDiagn n s  =
            Parsers.parseDiagnose n s
            |> Result.ok

        let prism (d: MRDM.MRDMPicu.Row) =
            Patient.createPRISM
            <!> parseFloat d.``sbp-0``
            <*> parseFloat d.``t-min12``
            <*> parseFloat d.``t-max12``
            <*> parseInt d.``adm-emv``
            <*> parseInt d.``hr-max12``
            <*> parseInt d.admpupils
            <*> parseFloat d.``ph-min12``
            <*> parseFloat d.``ph-max12``
            <*> parseFloat d.``bicarbonate-min12``
            <*> parseFloat d.``bicarbonate-max12``
            <*> parseFloat d.``paco2-max12``
            <*> parseFloat d.``pao2-0``
            <*> parseFloat d.``glucose-max12``
            <*> parseFloat d.``k-max12``
            <*> parseFloat d.``creatinine-max12``
            <*> parseFloat d.``ureum-max12``
            <*> parseFloat d.``leuco-min12``
            <*> parseFloat d.``pt-max12``
            <*> parseFloat d.``ptt-max12``
            <*> parseFloat d.``thrombo-min12``
            <*> mapAdmissionSource d.``adm-sourceunitid``
            <*> parseBool d.contrean12
            <*> parseBool d.cancer
            <*> mapLowRiskPRISM d.``risicodiag-hoofd``

        let pim (d : MRDM.MRDMPicu.Row) =
            let cardiac g =
                let d1 =
                    d.diagnose1
                    |> Parsers.parseDiagnose "diagnose1"
                    |> List.exists (fun d -> d.Group = g)
                let d2 =
                    d.diagnose2
                    |> Parsers.parseDiagnose "diagnose2"
                    |> List.exists (fun d -> d.Group = g)
                d1 || d2

            Patient.createPIM
            <!> mapUrgency d.``adm-typeid-urgentie``
            <*> parseBool d.recovery
            <*> parseBool d.bypass
            <*> Result.ok (cardiac "cardiovasculair")
            <*> (mapRisk d.``risicodiag-hoofd``
                         d.``cprprehosp-riskpim``
                         d.``cprprepicu-riskpim``
                         d.``leukemie-riskpim``
                         d.``bmtrecipient-riskpim``
                         d.``sponthersenbl-riskpim``
                         d.``cardmyopath-riskpim``
                         d.``scid-riskpim``
                         d.``hiv-riskpim``
                         d.``neurodegen-riskpim``
                         d.``hypoplast-riskpim``)
            <*> parseBool d.ventilated
            <*> mapPupils d.admpupils
            <*> parseFloat d.``pao2-0``
            <*> parseFloat d.``fio2-0``
            <*> parseFloat d.``be-0``
            <*> parseFloat d.``sbp-0``

        picuData
        |> Array.map (fun d ->
            let find n c =
                if c |> isNullOrWhiteSpace then Result.ok None
                else
                    match MRDM.Codes.find n c with
                    | Some d -> d |> Some |> Result.ok 
                    | None ->
                        [| sprintf "couldn't find code %s with name %s" c n |]
                        |> Result.error

            Patient.createPICUAdmission
            <!> Result.ok d.``ziekenhuis-episode-upn``
            <*> Result.ok d.``adm-ic-id`` 
            <*> parseDateOpt d.``adm-ic-admdate``
            <*> parseDateOpt d.``adm-ic-disdate``
            <*> find "adm-disreasonid" d.``adm-disreasonid``
            <*> mapAdmType d.``adm-typeid-soort``
            <*> find "adm-indication" d.``adm-indication``
            <*> find "adm-refspecialism" d.``adm-refspecialism``
            <*> getDiagn "diagnose1" d.diagnose1
            <*> getDiagn "diagnose2" d.diagnose2
            <*> parseFloat d.gewicht
            <*> parseInt d.``adm-length``
            <*> parseBool d.contrean12
            <*> pim d
            <*> Result.ok None
            <*> prism d
            <*> Result.ok None
        )
        |> Result.foldOk 

    [<Literal>]
    let cachePath = "./../mrdm/data.cache"

    let parseMRDM () : Result<(Types.Patient [] * string []), string []> =
        match cachePath |> Cache.getCache<Result<(Types.Patient [] * string []), string []>> with
        | Some pats -> pats
        | None ->
            let pats =
                printfn "Start parsing, this can take a while ..."
                let timer = new Stopwatch ()
                timer.Start ()

                let hospData = mrdmHospital.Data |> Seq.toArray
                let picuData = mrdmPicu.Data |> Seq.toArray
                let picuAdms =
                    printfn "parsing picu admissions"
                    parsePICUAdmissions picuData
                let clickData = Click.pimprismHist.Data |> Seq.toArray

                let diagnoses =
                    mrdmDiagnos.Data
                    |> Seq.toArray
                    |> Array.map (fun r ->
                        {|
                            hn = r.``ziekenhuis-episode-upn``
                            ad = r.``adm-ic-admdate`` |> Parsers.parseDateOpt
                            dd = r.``adm-ic-disdate`` |> Parsers.parseDateOpt
                            dn = r.``bijkomende-diagnose``
                        |}
                    )

                let parsePat i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i parse patient" i
                    parsePatient hospData

                let parseHosp i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i parse hospital admission" i
                    parseHospAdm hospData

                let addPICU i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i add picu admission" i
                    addPICUAdmissions picuAdms diagnoses

                let validClick i =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: %i validated click data" i
                    validateWithClickData clickData

                let filter xs =
                    timer.ElapsedMilliseconds
                    |> printfn "%i: starting filtering duplicates"
                    let xs = xs |> filterDuplicateOrMore
                    timer.ElapsedMilliseconds
                    |> printfn "%i: finished filtering duplicates"
                    xs

                mrdmPatient.Data
                |> Seq.toArray
                |> Array.mapi parsePat
                |> Array.mapi parseHosp
                |> Array.mapi addPICU
                |> Array.mapi validClick
                |> filter

            pats |> Cache.cache cachePath
            pats


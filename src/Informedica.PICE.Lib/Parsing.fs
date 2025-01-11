namespace Informedica.PICE.Lib


module Parsing =

    open System
    open System.Diagnostics
    open System.Globalization

    open Informedica.Utils.Lib
    open Informedica.PimPrism.Lib
    open Types
    open MRDM

    open Result.Operators

    let isNullOrWhiteSpace (s: String) = s |> String.IsNullOrWhiteSpace
    let notNullOrWhiteSpace = isNullOrWhiteSpace >> not


    module Parsers =

        let parseInt (s: string) =
            match Int32.TryParse(s) with
            | true, x -> Some x
            | false, _ -> None


        let parseFloat (s: string) =
            match Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture) with
            | true, x -> Some x
            | false, _ -> None


        let parseDate (s: string) =
            try
                DateTime.Parse(s, DateTimeFormatInfo.InvariantInfo)
            with _ ->
                $"could not parse date {s}" |> failwith


        let parseDateOpt (s: string) =
            if s |> String.IsNullOrWhiteSpace then
                None
            else
                s |> parseDate |> Some


        let mapRiscDiagnosis d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs =
            match d with
            | s when s = "0" -> []
            | s when s = "1" -> [ Croup ]
            | s when s = "2" -> [ ObstructiveSleepApnea ]
            | s when s = "3" -> [ Bronchiolitis ]
            | s when s = "4" -> [ Asthma ]
            | s when s = "5" -> [ LiverFailure ]
            | s when s = "6" -> [ SeizureDisorder ]
            | s when s = "7" -> [ DiabeticKetoacidosis ]
            | s when s = "8" -> [ LiverFailure ]
            | s when s = "9" -> [ NecrotizingEnterocolitis ]
            | s when s = "10" -> [ DiabeticKetoacidosis ]
            //            | s when s = "11" -> [ PIM.CardiomyopathyOrMyocarditis ]
            | _ -> []
            |> List.append
                [
                    if leukemia = "1" then
                        LeukemiaorLymphoma
                    if bmt = "1" then
                        BoneMarrowTransplant
                    if cva = "1" then
                        CerebralHemorrhage
                    if card = "1" then
                        CardiomyopathyOrMyocarditis
                    if scid = "1" then
                        SevereCombinedImmuneDeficiency
                    if hiv = "1" then
                        HIVPositive
                    if neuro = "1" then
                        NeurodegenerativeDisorder
                    if hlhs = "1" then
                        HypoplasticLeftHeartSyndrome
                    if cprpre = "1" then
                        CardiacArrestPreHospital
                    if cprin = "1" then
                        CardiacArrestInHospital
                ]


        let mapUrgency =
            function
            | s when s = "10" -> NotElective
            | s when s = "11" -> Elective
            | _ -> UnknownUrgency


        let mapPupils =
            function
            | s when s = "1" -> FixedDilated
            | s when s = "0" -> NormalPupils
            | _ -> UnknownPupils


        let mapPatientState =
            function
            | s when s = "0" -> Alive
            | s when s = "1" -> Dead
            | _ -> UnknownPatientState


        let mapAdmissionType =
            function
            | s when s = "1" -> Medical
            | s when s = "2" -> Surgery
            | s when s = "3" -> DOA
            | _ -> UnknownAdmissionType


        let mapGender =
            function
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
        let mapAdmissionSource =
            function
            | s when s = "109" -> Recovery
            | s when s = "129" -> AnotherHospital
            | s when s = "115" || s = "107" -> EmergencyUnit
            | s when s = "99" || s = "77" -> UnknownAdmissionSource
            | _ -> InHospital


        let mapLowRiskPRISM s =
            [ "7"; "10"; "11" ] |> List.exists ((=) s)


        let parseDiagnose n id =
            id
            |> Options.find n
            |> function
                | None -> []
                | Some s ->
                    s.Label
                    |> String.replace "(" ""
                    |> String.replace ")" "|"
                    |> String.split "|"
                    |> function
                        | [ g; d ] ->
                            [
                                {
                                    Id = id
                                    Group = g |> String.trim |> String.toLower
                                    Name = d |> String.trim |> String.toLower
                                }
                            ]
                        | _ -> []


    let findOk n d =
        Options.find n d
        |> function
            | Some s -> s |> Some
            | None -> None
        |> Result.ok


    let parseDateOpt s =
        if s |> isNullOrWhiteSpace then
            None |> Result.ok
        else
            s |> Result.tryWithOk Parsers.parseDate

    let parseBool = ((=) "1") >> Result.ok


    let parseFloat s =
        if String.IsNullOrWhiteSpace(s) then
            Result.ok None
        else
            Result.okIfNone [| $"couldn't parse float %s{s}" |] (Parsers.parseFloat s)


    let parseInt s =
        if String.IsNullOrWhiteSpace(s) then
            Result.ok None
        else
            Result.okIfNone [| $"couldn't parse int %s{s}" |] (Parsers.parseInt s)

    
    let parseVent (s: string) =
        match s |> parseInt with
        | Ok (Some duration, _) ->
            match duration with
            | _ when duration > 3 * 30 -> Some { Id = "90"; Label = "> 3 maanden" }
            | _ when duration > 30 -> Some { Id = "30"; Label = "> 1 maand" }
            | _ when duration > 14 -> Some { Id = "14"; Label = "> 2 weken" }
            | _ when duration > 7 -> Some { Id = "07"; Label = "> 1 week" }
            | _ when duration > 2 -> Some { Id = "02"; Label = "> 2 dagen" }
            | _ when duration = 1 -> Some { Id = "01"; Label = "1 dag" }
            | _ when duration = 0 -> Some { Id = "00"; Label = "Geen" }
            | _ -> Some { Id = "00"; Label = "Geen" }
            |> Result.ok
        | Ok (None, _) -> Some {Id = "00"; Label = "Geen"} |> Result.ok
        | Error e -> Error e

    
    let parsePatient (hospData: MRDMHospital.Row[]) (d: MRDMPatient.Row) =
        let getHospNum (data: MRDMHospital.Row[]) =
            let errs, hn =
                data
                |> Array.filter (fun hd -> hd.patient_uri = d.uri)
                |> Array.map _.ziekenhuis_episode_upn
                |> Array.distinct
                |> function
                    | [||] -> [| $"no hospitalnumber for: %A{d}" |], ""
                    | [| hn |] -> [||], hn
                    | xs ->
                        let msg =
                            xs
                            |> Array.map (sprintf "%s")
                            |> Array.append [| $"multiple hospitalnumbers for %s{d.idcode}:" |]
                            |> String.concat "\n"

                        [| msg |], ""

            if errs |> Array.isEmpty then
                hn |> Result.ok
            else
                errs |> Result.error

        let mapPatientState = Parsers.mapPatientState >> Result.ok

        Patient.create <!> Result.ok d.uri
        <*> getHospNum hospData
        <*> Result.ok d.naam
        <*> Result.ok d.voornaam
        <*> parseDateOpt d.gebdat
        <*> (Parsers.mapGender >> Result.ok) d.geslacht
        <*> parseFloat d.pat_weight_of_birth
        <*> parseInt d.pat_zwduur
        <*> mapPatientState d.status
        <*> parseDateOpt d.datovl
        <*> findOk "adm-deathmodeid" d.adm_deathmodeid
        <*> findOk "adm-deceasedwhereid" d.adm_deceasedwhereid


    let parseHospAdm (hospData: MRDMHospital.Row[]) =
        let fErr msgs = msgs |> Result.Error

        let fOk (p: Patient, msgs1) =
            hospData
            |> Array.filter (fun d -> d.patient_uri = p.Id)
            |> Array.map (fun d ->
                Patient.createHospitalAdmission <!> Result.ok d.uri
                <*> Result.ok d.ziekenhuis_episode_upn
                <*> parseDateOpt d.adm_hosp_admdate
                <*> (findOk "herk-tran-door" d.herk_tran_door)
                <*> (findOk "adm-transport-adm" d.adm_transport_adm)
                <*> (findOk "adm-desthospunitid" d.adm_desthospitalid)
                <*> parseDateOpt d.adm_hosp_disdate)
            |> Result.foldOk
            |> function
                | Result.Ok(adms, msgs2) ->
                    msgs1
                    |> Array.append msgs2
                    |> Result.okMsg
                        { p with
                            HospitalAdmissions = adms |> Array.toList
                        }
                | Result.Error msgs2 -> msgs1 |> Array.append msgs2 |> Result.error

        Result.either fOk fErr


    let addPICUAdmissions
        (admissions: Result<PICUAdmission[] * string[], _>)
        (diagnoses: {| pi: string; dn: string |}[])
        =

        let calcPRISM bdt adt prism =
            match prism with
            | None -> None
            | Some prism ->
                { prism with Age = bdt }
                |> fun prism ->
                    match adt with
                    | Some dt ->
                        prism
                        |> PRISM.mapPRISMtoInput
                        |> PRISM.calculate dt
                        |> PRISM.mapInputToPRISM prism
                        |> Some
                    | None -> prism |> Some

        let fErr msgs = msgs |> Result.Error

        let fOk (p: Patient, msgs1) =
            match admissions with
            | Result.Ok(xs, msgs2) ->
                let p =
                    { p with
                        HospitalAdmissions =
                            p.HospitalAdmissions
                            |> List.map (fun ha ->
                                { ha with
                                    PICUAdmissions =
                                        xs
                                        |> Array.filter (fun pa -> ha.Id = pa.HospitalAdmissionId)
                                        |> Array.map (fun pa ->
                                            let diagnoses = diagnoses |> Array.filter (fun d -> d.pi = pa.Id)

                                            { pa with
                                                HospitalNumber = p.HospitalNumber
                                                PRISM24 = pa.PRISM24 |> calcPRISM p.BirthDate pa.AdmissionDate
                                                PRISM12 = pa.PRISM12 |> calcPRISM p.BirthDate pa.AdmissionDate
                                                PRISM4 = pa.PRISM4 |> calcPRISM p.BirthDate pa.AdmissionDate
                                                Diagnoses =
                                                    diagnoses
                                                    |> Array.toList
                                                    |> List.collect (fun d ->
                                                        d.dn |> Parsers.parseDiagnose "bijkomende-diagnose")
                                            })
                                        |> Array.toList
                                })
                    }

                Result.okMsg p (msgs1 |> Array.append msgs2)
            | Result.Error msgs2 -> Result.okMsg p (msgs1 |> Array.append msgs2)

        Result.either fOk fErr


    let filterDuplicateOrMore (results: Result<Patient * string[], string[]> array) =
        printfn $"going to foldOk {results |> Array.length} results"

        results
        |> Result.foldOk2
        |> function
            | Result.Error msgs -> msgs |> Result.error
            | Result.Ok(pats, msgs1) ->
                printfn $"start detecting duplicates"
                // Detect records with the same hospital number
                let pats, msgs2 =
                    let distPats = pats |> Array.distinctBy _.HospitalNumber

                    let msgs =
                        pats
                        |> Array.filter (fun p -> distPats |> Array.exists ((=) p) |> not)
                        |> Array.mapi (fun i p -> $"%i{i}. dupuplicate patient %s{p.HospitalNumber}\n")

                    distPats, msgs

                Result.okMsg pats (msgs1 |> Array.append msgs2)


    let parsePICUAdmissions (picuData: MRDMPicu.Row[]) =
        let mapAdmType = Parsers.mapAdmissionType >> Result.ok
        let mapUrgency = Parsers.mapUrgency >> Result.ok

        let mapRisk d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs =
            Parsers.mapRiscDiagnosis d cprpre cprin leukemia bmt cva card scid hiv neuro hlhs
            |> Result.ok

        let mapPupils = Parsers.mapPupils >> Result.ok
        let mapAdmissionSource = Parsers.mapAdmissionSource >> Result.ok
        let mapLowRiskPRISM = Parsers.mapLowRiskPRISM >> Result.ok
        let getDiagn n s = Parsers.parseDiagnose n s |> Result.ok
        let mapReadm s = s = "13" |> Result.ok

        let prism (d: MRDM.MRDMPicu.Row) =
            Patient.createPRISM <!> parseFloat d.sbp_0
            <*> parseFloat d.t_min12
            <*> parseFloat d.t_max12
            <*> parseInt d.adm_emv
            <*> parseInt d.hr_max12
            <*> parseInt d.admpupils
            <*> parseFloat d.ph_min12
            <*> parseFloat d.ph_max12
            <*> parseFloat d.bicarbonate_min12
            <*> parseFloat d.bicarbonate_max12
            <*> parseFloat d.paco2_max12
            <*> parseFloat d.pao2_0
            <*> parseFloat d.glucose_max12
            <*> parseFloat d.k_max12
            <*> parseFloat d.creatinine_max12
            <*> parseFloat d.ureum_max12
            <*> parseFloat d.leuco_min12
            <*> parseFloat d.pt_max12
            <*> parseFloat d.ptt_max12
            <*> parseFloat d.thrombo_min12
            <*> mapAdmissionSource d.adm_sourceunitid
            <*> parseBool d.contrean12
            <*> parseBool d.cancer
            <*> mapLowRiskPRISM d.risicodiag_hoofd

        let pim (d: MRDM.MRDMPicu.Row) =
            let cardiacSurg g =
                let d1 =
                    d.diagnose1
                    |> Parsers.parseDiagnose "diagnose1"
                    |> List.exists (fun d -> d.Group = g)

                let d2 =
                    d.diagnose2
                    |> Parsers.parseDiagnose "diagnose2"
                    |> List.exists (fun d -> d.Group = g)

                d1 || d2

            Patient.createPIM <!> mapUrgency d.adm_typeid_urgentie
            <*> parseBool d.recovery
            <*> parseBool d.bypass
            <*> Result.ok (cardiacSurg "hartchirurgie")
            <*> (mapRisk
                d.risicodiag_hoofd
                d.cprprehosp_riskpim
                d.cprprepicu_riskpim
                d.leukemie_riskpim
                d.bmtrecipient_riskpim
                d.sponthersenbl_riskpim
                d.cardmyopath_riskpim
                d.scid_riskpim
                d.hiv_riskpim
                d.neurodegen_riskpim
                d.hypoplast_riskpim)
            <*> parseBool d.ventilated
            <*> mapPupils d.admpupils
            <*> parseFloat d.pao2_0
            <*> parseFloat d.fio2_0
            <*> parseFloat d.be_0
            <*> parseFloat d.sbp_0

        picuData
        |> Array.map (fun d ->
            let find n c =
                if c |> isNullOrWhiteSpace then
                    Result.ok None
                else
                    match Options.find n c with
                    | Some d -> d |> Some |> Result.ok
                    | None -> [| $"couldn't find code %s{c} with name %s{n}" |] |> Result.error

            Patient.createPICUAdmission <!> Result.ok d.uri
            <*> Result.ok d.ziekenhuis_episode_uri
            <*> Result.ok d.adm_ic_id
            <*> Result.ok "" //d.``ziekenhuis-episode-upn``
            <*> mapReadm d.adm_readmtypeid
            <*> parseDateOpt d.adm_ic_admdate
            <*> parseDateOpt d.adm_ic_disdate
            <*> find "adm-disreasonid" d.adm_disreasonid
            <*> mapAdmType d.adm_typeid_soort
            <*> find "adm-indication" d.adm_indication
            <*> find "adm-refspecialism" d.adm_refspecialism
            <*> getDiagn "diagnose1" d.diagnose1
            <*> getDiagn "diagnose2" d.diagnose2
            <*> parseFloat d.gewicht
            <*> parseInt d.adm_length
            <*> parseBool d.contrean12
            <*> parseBool d.septische_shock
            <*> parseBool d.canule
            <*> parseInt d.adm_ventilationdays
            <*> parseInt d.adm_ventilationdaysin
            <*> parseInt d.adm_ventilationdaysni
            <*> parseInt d.adm_ventilationdaysother
            <*> parseVent d.adm_ventilationdaysin
            <*> pim d
            <*> Result.ok None
            <*> prism d
            <*> Result.ok None)
        |> Result.foldOk


    let parseMRDM exportPath cachePath : Result<Types.Patient[] * string[], string[]> =
        match cachePath |> Cache.getCache<Types.Patient[]> with
        | Some result -> (result, [||]) |> Ok
        | None ->
            let result =
                printfn "Start parsing, this can take a while ..."
                let timer = Stopwatch()
                timer.Start()

                let hospData = (getMrdmHospital exportPath).Data |> Seq.toArray
                let picuData = (getMrdmPicu exportPath).Data |> Seq.toArray

                let picuAdms =
                    printfn "parsing picu admissions"
                    parsePICUAdmissions picuData
                //                let clickData = Click.pimprismHist.Data |> Seq.toArray

                let diagnoses =
                    (getMrdmDiagnose exportPath).Data
                    |> Seq.toArray
                    |> Array.map (fun r ->
                        {|
                            pi = r.picu_episode_uri
                            dn = r.bijkomende_diagnose
                        |})

                let parsePat i =
                    Console.Write($"\r{i}: {timer.ElapsedMilliseconds} parse patient")
                    parsePatient hospData

                let parseHosp i =
                    Console.Write($"\r{i}: {timer.ElapsedMilliseconds} parse hospital admission")
                    parseHospAdm hospData

                let addPICU i =
                    Console.Write($"\r{i}: {timer.ElapsedMilliseconds} add picu admission")
                    addPICUAdmissions picuAdms diagnoses

                // let validClick i =
                //     timer.ElapsedMilliseconds
                //     |> printfn "%i: %i validated click data" i
                //     validateWithClickData clickData

                let filter xs =
                    timer.ElapsedMilliseconds |> printfn "%i: starting filtering duplicates"
                    let xs = xs |> filterDuplicateOrMore
                    timer.ElapsedMilliseconds |> printfn "%i: finished filtering duplicates"
                    xs

                (getMrdmPatient exportPath).Data
                |> Seq.toArray
                |> Array.mapi parsePat
                |> Array.mapi parseHosp
                |> Array.mapi addPICU
                //                |> Array.mapi validClick
                |> filter

            match result with
            | Ok(pats, _) -> pats |> Cache.cache cachePath

            | Error _ -> ()

            result

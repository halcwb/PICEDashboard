namespace Informedica.PICE.Lib



module Patient =

    open System
    open Types
    

    let create id 
               hosptitalNumber 
               birthDate 
               gender 
               birthWeight
               gestationAge
               vitalStatus 
               dateOfDeath 
               deathMode 
               locationOfDeath =
        {
            Id = id
            HospitalNumber = hosptitalNumber
            BirthDate = birthDate
            Gender = gender
            BirthWeight = birthWeight
            GestationalAge = gestationAge
            PatientState = vitalStatus
            DateOfDeath = dateOfDeath
            DeathMode = deathMode
            DeathLocation = locationOfDeath
            HospitalAdmissions = []
        }

    let createHospitalAdmission id hn adt trh trt dest ddt =
        {
            Id = id
            HospitalNumber = hn
            AdmissionDate = adt
            TransportHospital = trh
            TransportTeam = trt
            DischargeDate = ddt
            DischargeDestination = dest
            PICUAdmissions = []
        }

    let createPIM
        urgency
        recovery
        bypass
        cardiac
        riskDiagnosis
        ventilated
        pupils
        paO2
        fiO2
        be
        sbp
        =
        let recovMapping =
            match recovery, bypass, cardiac with
            | false, false, true  
            | false, false, false -> PIM.NoRecovery
            | false, true,  false // asume that post cardiac bypass is always recovery
            | false, true,  true  // asume that post cardiac bypass is always recovery
            | true,  true,  true  // asume that post cardiac bypass has precedence above cardiac
            | true,  true,  false -> PIM.PostCardiacByPass
            | true,  false, true  -> PIM.PostCariacNonByPass
            | true,  false, false -> PIM.PostNonCardiacProcedure
                   
        {
            Urgency = urgency
            Recovery = recovMapping
            RiskDiagnosis = riskDiagnosis
            Ventilated = ventilated
            AdmissionPupils = pupils
            PaO2 = paO2
            FiO2 = fiO2
            BaseExcess = be
            SystolicBloodPressure = sbp
            PIM2Score = None
            PIM2Mortality = None
            PIM3Score = None
            PIM3Mortality = None
        }
        |> PIM.calculatePIM2
        |> PIM.calculatePIM3


    let createPRISM
        sbpMin
        tempMin
        tempMax
        emv
        hrMax
        pupils
        phMin
        phMax
        bicMin
        bicMax
        pCO2Max
        paO2Min
        glucMax
        potassiumMax
        creatMax
        ureaMax
        wbcMin
        ptMax
        pttMax
        platMin
        admSource
        cpr
        cancer
        lowRisk =
        {
            Age = None
            SystolicBloodPressureMin = sbpMin
            TemperatureMin = tempMin
            TemperatureMax = tempMax
            MentalStatus = emv
            HeartRateMax = hrMax
            PupilsFixed = pupils
            PHMin = phMin
            PHMax = phMax
            BicarbonateMin = bicMin
            BicarbonateMax = bicMax
            PCO2Max = pCO2Max
            PaO2Min = paO2Min
            GlucoseMax = glucMax
            PotassiumMax = potassiumMax
            CreatinineMax = creatMax
            UreaMax = ureaMax
            WhiteBloodCountMin = wbcMin
            PTMax = ptMax
            PTTMax = pttMax
            PlateletsMin = platMin
            AdmissionSource = admSource
            CPR24HourBefore = cpr
            Cancer = cancer
            LowRiskPrimary = lowRisk
            PRISM3Score = None
            PRISM3Neuro = None
            PRISM4Mortality = None
        }
        |> Some


    let createPICUAdmission
        id 
        hospitalAdmissionId
        clickId
        hospitalNumber
        admissionDate
        dischargeDate
        dischargeReason
        admissionType
        admissionIndication
        referingSpecialism
        primaryDiagn
        secondaryDiagn
        admissionWeight
        admissionLength
        contReanimation
        canule
        pim
        prism24
        prism12
        prism4
        =
        {
            Id = id
            HospitalAdmissionId = hospitalAdmissionId
            ClickId = clickId
            HospitalNumber = hospitalNumber
            AdmissionDate = admissionDate
            DischargeDate = dischargeDate
            DischargeReason = dischargeReason
            AdmissionType = admissionType
            AdmissionIndication = admissionIndication
            ReferingSpecialism = referingSpecialism
            PrimaryDiagnosis = primaryDiagn
            SecondaryDiagnosis = secondaryDiagn
            Diagnoses = []
            AdmissionWeight = admissionWeight
            AdmissionLength = admissionLength
            ContinuousReanimation = contReanimation
            Canule = canule
            PIM = pim
            PRISM24 = prism24
            PRISM12 = prism12
            PRISM4 = prism4
        }

    let picuAdmissionToString (a : PICUAdmission) =
        sprintf "%A %s %s"
            (a.AdmissionDate)
            (a.ReferingSpecialism |> Utils.DataOption.optToString )
            (a.AdmissionIndication |> Utils.DataOption.optToString)


    let hospitalAdmissionToString (a : HospitalAdmission) =
        sprintf "Ziekenhuis opname: %A - %A"
            a.AdmissionDate
            a.DischargeDate
        |> fun s ->
            a.PICUAdmissions
            |> List.map (picuAdmissionToString >> (sprintf "%s %s") s)


    let piceAdmissionToString (a : Patient) =
        sprintf "%s: %A"
            a.HospitalNumber
            a.PatientState
        |> fun s ->
            a.HospitalAdmissions
            |> List.collect (fun ha ->
                ha
                |> hospitalAdmissionToString
                |> List.map (sprintf "%s %s" s))


namespace Informedica.PICE.Lib


module Statistics =

    open System
    open System.Text

    open Types
    open Utils
    open Validation


    type Totals () =
        member val Patients = 0 with get, set
        member val Admissions = 0 with get, set
        member val Admitted = 0 with get, set
        member val Discharged = 0 with get, set
        member val PICUDays = 0 with get, set
        member val Deaths = 0 with get, set
        member val HospitalDeaths = 0 with get, set
        member val PICUDeaths = 0 with get, set
        member val PIM2Mortality = 0. with get, set
        member val PIM3Mortality = 0. with get, set
        member val PRISM4Mortality = 0. with get, set
        member val Gender : (string * int) list = [] with get, set
        member val AgeGroup : (string * int) list = [] with get, set
        member val DischargeReasons : (string * int) list = [] with get, set
        member val HospitalDischargeDestinations : (string * int) list = [] with get, set


    type MonthTotals () =
        member val Month = 0 with get, set
        member val Totals = Totals ()


    type YearTotals () =
        member val Year = 0 with get, set
        member val Totals = Totals ()
        member val MonthTotals : MonthTotals list = [] with get, set


    type Statistics () =
        member val Totals : Totals = Totals () with get, set
        member val YearTotals : YearTotals list = [] with get, set
        member val InvalidPatients : (string * int) list = [] with get, set


    let periodInYear yr (from : DateTime option) (until : DateTime option) =
        if from > until then
            sprintf "from: %A cannot be later than until %A" from until
            |> failwith
        else
            match from, until with
            | None,     _    -> false
            | Some fdt, None -> fdt.Year <= yr
            | Some fdt, Some udt ->
                fdt.Year <= yr && udt.Year >= yr


    let dateInYear yr (date: DateTime option) =
        match date with
        | None     -> false
        | Some dt -> dt.Year = yr


    let periodInYearMonth yr mo from until  =
        if periodInYear  yr from until |> not then false
        else
            match from, until with
            | None,     _    -> false
            | Some adt, None -> adt.Month <= mo
            | Some adt, Some ddt ->
                adt.Month <= mo && ddt.Month >= mo
       

    let dateInYearMonth yr mo date =
        if dateInYear yr date |> not then false
        else
            match date with
            | None     -> false
            | Some dt -> dt.Month = mo


    let periodFilter yr mo adt ddt =
        match yr, mo with
        | None, None -> true
        | Some y, None   -> periodInYear  y adt ddt
        | Some y, Some m -> periodInYearMonth y m adt ddt
        | _ -> "not a valid filter" |> failwith


    let dateFilter yr mo adt =
        match yr, mo with
        | None,   None -> true
        | Some y, None   -> dateInYear y adt
        | Some y, Some m -> dateInYearMonth y m adt
        | _ -> "not a valid filter" |> failwith


    let calculate (pats: Patient list) =
        let stats = Statistics ()

        let getPRISMMort (pa : PICUAdmission) =
            match pa.PRISM4, pa.PRISM12, pa.PRISM24 with
            | Some prism, _, _
            | None, Some prism, _
            | None, None, Some prism -> prism.PRISM4Mortality
            | _ -> None

        let countBy un (ds : DataOption option list) =
            ds
            |> List.countBy id
            |> List.sortByDescending snd
            |> List.map (fun (k, v) -> 
                match k with
                | Some d -> d.Label, v
                | None   -> un, v
            )

        let genderToCount (pats : Patient list) = 
            pats
            |> List.distinctBy (fun p -> p.HospitalNumber)
            |> List.map (fun p -> 
                match p.Gender with
                | Male -> "Man"
                | Female -> "Vrouw"
                | UnknownGender -> "Onbekend"
            )
            |> List.countBy id

        let ageToCount (dts : (DateTime option * DateTime option) list) =
            dts
            |> List.map (fun (ad, bd) ->
                match ad, bd with
                | Some ad, Some bd ->
                    match (ad - bd).TotalDays with
                    | ds when ds < 28.  -> "0 dagen - 4 weken"
                    | ds when ds < 365. -> "1 maand - 1 jaar"
                    | ds when ds < (365. * 4.)  -> "1 jaar - 4 jaar"
                    | ds when ds < (365. * 12.) -> "4 jaar - 12 jaar"
                    | ds when ds < (365. * 16.) -> "12 jaar - 16 jaar"
                    | ds when ds < (365. * 18.) -> "16 jaar - 18 jaar"
                    | _ -> "ouder dan 18 jaar"
                | _, _ -> "onbekende leeftijd"
            )
            |> List.countBy id
            |> List.sortBy (fun (k, _) ->
                match k with
                | s when s = "0 dagen - 4 weken" -> 0
                | s when s = "1 maand - 1 jaar"  -> 1
                | s when s = "1 jaar - 4 jaar"   -> 2
                | s when s = "4 jaar - 12 jaar"  -> 3
                | s when s = "12 jaar - 16 jaar" -> 4
                | s when s = "16 jaar - 18 jaar" -> 5
                | s when s = "ouder dan 18 jaar" -> 6
                | _ -> 999
            )
             
        let pats =
            let notValid =
                pats
                |> List.map validatePat
                |> List.filter (fun errs -> errs |> List.length > 0)
                |> fun errs ->
                    stats.InvalidPatients <-
                        errs
                        |> List.collect (fun errs ->
                            errs
                            |> List.collect (fun err -> match err with | IsValid -> [] | NotValid(_, s) -> [s])
                        )
                        |> List.countBy id

                    errs
                |> List.collect (fun errs ->
                    errs
                    |> List.collect (fun err -> match err with | IsValid -> [] | NotValid(p, _) -> [p])
                )
                |> List.distinct

            pats
            |> List.filter (fun p ->
                notValid
                |> List.exists ((=) p)
                |> not 
            )
            |> List.distinctBy (fun p -> p.HospitalNumber)
            |> List.collect (fun p ->
                p.HospitalAdmissions
                |> List.collect (fun ha ->
                    ha.PICUAdmissions
                    |> List.distinct
                    |> List.map (fun pa ->
                        {|
                            patient = p
                            hospitalAdmission = ha
                            picuAdmission = pa
                        |}
                    )
                    // Exclude current year
                    |> List.filter (fun p ->
                        match p.picuAdmission.AdmissionDate with
                        | None -> false
                        | Some dt when dt.Year = DateTime.Now.Year -> false
                        | _ -> true
                    )
                )
            )

        stats.Totals.Patients <-
            pats
            |> List.map (fun p -> p.patient)
            |> List.distinctBy (fun p -> p.HospitalNumber)
            |> List.length

        stats.Totals.Admissions <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.distinct
            |> List.length

        stats.Totals.Discharged <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa -> pa.DischargeDate |>  Option.isSome)
            |> List.length

        stats.Totals.Deaths <-
            pats
            |> List.map (fun p -> p.patient)
            |> List.filter (fun p -> 
                p.PatientState = Dead || 
                p.DateOfDeath |> Option.isSome ||
//                p.DeathMode <> "" ||
//                p.DeathLocation <> "Niet overleden" ||
                p.HospitalAdmissions
                |> List.exists (fun ha ->
                    (ha.DischargeDestination |> DataOption.EqsIdOpt "128") ||
                    ha.PICUAdmissions
                    |> List.exists (fun pa ->
                        pa.AdmissionType = DOA ||
                        (pa.DischargeReason |> DataOption.EqsIdOpt "100")
                    )
                )
            )
            |> List.distinctBy (fun p -> p.HospitalNumber)
            |> List.length

        stats.Totals.PIM2Mortality <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa ->
                pa.DischargeDate |>  Option.isSome &&
                pa.PIM.PIM2Mortality |> Option.isSome
            )
            |> List.map (fun pa -> pa.PIM.PIM2Mortality |> Option.get)
            |> List.filter (Double.IsNaN >> not)
            |> List.sum

        stats.Totals.PIM3Mortality <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa ->
                pa.DischargeDate |>  Option.isSome &&
                pa.PIM.PIM3Mortality |> Option.isSome
            )
            |> List.map (fun pa -> pa.PIM.PIM3Mortality |> Option.get)
            |> List.filter (Double.IsNaN >> not)
            |> List.sum

        stats.Totals.PRISM4Mortality <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.filter (fun pa ->
                pa.DischargeDate |>  Option.isSome &&
                pa |> getPRISMMort |> Option.isSome
            )
            |> List.map (fun pa -> pa |> getPRISMMort |> Option.get)
            |> List.filter (Double.IsNaN >> not)
            |> List.sum

        stats.Totals.DischargeReasons <-
            pats
            |> List.map (fun p  -> p.picuAdmission.DischargeReason)
            |> countBy "Onbekend"

        stats.Totals.HospitalDischargeDestinations <-
            let unknown = 
                MRDM.Codes.find "adm-desthospunitid" "99"
                |> function
                | Some d -> d.Label
                | None   -> ""

            pats
            |> List.map (fun p ->  p.hospitalAdmission.DischargeDestination)
            |> countBy unknown

        stats.Totals.Gender <- pats |> List.map (fun p -> p.patient) |> genderToCount

        stats.Totals.AgeGroup <-
            pats
            |> List.map (fun p -> p.picuAdmission.AdmissionDate, p.patient.BirthDate)
            |> ageToCount

        stats.Totals.PICUDays <-
            pats
            |> List.map (fun p -> p.picuAdmission)
            |> List.map (fun p ->
                match p.AdmissionDate, p.DischargeDate with
                | Some adt, Some ddt -> (ddt - adt).TotalDays
                | _ -> 0.
            )
            |> List.sum
            |> int

        let yrTots =
            [ 2003..DateTime.Now.Year - 1 ]
            |> List.map (fun yr ->
                let tot = new YearTotals()
                tot.Year <- yr
                tot.MonthTotals <-
                    [1..12]
                    |> List.map (fun m ->
                        let stat = new MonthTotals ()
                        stat.Month <- m
                        stat
                    )
                tot
            )

        let inline filterAdmitted f m = 
            pats
            |> List.filter (fun d -> d.picuAdmission.DischargeDate |> Option.isSome)
            |> List.filter (fun d ->
                f d.picuAdmission.AdmissionDate d.picuAdmission.DischargeDate
            )
            |> List.map m

        let inline filterAdmission f m = 
            pats
            |> List.filter (fun p ->
                f p.picuAdmission.AdmissionDate
            )
            |> List.map m

        let inline filterDischarged f m =
            pats
            |> List.filter (fun p ->
                f p.picuAdmission.DischargeDate
            )
            |> List.map m

        // Patient statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let admitted = 
                filterAdmitted (periodFilter yr None) (fun d -> d.patient)
                |> List.distinctBy (fun p -> p.HospitalNumber)

            tot.Totals.Patients <-
                admitted
                |> List.length

            tot.Totals.Deaths <-
                admitted
                |> List.filter (fun p ->
                    match p.DateOfDeath with
                    | None    -> false
                    | Some dt -> dt.Year = tot.Year
                )
                |> List.length

            tot.Totals.PIM2Mortality <-
                pats
                |> List.map (fun p -> p.picuAdmission)
                |> List.filter (fun pa ->
                    pa.DischargeDate |>  Option.isSome &&
                    pa.PIM.PIM2Mortality |> Option.isSome
                )
                |> List.filter (fun pa -> pa.DischargeDate.Value.Year = yr.Value)
                |> List.map (fun pa -> pa.PIM.PIM2Mortality |> Option.get)
                |> List.filter (Double.IsNaN >> not)
                |> List.sum

            tot.Totals.PIM3Mortality <-
                pats
                |> List.map (fun p -> p.picuAdmission)
                |> List.filter (fun pa ->
                    pa.DischargeDate |>  Option.isSome &&
                    pa.PIM.PIM3Mortality |> Option.isSome
                )
                |> List.filter (fun pa -> pa.DischargeDate.Value.Year = yr.Value)
                |> List.map (fun pa -> pa.PIM.PIM3Mortality |> Option.get)
                |> List.filter (Double.IsNaN >> not)
                |> List.sum

            tot.Totals.PRISM4Mortality <-
                pats
                |> List.map (fun p -> p.picuAdmission)
                |> List.filter (fun pa ->
                    pa.DischargeDate |>  Option.isSome &&
                    pa |> getPRISMMort |> Option.isSome
                )
                |> List.filter (fun pa -> pa.DischargeDate.Value.Year = yr.Value)
                |> List.map (fun pa -> pa |> getPRISMMort |> Option.get)
                |> List.filter (Double.IsNaN >> not)
                |> List.sum
        )
        // PICU admission statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let admissions = filterAdmission (dateFilter yr None) (fun d -> d.picuAdmission)

            tot.Totals.Admissions <-
                admissions
                |> List.length

            tot.Totals.Gender <-
                filterAdmission (dateFilter yr None) id
                |> List.map (fun p -> p.patient)
                |> genderToCount

            tot.Totals.AgeGroup <-
                filterAdmission (dateFilter yr None) id
                |> List.map (fun p -> p.picuAdmission.AdmissionDate, p.patient.BirthDate)
                |> ageToCount

        )
        // PICU discharge statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let discharged = filterDischarged (dateFilter yr None) (fun d -> d.picuAdmission)

            tot.Totals.Discharged <-
                discharged
                |> List.length

            tot.Totals.DischargeReasons <-
                discharged
                |> List.map (fun a -> a.DischargeReason )
                |> countBy "Onbekend"
        )
        // PICU admitted statistics
        yrTots
        |> List.iter (fun tot ->
            let yr = Some tot.Year
            let admitted = filterAdmitted (periodFilter yr None) (fun d -> d.picuAdmission)

            tot.Totals.Admitted <-
                admitted
                |> List.length

            tot.Totals.PICUDays <-
                admitted
                |> List.map (fun pa ->
                    let start = DateTime(tot.Year, 1, 1)
                    let stop = DateTime(tot.Year, 12, 31)

                    let adt =
                        pa.AdmissionDate |> Option.get
                        |> fun dt -> if dt.Year < tot.Year then start else dt
                    let ddt =
                        pa.DischargeDate |> Option.get
                        |> fun dt -> if dt.Year > tot.Year then stop else dt

                    (ddt - adt).TotalDays 
                )
                |> List.sum
                |> int
        )

        yrTots
        |> List.iter (fun yrTot ->
            yrTot.MonthTotals <-
                [1..12]
                |> List.filter (fun mo ->
                    DateTime(yrTot.Year, mo, 1) > DateTime.Now |> not
                )
                |> List.map (fun mo ->
                    let moTot = MonthTotals()

                    moTot.Month <- mo

                    let yr = Some yrTot.Year
                    let mo = Some mo

                    moTot.Totals.Patients <-
                        filterAdmitted (periodFilter yr mo) (fun d -> d.patient)
                        |> List.length

                    moTot.Totals.Deaths <-
                        filterAdmitted (periodFilter yr mo) (fun d -> d.patient)
                        |> List.filter (fun p -> p.DateOfDeath |> Option.isSome)
                        |> List.length

                    moTot.Totals.Admissions <-
                        filterAdmission (dateFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.length

                    moTot.Totals.Admitted <-
                        filterAdmitted (periodFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.length

                    moTot.Totals.Discharged <-
                        filterDischarged (dateFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.length

                    moTot.Totals.PICUDays <-
                        filterAdmitted (periodFilter yr mo) (fun d -> d.picuAdmission)
                        |> List.map (fun pa ->
                            let start = DateTime(yrTot.Year, moTot.Month, 1)
                            let stop = start.AddMonths(1)

                            let adt =
                                pa.AdmissionDate |> Option.get
                                |> fun dt ->
                                    if dt.Month < moTot.Month then start else dt
                            let ddt =
                                pa.DischargeDate |> Option.get
                                |> fun dt -> if dt.Month > moTot.Month then stop else dt

                            (ddt - adt).TotalDays 
                        )
                        |> List.sum
                        |> int

                    moTot
                )
        )

        stats.YearTotals <- yrTots
        stats
    
    module Literals =

        [<Literal>]
        let line = "---"
        [<Literal>]
        let headers5 = "|---|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns5 = "|{0}|{1}|{2}|{3}|{4}|"
        [<Literal>]
        let headers6 = "|---|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns6 = "|{0}|{1}|{2}|{3}|{4}|{5}|"
        [<Literal>]
        let columns6link = "|[{0}]()|{1}|{2}|{3}|{4}|{5}|"
        [<Literal>]
        let columns6tick = "|{0}|`{1}`|`{2}`|`{3}`|`{4}`|`{5}`|"
        [<Literal>]
        let headers8 = "|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns8 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|"
        [<Literal>]
        let headers9 = "|---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|"
        [<Literal>]
        let columns9 = "|{0}|{1}|{2}|{3}|{4}|{5}|{6:F0}|{7:F0}|{8:F0}|"
        [<Literal>]
        let patTot = "* Totaal aantal patienten: {0}"
        [<Literal>]
        let adsTot = "* Totaal aantal opnames: {0}"
        [<Literal>]
        let disTot = "* Totaal aantal ontslagen: {0}"
        [<Literal>]
        let adtTot = "* Totaal aantal opgenomen: {0}"
        [<Literal>]
        let dthTot = "* Totaal aantal overleden: {0}"
        [<Literal>]
        let dayTot = "* Totaal aantal verpleegdagen: {0}"
        [<Literal>]
        let estPIM2 = "* Geschatte PIM2 mortaliteit: {0:F0}"
        [<Literal>]
        let estPIM3 = "* Geschatte PIM3 mortaliteit: {0:F0}"
        [<Literal>]
        let estPRISM = "* Geschatte PRISM4 mortaliteit: {0:F0}"
        [<Literal>]
        let yearTitle = "#### Totalen van {0}"
        [<Literal>]
        let monthTitle = "{0:MMMM}"
        [<Literal>]
        let countItem = "* {0}: {1}"

    let toString (stats : Statistics) =
        let calcPerc t n  =
            try
                if t > 0 then
                    StringBuilder.builder ""
                    |> StringBuilder.appendFormat "{0:F0} ({1:F0}%)" [ n |> box; (100. * n / (t |> float)) |> box ]
                    |> StringBuilder.toString
                else
                    sprintf "%A" n 
            with 
            | e -> sprintf "error calcPerc %A %A\n%s" t n (e.ToString())
                   |> failwith

        let printCount title kvs sort sb =
            sb
            |> StringBuilder.appendLine title
            |> fun sb ->
                let t =
                    kvs
                    |> List.map snd
                    |> List.sum

                kvs
                |> fun xs -> if sort then xs |> List.sortByDescending snd else xs
                |> List.fold (fun acc (s, c) ->
                    let c = calcPerc t (float c)
                    acc
                    |> StringBuilder.appendLineFormat Literals.countItem [ s |> box; c |> box ]
                ) sb

        let printTotals n t (totals : Totals) sb =
            let calc = calcPerc totals.Patients
            sb
            |> StringBuilder.appendLineFormat t [ n |> box ]
            |> StringBuilder.appendLineFormat Literals.patTot [ totals.Patients |> box ]
            |> StringBuilder.appendLineFormat Literals.adsTot [ totals.Admissions |> box ]
            |> StringBuilder.appendLineFormat Literals.disTot [ totals.Discharged |> box ]
            |> StringBuilder.appendLineFormat Literals.dayTot [ totals.PICUDays |> box ]
            |> StringBuilder.appendLineFormat Literals.dthTot [ calc (float totals.Deaths) |> box ]
            |> StringBuilder.appendLineFormat Literals.estPIM2 [ calc totals.PIM2Mortality |> box ]
            |> StringBuilder.appendLineFormat Literals.estPIM3 [ calc totals.PIM3Mortality |> box ]
            |> StringBuilder.newLine
            |> StringBuilder.newLine
            |> StringBuilder.appendLine "De getoonde mortaliteit in bovenstaande lijst is de ziekenhuis mortaliteit"
            |> StringBuilder.newLine
            |> StringBuilder.newLine
            |> printCount "#### Geslacht" totals.Gender true
            |> StringBuilder.newLine
            |> StringBuilder.newLine
            |> printCount "#### Leeftijdsgroup" totals.AgeGroup false
            |> StringBuilder.newLine
            |> StringBuilder.newLine
            |> printCount "#### PICU Ontslag redenen" totals.DischargeReasons true

        let printMonthTabel (yrTot : YearTotals) sb =
            let caps =
                [
                    "Maand"
                    "Patienten"
                    "Opnames"
                    "Ontslagen"
                    "Ligdagen"
                    "Overleden"
                ]
                |> List.map box

            let sb =
                sb
                |> StringBuilder.newLine
                |> StringBuilder.appendLine "#### Per maand"
                |> StringBuilder.appendLineFormat Literals.columns6 caps
                |> StringBuilder.appendLine Literals.headers6

            yrTot.MonthTotals
            |> List.fold (fun acc stat ->
                let mo =
                    StringBuilder.builder ""
                    |> StringBuilder.appendFormat Literals.monthTitle [ DateTime(2000, stat.Month, 1) |> box ]
                let vals =
                    [
                        mo.ToString ()         |> box
                        stat.Totals.Patients   |> box
                        stat.Totals.Admissions |> box
                        stat.Totals.Discharged |> box
                        stat.Totals.PICUDays   |> box
                        calcPerc stat.Totals.Patients (float stat.Totals.Deaths) |> box
                    ]
        
                acc
                |> StringBuilder.appendLineFormat Literals.columns6 vals
            ) sb

        let yrs =
            stats.YearTotals
            |> List.sortByDescending (fun t -> t.Year)
            |> List.fold (fun acc stat ->
                acc
                |> StringBuilder.appendLine (sprintf "## Rapportage over %i" stat.Year)
                |> printTotals stat.Year Literals.yearTitle stat.Totals
                |> printMonthTabel stat
                |> fun sb ->
                    sb
                    |> StringBuilder.newLine
                    |> StringBuilder.newLine

            ) ("" |> StringBuilder.builder)
            |> StringBuilder.toString

        let caps =
            [
                "Jaar"
                "Patienten"
                "Opnames"
                "Ontslagen"
                "Ligdagen"
                "Overleden"
                "PIM2 Mortaliteit"
                "PIM3 Mortaliteit"
                "PRISM4 Mortaliteit"
            ]
            |> List.map box

        "# PICE Rapport"
        |> StringBuilder.builder
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> printCount "#### Validatie" stats.InvalidPatients true
        |> StringBuilder.newLine
        |> StringBuilder.appendLine "#### Totalen over de hele periode"
        |> StringBuilder.appendLineFormat Literals.patTot [ stats.Totals.Patients |> box ]
        |> StringBuilder.appendLineFormat Literals.adsTot [ stats.Totals.Admissions |> box ]
        |> StringBuilder.appendLineFormat Literals.disTot [ stats.Totals.Discharged |> box ]
        |> StringBuilder.appendLineFormat Literals.dayTot [ stats.Totals.PICUDays |> box ]
        |> StringBuilder.appendLineFormat Literals.dthTot [ calcPerc stats.Totals.Patients (float stats.Totals.Deaths) |> box ]
        |> StringBuilder.appendLineFormat Literals.estPIM2 [ calcPerc stats.Totals.Patients stats.Totals.PIM2Mortality |> box ]
        |> StringBuilder.appendLineFormat Literals.estPIM3 [ calcPerc stats.Totals.Patients stats.Totals.PIM3Mortality |> box ]
        |> StringBuilder.appendLineFormat Literals.estPRISM [ calcPerc stats.Totals.Patients stats.Totals.PRISM4Mortality |> box ]
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> StringBuilder.appendLine "De getoonde mortaliteit in bovenstaande lijst is de totale mortaliteit"
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> printCount "#### Geslacht" stats.Totals.Gender true
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> printCount "#### Leeftijdsgroup" stats.Totals.AgeGroup false
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> printCount "#### Ziekenhuis ontslag bestemming" stats.Totals.HospitalDischargeDestinations true
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> printCount "#### PICU ontslag redenen" stats.Totals.DischargeReasons true
        |> StringBuilder.newLine
        |> StringBuilder.newLine
        |> StringBuilder.appendLine "#### Per jaar"
        |> fun sb ->
            let sb =
                sb
                |> StringBuilder.appendLineFormat Literals.columns9 caps
                |> StringBuilder.appendLine Literals.headers9
            stats.YearTotals
            |> List.sortByDescending (fun t -> t.Year)
            |> List.fold (fun acc stat ->
                let calc = calcPerc stat.Totals.Patients
                let vals =
                    [
                        stat.Year              |> box
                        stat.Totals.Patients   |> box
                        stat.Totals.Admissions |> box
                        stat.Totals.Discharged |> box
                        stat.Totals.PICUDays   |> box
                        calc (float stat.Totals.Deaths) |> box
                        calc stat.Totals.PIM2Mortality |> box
                        calc stat.Totals.PIM3Mortality |> box
                        calc stat.Totals.PRISM4Mortality |> box
                    ]
        
                acc
                |> StringBuilder.appendLineFormat Literals.columns9 vals
            ) sb
            |> fun sb ->
                let t = stats.YearTotals |> List.sumBy (fun s -> s.Totals.Patients)
                let calc = calcPerc t
                let vals =
                    [
                        "Totalen"            |> box
                        stats.YearTotals |> List.sumBy (fun s -> s.Totals.Patients) |> box
                        stats.YearTotals |> List.sumBy (fun s -> s.Totals.Admissions) |> box
                        stats.YearTotals |> List.sumBy (fun s -> s.Totals.Discharged) |> box
                        stats.YearTotals |> List.sumBy (fun s -> s.Totals.PICUDays)   |> box
                        calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.Deaths) |> float) |> box
                        calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.PIM2Mortality)) |> box
                        calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.PIM3Mortality)) |> box
                        calc (stats.YearTotals |> List.sumBy (fun s -> s.Totals.PRISM4Mortality)) |> box
                    ]
                
                sb
                |> StringBuilder.appendLineFormat Literals.columns9 vals
        |> StringBuilder.newLine
        |> StringBuilder.appendLine yrs
        |> StringBuilder.toString


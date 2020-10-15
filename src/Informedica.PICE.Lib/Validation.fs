namespace Informedica.PICE.Lib

module Validation =

    open System
    open Types

    let validatePat (p : Patient) =
        let fromLaterThanUntil from until =
            match from, until with
            | Some from, Some until -> from > until
            | _ -> false
            
        [
            match p.BirthDate, p.DateOfDeath with
            | None, _ -> NotValid (p, "Geen geboortedatum") 
            | Some _, None -> IsValid
            | Some bd, Some dd ->
                if bd > dd then
                    NotValid (p, "Geboortedatum na datum van overlijden")

            match p.DateOfDeath, p.PatientState with
            | Some _, Alive -> NotValid (p, "Patient status is in leven maar datum van overlijden bekend")
            | _ -> IsValid

            let xs =
                p.HospitalAdmissions
                |> List.filter (fun ha -> fromLaterThanUntil ha.AdmissionDate ha.DischargeDate)

            if xs |> List.length > 0 then
                NotValid (p, "Ziekenhuis opname datum na ontslag datum")

            let xs =
                p.HospitalAdmissions
                |> List.filter (fun ha ->
                    ha.PICUAdmissions
                    |> List.exists (fun pa -> fromLaterThanUntil pa.AdmissionDate pa.DischargeDate)
                )

            if xs |> List.length > 0 then
                NotValid(p, "PICU Opname datum na ontslag datum")

            if String.IsNullOrWhiteSpace(p.HospitalNumber) then
                NotValid(p, "Geen ziekenhuis nummer")
        ]
        |> List.filter ((<>) IsValid)


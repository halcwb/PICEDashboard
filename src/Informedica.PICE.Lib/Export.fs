namespace Informedica.PICE.Lib


module Export =

    open System
    open System.Data

    open Microsoft.Data.SqlClient

    open Types

    [<Literal>]
    let serverName = "VOXDB-PICURED01"
    [<Literal>]
    let databaseName = "PICE_MRDM"

    let tableName = $"""PimPrism_{DateTime.Now.ToString("yy_MM_dd_HHmm")}"""


    let getPatientState (pa : PICUAdmission) (pat : Patient) =
        match pa.AdmissionDate, pa.DischargeDate with
        | Some dt1, Some dt2 ->
            match pat.DateOfDeath with
            | Some dt -> 
                if dt >= dt1 && dt <= dt2 then "Death"
                else
                    match pa.DischargeReason with
                    | None -> "Alive"
                    | Some dr ->
                        if dr.Id = "100" then "Death" else "Alive"
            | None -> "Alive"
        | _ -> "Alive"


    let optDateDiff fSome defVal (dt1 : DateTime option) (dt2 : DateTime option) =
        match dt1, dt2 with
        | Some d1, Some d2 -> (d1 - d2).TotalDays |> fSome
        | _, _             -> defVal


    let export (pats: Result<Patient[] * string[], _>) =
        let optToString = Option.map string >> Option.defaultValue ""
        let optDateDiff = optDateDiff string ""

        let headers =
            [
                "HospitalNumber"
                "AdmissionDate"
                "DischargeDate"
                "Age"
                "RiskDiagnoses"
                "Urgency"
                "Recovery"
                "Ventilated"
                "AdmissionPupils"
                "SystolicBP"
                "BaseExcess"
                "FiO2"
                "PaO2"
                "PIM2Score"
                "PIM2Mort"
                "PIM3Score"
                "PIM3Mort"
                "AdmissionSource"
                "Cancer"
                "CPR24HourBefore"
                "CreatinineMax"
                "GlucoseMax"
                "HeartRateMax"
                "LowRiskPrimary"
                "MentalStatus"
                "PaO2Min"
                "PCO2Max"
                "PHMin"
                "PHMax"
                "PlateletsMin"
                "PotassiumMax"
                "PTMax "
                "PTTMax"
                "PupilsFixed"
                "SystolicBloodPressureMin"
                "TemperatureMin"
                "TemperatureMax"
                "BicarbonateMin"
                "BicarbonateMax"
                "UreaMax"
                "WhiteBloodCountMin"
                "PRISM3Neuro"
                "PRISM3Score"
                "PRISM4Mortality"
                "Status"
                "Sepsis"
                "Diagnosis1"
                "Diagnosis2"
                "DiagnosesOther"
                "Specialism"
            ]
            |> List.singleton

        pats
        |> Result.valueOrDefault (fun _ -> [||])
        |> Array.toList
        |> List.collect (fun pat ->
            pat.HospitalAdmissions
            |> List.collect (fun ha ->
                ha.PICUAdmissions
                |> List.filter (fun pa ->
                    pa.AdmissionDate.IsSome &&
                    pa.DischargeDate.IsSome
                )
                |> List.distinctBy (fun pa -> 
                    pa.HospitalNumber.Trim(), 
                    pa.AdmissionDate.Value
                )
                |> List.map (fun pa ->
                    let optPRISM p =
                        match p with
                        | None -> []
                        | Some prism ->
                            [
                                prism.AdmissionSource |> string
                                prism.Cancer |> string
                                prism.CPR24HourBefore |> string
                                prism.CreatinineMax |> optToString
                                prism.GlucoseMax |> optToString
                                prism.HeartRateMax |> Option.map float |> optToString
                                prism.LowRiskPrimary |> string
                                prism.MentalStatus |> Option.map float |> optToString
                                prism.PaO2Min |> optToString
                                prism.PCO2Max |> optToString
                                prism.PHMin |> optToString
                                prism.PHMax |> optToString
                                prism.PlateletsMin |> optToString
                                prism.PotassiumMax |> optToString
                                prism.PTMax  |> optToString
                                prism.PTTMax |> optToString
                                prism.PupilsFixed |> Option.map float |> optToString
                                prism.SystolicBloodPressureMin |> optToString
                                prism.TemperatureMin |> optToString
                                prism.TemperatureMax |> optToString
                                prism.BicarbonateMin |> optToString
                                prism.BicarbonateMax |> optToString
                                prism.UreaMax |> optToString
                                prism.WhiteBloodCountMin |> optToString
                                prism.PRISM3Neuro |> Option.map float |> optToString
                                prism.PRISM3Score |> Option.map float |> optToString
                                prism.PRISM4Mortality |> optToString
                            ]


                    [
                        pat.HospitalNumber
                        pa.AdmissionDate.Value |> string
                        pa.DischargeDate.Value |> string
                        optDateDiff pa.AdmissionDate pat.BirthDate
                        pa.PIM.RiskDiagnosis |> List.map string |> String.concat ", "
                        pa.PIM.Urgency |> string
                        pa.PIM.Recovery |> string
                        pa.PIM.Ventilated |> string
                        pa.PIM.AdmissionPupils |> PIM.pupilsToString
                        pa.PIM.SystolicBloodPressure |> optToString
                        pa.PIM.BaseExcess |> optToString
                        pa.PIM.FiO2 |> optToString
                        pa.PIM.PaO2 |> optToString
                        pa.PIM.PIM2Score |> optToString
                        pa.PIM.PIM2Mortality |> optToString
                        pa.PIM.PIM3Score |> optToString
                        pa.PIM.PIM3Mortality |> optToString
                        match pa.PRISM4, pa.PRISM12, pa.PRISM24 with
                        | Some prism, _, _
                        | None, Some prism, _ 
                        | None, None, Some prism -> yield! (prism |> Some |> optPRISM)
                        | _ -> ()
                        getPatientState pa pat
                        pa.Sepsis |> string
                        pa.PrimaryDiagnosis |> List.map (fun d -> d.Name) |> String.concat ";"
                        pa.SecondaryDiagnosis |> List.map (fun d -> d.Name) |> String.concat ";"
                        pa.Diagnoses |> List.map (fun d -> d.Name) |> String.concat ";"
                        pa.ReferingSpecialism |> Option.map (fun rs -> rs.Label) |> Option.defaultValue ""
                    ]
                )
            )
        )
        |> List.append headers


    let exportToDatabase (pats: Result<Patient[] * string[], _>) =
        pats
        |> function
        | Error _ -> ()
        | Ok _ ->
            let data = 
                pats 
                |> export
                |> List.distinctBy (fun x -> x.[0], x.[1])
        
            let headers =
                let stringCols =
                    [
                        "RiskDiagnoses"; "Urgency";
                        "Recovery"; "Ventilated"; "AdmissionPupils";
                        "AdmissionSource"; "Cancer"; "CPR24HourBefore";
                        "MentalStatus";
                        "PupilsFixed"; "Status"; "Sepsis"; "Diagnosis1";
                        "Diagnosis2"; "DiagnosesOther";
                        "Specialism"
                    ]
        
                let toColumn s =
                    match s with
                    | _ when s = "HospitalNumber" ->
                        { Table.Name = s; Table.Type = Table.VarChar(100); Table.IsKey = true }
                    | _ when s = "AdmissionDate" ->
                        { Table.Name = s; Table.Type = Table.DateTime; Table.IsKey = true }
                    | _ when s = "DischargeDate" ->
                        { Table.Name = s; Table.Type = Table.DateTime; Table.IsKey = false }
                    | _ when stringCols |> List.exists ((=) s) ->
                        { Table.Name = s; Table.Type = Table.VarChar(255); Table.IsKey = false }
                    | _ ->
                        { Table.Name = s; Table.Type = Table.Float; Table.IsKey = false }
        
                data
                |> List.head
                |> List.map toColumn
        
        
            let db = Database.create serverName databaseName
        
        
            headers
            |> Table.createTable db.ConnectionString tableName
            |> ignore
        
        
            let createDataTable (headers : Table.Column list) data =
                let addData (table : DataTable) =
                    data
                    |> List.fold (fun (tbl : DataTable) row ->
                        let xs = List.map2 Table.boxColumn headers row
                        tbl.Rows.Add(xs |> List.toArray) |> ignore
                        tbl
                    ) table
        
                use tbl = new DataTable ()
                headers
                |> List.fold (fun (tbl: DataTable) h ->
                    use col = new DataColumn()
                    col.AllowDBNull <- h.IsKey |> not
                    col.ColumnName <- h.Name
                    col.DataType <- h.Type |> Table.columnTypeToType
                    tbl.Columns.Add(col)
                    tbl
                ) tbl
                |> addData
        
        
            createDataTable headers (data |> List.skip 1)
            |> fun tbl ->
                use conn = new SqlConnection(db.ConnectionString)
                use bulk = new SqlBulkCopy(conn)
                conn.Open()
                bulk.BatchSize <- 5000
                bulk.DestinationTableName <- tableName
                bulk.WriteToServer(tbl, DataRowState.Added)
        
        
        
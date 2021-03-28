﻿namespace Informedica.PICE.Lib

open System
open System.IO

open Microsoft.Data.SqlClient


[<RequireQualifiedAccessAttribute>]
module Path =

    let getDirectoryName (path : string) = Path.GetDirectoryName(path)


[<RequireQualifiedAccessAttribute>]
module Console =


    type MessageType = | Info | Warning | Error

    let writeLine mt (s : string) =
        Console.ForegroundColor <-
            match mt with
            | Info -> ConsoleColor.Blue
            | Warning -> ConsoleColor.Yellow
            | Error -> ConsoleColor.Red
        Console.WriteLine(s)
        Console.ForegroundColor <- ConsoleColor.White

    let writeLineInfo = writeLine Info
    let writeLineWarning = writeLine Warning
    let writeLineError = writeLine Error


[<RequireQualifiedAccess>]
module SqlConnectionStringBuilder =

    let tryCreate connString =
        try
            SqlConnectionStringBuilder(connString)
            |> Some
        with
        | e ->
            printfn $"cannot create the connection string builder:\n{e.Message}"
            None


    let defaultBuilder () =
        SqlConnectionStringBuilder(@"Data Source=.;Initial Catalog=master;Persist Security Info=True;Integrated Security=SSPI;")


[<RequireQualifiedAccessAttribute>]
module SqlCommand =

    let executeNonQuery connString cmdText =
        try
            use conn = new SqlConnection(connString)
            use cmd = new SqlCommand(cmdText, conn)
            conn.Open()
            cmd.ExecuteNonQuery() |> ignore
            true
        with
        | e ->
            $"Could not execute: {cmdText}\nWith exception:\n{e.Message}"
            |> printfn "%s"
            false


type Database  =
    {
        ConnectionString : string
        Created : bool
        Name : string
        Server : string
        OpenConnection : unit -> SqlConnection
    }


[<RequireQualifiedAccessAttribute>]
module Database =


    let systemDbs = [ "master"; "tempdb"; "model"; "msdb" ]


    let canPingDatabase connString =
        try
            let builder = SqlConnectionStringBuilder(connString)
            // make sure it doesn't take for ever
            builder.CommandTimeout <- 1
            builder.ConnectTimeout <- 1
            // try the connection string
            use conn = new SqlConnection(builder.ConnectionString)
            conn.Open()
            use cmd = new SqlCommand("SELECT GETDATE()", conn)
            cmd.ExecuteScalar() |> ignore
            true
        with
        | e ->
            $"Cannot ping database at: {connString}"
            |> Console.writeLineWarning
            $"With error:\n{e.Message}"
            |> Console.writeLineError
            false


    let getMasterConnectionString connString =
        connString
        |> SqlConnectionStringBuilder.tryCreate
        |> Option.map (fun bldr -> bldr.InitialCatalog <- "master"; bldr.ConnectionString)
        |> Option.get


    let databaseExists connString =
        let dbExists name  (reader: SqlDataReader) =
            // default database names to be case insensitive
            let name = name |> String.toLower
            let rec exists b acc =
                if b |> not || acc then acc
                else
                    let x =
                        reader.GetString(0)
                        |> String.toLower
                    x = name
                    |> exists (reader.Read())
            exists (reader.Read()) false

        connString
        |> SqlConnectionStringBuilder.tryCreate
        |> function
        | None        ->
            $"Couldn't create a connection string with {connString}"
            |> Console.writeLineWarning
            false
        | Some builder ->
            let dbName = builder.InitialCatalog
            builder.InitialCatalog <- "master"
            if builder.ConnectionString |> canPingDatabase |> not then false
            else
                if systemDbs |> List.exists (String.toLower >> ((=) (dbName |> String.toLower))) then
                    $"The database is a system database: {dbName}"
                    |> Console.writeLineWarning
                    true
                else
                    // make sure it doesn't wait forever
                    builder.ConnectTimeout <- 30
                    builder.CommandTimeout <- 30
                    // create the connection
                    use conn = new SqlConnection(builder.ConnectionString)
                    conn.Open()
                    // create the command
                    let inList = systemDbs |> List.map (sprintf "'%s'") |> String.concat ", "
                    let cmdText = $"SELECT NAME FROM sys.databases WHERE NAME NOT IN ({inList});"
                    use cmd = new SqlCommand(cmdText, conn)
                    // create the reader to check whether the database exists
                    use reader = cmd.ExecuteReader()
                    reader |> dbExists dbName


    let createDatabaseIfDoesNotExist (connString : string) =
        connString
        |> SqlConnectionStringBuilder.tryCreate
        |> function
        | None ->
            $"Cannot create datebase with connection: {connString}"
            |> Console.writeLineWarning
            false
        | Some builder ->
            if builder.ConnectionString |> databaseExists then
                $"Database: {builder.InitialCatalog} already exists"
                |> Console.writeLineInfo
                true
            else
                let dbName = builder.InitialCatalog
                builder.InitialCatalog <- "master"

                let cmdText =
                    $"CREATE DATABASE {dbName}"
                if SqlCommand.executeNonQuery builder.ConnectionString cmdText then
                    $"Created {dbName} on server {builder.DataSource}"
                    |> Console.writeLineInfo
                    true
                else
                    $"Create database {dbName} using command:\n{cmdText}\ndid not succeed"
                    |> Console.writeLineWarning
                    false


    let create server name =
        let connString =
            let bldr = SqlConnectionStringBuilder.defaultBuilder ()
            bldr.DataSource <- server
            bldr.InitialCatalog <- name
            bldr.ConnectionString
        {
            ConnectionString = connString
            Created = createDatabaseIfDoesNotExist connString
            Name = name
            Server = server
            OpenConnection =
                fun () ->
                    new SqlConnection(connString)

        }


    let listTables (db : Database) =
        let cmdText = $"""SELECT TABLE_NAME 
        FROM [{db.Name}].INFORMATION_SCHEMA.TABLES 
        WHERE TABLE_TYPE = 'BASE TABLE'"""
        use conn = new SqlConnection(db.ConnectionString)
        conn.Open()
        use cmd = new SqlCommand(cmdText, conn)
        use reader = cmd.ExecuteReader()

        let rec read (reader: SqlDataReader) acc =
            if reader.Read () |> not then acc
            else
                reader.GetString(0)::acc
                |> read reader

        read reader []


[<RequireQualifiedAccessAttribute>]
module Table =

    type Column =
        {
            Name : string
            Type : ColumnType
            IsKey : bool
        }
    and ColumnType =
    | VarChar of int
    | VarCharMax
    | NVarChar of int
    | NVarCharMax
    | Text
    | NText
    | Bit
    | Int
    | BigInt
    | Decimal of leftDigits: int * rightDigits: int
    | Float
    | Date
    | DateTime
    | Time

    let columnTypeToString = function
        | VarChar n -> $"varchar({n})"
        | VarCharMax -> "varchar(max)"
        | NVarChar n -> $"nvarchar({n})"
        | NVarCharMax -> "nvarchar(max)"
        | Text -> "text"
        | NText -> "ntext"
        | Bit -> "bit"
        | Int -> "int"
        | BigInt -> "bigint"
        | Decimal(l, r) -> $"decimal({l}, {r})"
        | Float -> "float"
        | Date -> "date"
        | DateTime -> "datetime"
        | Time -> "time"


    let columnTypeToType = function
        | VarChar _
        | VarCharMax
        | NVarChar _
        | NVarCharMax
        | Text
        | NText -> typeof<String>
        | Bit -> typeof<Boolean>
        | Int -> typeof<Int32>
        | BigInt -> typeof<Int64>
        | Decimal(l, r) -> typeof<Decimal>
        | Float -> typeof<Double>
        | Date
        | DateTime
        | Time -> typeof<DateTime>


    let boxColumn (col: Column) (s : string) =
        match col.Type with
        | VarChar _
        | VarCharMax
        | NVarChar _
        | NVarCharMax
        | Text
        | NText -> s |> box
        | Bit ->
            match Boolean.TryParse(s) with
            | true, b  -> b |> box
            | false, _ -> null
        | Int ->
            match Int32.TryParse(s) with
            | true, i  -> i |> box
            | false, _ -> null
        | BigInt ->
            match Int64.TryParse(s) with
            | true, i  -> i |> box
            | false, _ -> null
        | Decimal(l, r) ->
            let s = s |> String.replace "," "."
            match Decimal.TryParse(s) with
            | true, d  -> d |> box
            | false, _ -> null
        | Float ->
            let s = s |> String.replace "," "."
            match Double.TryParse(s) with
            | true, d  -> d |> box
            | false, _ -> null
        | Date
        | DateTime
        | Time ->
            match DateTime.TryParse(s) with
            | true, dt when dt >= (new DateTime(2000, 1, 1)) && 
                            dt <= DateTime.Now -> dt |> box
            | _ -> null


    let columnToString (col : Column) =
        let key =
            if col.IsKey |> not then "NULL"
            else "NOT NULL"
        $"{col.Name} {col.Type |> columnTypeToString} {key}"


    let createTable connString name columns =
        let pk =
            columns
            |> List.filter (fun col -> col.IsKey)
            |> function
            | [] -> ""
            | cs ->
                let keys = cs |> List.map (fun c -> c.Name) |> String.concat ", "
                $"CONSTRAINT PK_{name} PRIMARY KEY ({keys})"
        let cols =
            columns
            |> List.map columnToString
            |> String.concat ", "
            |> fun s ->
                if pk = "" then s
                else $"{s}, {pk}"
        $"CREATE TABLE {name} ({cols})"
        |> SqlCommand.executeNonQuery connString


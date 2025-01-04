module Api


open Shared


/// Defines how routes are generated on server and mapped from client
let routerPaths typeName method = $"/api/%s{typeName}/%s{method}"


/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IServerApi =
    {
        SayHello: unit -> Async<Result<string, string>>
        GetReport: Filter -> Async<Result<Report, string>>
        GetScoresCSV: string list -> Async<Result<string, string>>
    }

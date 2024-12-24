open Fake.Core
open Fake.IO

open Helpers

initializeContext ()

let sln = "PICEDashboard.sln"

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let deployPath = Path.getFullName "deploy"
let sharedTestsPath = Path.getFullName "tests/Shared"
let serverTestsPath = Path.getFullName "tests/Server"
let clientTestsPath = Path.getFullName "tests/Client"


Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] clientPath // Delete *.fs.js files created by Fable
)


Target.create "RestoreClientDependencies" (fun _ -> run npm [ "ci" ] clientPath)


Target.create "Bundle" (fun _ ->
    [
        "server", dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] serverPath
        "client", npm [ "run"; "build" ] clientPath
    ]
    |> runParallel

    Shell.cp_r (Path.combine clientPath "dist") deployPath)


Target.create "Build" (fun _ -> run dotnet [ "build"; sln ] ".")


Target.create "Run" (fun _ ->
    [
        "server", dotnet [ "run"; "--no-restore" ] serverPath
        "client", npm [ "run"; "dev" ] clientPath
    ]
    |> runParallel)


Target.create "RunTestsHeadless" (fun _ ->
    run dotnet [ "run" ] serverTestsPath
    run dotnet [ "fable"; "-o"; "output" ] clientTestsPath
//    run npx [ "mocha"; "output" ] clientTestsPath
)

Target.create "WatchRunTests" (fun _ ->
    [
        "server", dotnet [ "watch"; "run"; "--no-restore" ] serverTestsPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] clientTestsPath
    ]
    |> runParallel)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")


open Fake.Core.TargetOperators


let dependencies =
    [
        "Clean" ==> "RestoreClientDependencies" ==> "Bundle"
        "Clean" ==> "RestoreClientDependencies" ==> "Build" ==> "Run"

        "RestoreClientDependencies" ==> "Build" ==> "RunTestsHeadless"
        "RestoreClientDependencies" ==> "Build" ==> "WatchRunTests"
    ]


[<EntryPoint>]
let main args = runOrDefault args

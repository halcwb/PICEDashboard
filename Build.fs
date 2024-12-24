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


Target.create "clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] clientPath // Delete *.fs.js files created by Fable
)


Target.create "restoreclient" (fun _ -> run npm [ "ci" ] clientPath)


Target.create "bundle" (fun _ ->
    [
        "server", dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] serverPath
        "client", dotnet [ "fable"; "-o"; "output"; "-s"; "-e"; ".jsx"; "--run"; "npx"; "vite"; "build"; "--emptyOutDir" ] clientPath
    ]
    |> runParallel)


Target.create "build" (fun _ -> run dotnet [ "build"; sln ] ".")


Target.create "run" (fun _ ->
    [
        "server", dotnet [ "run"; "--no-restore" ] serverPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "-e"; ".jsx"; "--run"; "npx"; "vite" ] clientPath
    ]
    |> runParallel)


Target.create "testheadless" (fun _ ->
    run dotnet [ "run" ] serverTestsPath
    run dotnet [ "fable"; "-o"; "output"; "-e"; ".jsx" ] clientTestsPath
//    run npx [ "mocha"; "output" ] clientTestsPath
)

Target.create "watchtests" (fun _ ->
    [
        "server", dotnet [ "watch"; "run"; "--no-restore" ] serverTestsPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "-e"; ".jsx"; "--run"; "npx"; "vite" ] clientTestsPath
    ]
    |> runParallel)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")


open Fake.Core.TargetOperators


let dependencies =
    [
        "clean" ==> "restoreclient" ==> "bundle"
        "clean" ==> "restoreclient" ==> "build" ==> "run"

        "restoreclient" ==> "build" ==> "testheadless"
        "restoreclient" ==> "build" ==> "watchtests"
    ]


[<EntryPoint>]
let main args = runOrDefault args

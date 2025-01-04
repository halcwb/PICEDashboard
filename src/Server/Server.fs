module Server

open System
open Giraffe
open Saturn
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Serilog


let tryGetEnv key =
    match Environment.GetEnvironmentVariable key with
    | x when String.IsNullOrWhiteSpace x -> None
    | x -> Some x


let port =
    "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let webApi =
    Remoting.createApi ()
    |> Remoting.fromContext (fun (ctx : HttpContext) -> ctx.GetService<ServerApi.ServerApi>().Build())
    |> Remoting.withRouteBuilder Api.routerPaths
    |> Remoting.buildHttpHandler


let webApp =
    choose [ webApi; GET >=> text "PICE Dashboard App. Use localhost: 8080 for the GUI" ]

let serviceConfig (services: IServiceCollection) =
    services
        .AddSingleton<ServerApi.ServerApi>()
        .AddSerilog(fun loggerConfiguration ->
            loggerConfiguration
                .MinimumLevel.Debug()
                .Enrich.FromLogContext()
                .WriteTo.Console()
            |> ignore
        )

let application =
    application {
        url ("http://*:" + port.ToString() + "/")
        use_mime_types [ ".svg", "image/svg+xml"; ".png", "image/png" ]
        use_static "public" //publicPath
        use_router webApp
        memory_cache
        use_gzip
        //use_iis

        service_config serviceConfig
        host_config Env.configureHost
    }


System.Console.WriteLine("Starting server on port " + port.ToString())
run application

namespace Informedica.PICE.Server

module Server =

    open System
    open System.IO
    open Saturn
    open Giraffe
    open ServerApi
    open Fable.Remoting.Server
    open Fable.Remoting.Giraffe
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.AspNetCore.Http

    open Informedica.PICE.Shared.Api

    Text.Encoding.RegisterProvider(Text.CodePagesEncodingProvider.Instance)

    let tryGetEnv key = 
        match Environment.GetEnvironmentVariable key with
        | x when String.IsNullOrWhiteSpace x -> None 
        | x -> Some x


    let port =
        "SERVER_PORT"
        |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


    let publicPath = Path.GetFullPath "../Client/public"


    let webApi =
        Remoting.createApi()
        |> Remoting.fromContext (fun (ctx : HttpContext) -> ctx.GetService<ServerApi>().Build())
        |> Remoting.withRouteBuilder routerPaths
        |> Remoting.buildHttpHandler

    let webApp = choose [ webApi; GET >=> text "PICE Dashboard app. Use localhost: 8080 for the GUI" ]

    let serviceConfig (services: IServiceCollection) =
        services
          .AddSingleton<ServerApi>()
          .AddLogging()
      

    let application = application {
        url ("http://0.0.0.0:" + port.ToString() + "/")
        use_router webApp
        use_static publicPath
        use_gzip
        use_iis
    
        service_config serviceConfig
        webhost_config Env.configureHost
    }

    run application
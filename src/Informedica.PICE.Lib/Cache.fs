namespace Informedica.PICE.Lib

module Cache =

    open System.IO
    open Newtonsoft.Json

    ///
    let serialize x =
        JsonConvert.SerializeObject(x)

    let deSerialize<'T> (s: string) =
        JsonConvert.DeserializeObject<'T>(s)

    let cache p o =
        o
        |> serialize
        |> File.writeTextToFile p

    let clearCache fs = fs |> List.iter File.Delete

    let getCache<'T> p =
        try
            printfn "Reading cache: %s" p
            File.readAllLines p
            |> String.concat ""
            |> deSerialize<'T>
            |> Some
        with
        | _ -> None

namespace Informedica.PICE.Lib


/// Utility functions to apply memoization
module Memoization =

    open System.Collections.Generic
    
    /// Memoize a function `f` according
    /// to its parameter
    let memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some r -> r
            | None ->
                let r = f x
                cache := (!cache).Add(x, r)
                r

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

    let private getCache_<'T> p =
        try
            printfn "Reading cache: %s" p
            File.readAllLines p
            |> String.concat ""
            |> deSerialize<'T>
            |> Some
        with
        | _ -> None

    let getCache<'T> = Memoization.memoize getCache_<'T>
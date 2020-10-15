namespace Informedica.PICE.Lib

module Result =

    let ok x = Result.Ok (x, [||])

    let okMsg x msgs = Result.Ok (x, msgs)

    let error msgs = Result.Error msgs

    let tryWith f x =
        try
            f x
            |> ok
        with
        | e ->
            [ e.Message ]
            |> error


    let tryWithOk f x =
        try
            Some (f x)
            |> ok
        with
        | e ->
            [| e.Message |]
            |> okMsg None


    /// A function that applies either fSuccess or fFailure 
    /// depending on the case.
    let either fOk fErr = function
        | Result.Ok (x, msgs) -> fOk (x, msgs) 
        | Result.Error msgs -> fErr msgs 

    /// merge messages with a result
    let mergeMessages msgs result =
        let fOk (x, msgs2) = 
            msgs @ msgs2 |> okMsg x 
        let fErr errs = 
            error (errs @ msgs) 
        either fOk fErr result

    /// given a function that generates a new RopResult
    /// apply it only if the result is on the Success branch
    /// merge any existing messages with the new result
    let bindR f result =
        let fOk (x,msgs) = 
            f x |> mergeMessages msgs
        let fErr errs = 
            Result.Error errs 
        either fOk fErr result

    let bindL result f = bindR f result

    /// given a function wrapped in a result
    /// and a value wrapped in a result
    /// apply the function to the value only if both are Success
    let applyR f result =
        match f, result with
        | Result.Ok (f, msgs1), Result.Ok (x, msgs2) -> 
            msgs1 |> Array.append msgs2 |> okMsg (f x)
        | Result.Error msgs1,    Result.Ok (_, msgs2) 
        | Result.Ok (_, msgs2),  Result.Error msgs1 -> 
            msgs1 |> Array.append msgs2 |> error
        | Result.Error msgs1,   Result.Error msgs2 -> 
            msgs1 |> Array.append msgs2 |> error 

    /// given a function that transforms a value
    /// apply it only if the result is on the Success branch
    let liftR f result =
        let f' =  f |> ok
        applyR f' result 

    /// given two values wrapped in results apply a function to both
    let lift2R f result1 result2 =
        let f' = liftR f result1
        applyR f' result2 

    /// given three values wrapped in results apply a function to all
    let lift3R f result1 result2 result3 =
        let f' = lift2R f result1 result2 
        applyR f' result3

    /// given four values wrapped in results apply a function to all
    let lift4R f result1 result2 result3 result4 =
        let f' = lift3R f result1 result2 result3 
        applyR f' result4


    /// synonym for liftR
    let mapR = liftR


    /// given an RopResult, call a unit function on the success branch
    /// and pass thru the result
    let okTee f result = 
        let fOk (x, msgs) = 
            f (x, msgs)
            okMsg x msgs 
        let fErr msgs = msgs |> error
        either fOk fErr result

    /// given an RopResult, call a unit function on the failure branch
    /// and pass thru the result
    let errorTee f result = 
        let fOk (x, msgs) = msgs |> okMsg x 
        let fErr msgs = 
            f msgs
            msgs |> error
        either fOk fErr result

    /// given an RopResult, map the messages to a different error type
    let mapMessagesR f result = 
        match result with 
        | Result.Ok (x,  msgs) -> 
            List.map f msgs
            |> okMsg x
        | Result.Error msgs -> 
            List.map f msgs 
            |> error

    /// given an RopResult, in the success case, return the value.
    /// In the failure case, determine the value to return by 
    /// applying a function to the errors in the failure case
    let valueOrDefault f result = 
        match result with 
        | Result.Ok (x,_) -> x
        | Result.Error msgs -> f msgs

    /// lift an option to a RopResult.
    /// Return Success if Some
    /// or the given message if None
    let errIfNone msgs = function
        | Some x -> ok x
        | None -> msgs |> error 

    /// given an RopResult option, return it
    /// or the given message if None
    let errorIfNoneR msgs = function
        | Some result -> result
        | None -> error msgs 


    let okIfNone msgs = function
        | Some x -> ok (Some x)
        | None -> msgs |> okMsg None 


    let resultIs bf = function 
        | Result.Error _ -> bf |> not
        | Result.Ok _ -> bf


    let isError r = resultIs false r
    let isOk r = resultIs true r


    let getMessages = function
        | Result.Ok(_, msgs) | Result.Error msgs -> msgs


    let foldOk results =
        results
        |> Array.fold (fun acc result ->
            match result with
            | Result.Ok (x, msgs1) ->
                match acc with
                | Result.Ok (xs, msgs2) -> okMsg ([|x|] |> Array.append xs) (msgs1 |> Array.append msgs2)
                | Result.Error _ -> acc
            | Result.Error msgs1 ->
                match acc with
                | Result.Ok (xs, msgs2) -> okMsg xs (msgs1 |> Array.append msgs2)
                | Result.Error _ -> acc
        ) (okMsg [||] [||])


    module Operators =

        /// infix version of bindL
        let (>>=) = bindL

        let (<<=) = bindR

        /// infix version of apply
        let (<*>) = applyR

        /// infix version of liftR
        let (<!>) = liftR

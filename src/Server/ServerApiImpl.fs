module ServerApiImpl


let serverApi: Api.IServerApi =
    {
        test =
            fun () ->
                printfn "Running Hello World test"
                async { return "Hello World" }
    }

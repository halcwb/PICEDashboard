namespace Components


open Fable.Core
open Fable.Core.JsInterop


module Markdown =


    [<Import("getOverrides", from = "mui-markdown")>]
    let private getOverrides () = emitJsExpr () "getOverrides()"

    [<Emit "Object.assign({}, $0, $1)">]
    let private objectAssign (x: obj) (y: obj) : obj = jsNative

    [<Import("Typography", from = "@mui/material")>]
    let private Typography: obj = jsNative


    [<JSX.Component>]
    let View (text: {| md: string |}) =
        let headerSx =
            {|
                fontWeight = "bold"
                color = "primary.main"
                marginTop = 2
                marginBottom = 1
            |}

        let h1 =
            {|
                ``component`` = Typography
                props =
                    {|
                        sx = {| headerSx with fontSize = 24 |}
                    |}
            |}

        let h2 =
            {|
                ``component`` = Typography
                props =
                    {|
                        sx = {| headerSx with fontSize = 20 |}
                    |}
            |}

        let h3 =
            {|
                ``component`` = Typography
                props =
                    {|
                        sx = {| headerSx with fontSize = 18 |}
                    |}
            |}

        let h4 =
            {|
                ``component`` = Typography
                props =
                    {|
                        sx = {| headerSx with fontSize = 16 |}
                    |}
            |}

        let h5 =
            {|
                ``component`` = Typography
                props =
                    {|
                        sx = {| headerSx with fontSize = 14 |}
                    |}
            |}

        let h6 =
            {|
                ``component`` = Typography
                props =
                    {|
                        sx = {| headerSx with fontSize = 12 |}
                    |}
            |}

        let p =
            {|
                ``component`` = Typography
                props =
                    {|
                        fontSize = 12
                        paddingTop = 2
                        paddingBottom = 2
                    |}
            |}

        let a =
            {|
                ``component`` = Typography
                props = {| fontSize = 12 |}
            |}

        let overrides =
            {|
                h1 = h1
                h2 = h2
                h3 = h3
                h4 = h4
                h5 = h5
                h6 = h6
                p = p
                a = a
            |}

        // merge the overrides with the default overrides
        let overrides = objectAssign (getOverrides ()) overrides

        let options =
            {|
                disableParsingRawHTML = true
                overrides = overrides
            |}

        let md = text.md :> obj // temp fix for: https://github.com/fable-compiler/Fable/issues/3999

        JSX.jsx
            $"""
            import React from 'react';
            import {{ MuiMarkdown }} from 'mui-markdown';
            
            <MuiMarkdown 
                options={options}
                >
                {md}
            </MuiMarkdown>
            """

namespace Pages


module Statistics =

    open System

    open Elmish
    open Feliz
    open Feliz.UseElmish
    open Feliz.MaterialUI
    open Fable.MaterialUI.Icons
    open Fable.Core.JsInterop
    open Feliz.Markdown
    
    open Informedica.PICE.Shared

    open Utils

    let useStyles = Styles.makeStyles(fun styles theme ->
        {|
            print = styles.create [
                style.marginBottom (theme.spacing 2)
                style.paddingTop (theme.spacing 1)
                style.color (theme.palette.primary.dark)
            ]

            form = styles.create [
            ]

            div = styles.create [
                style.marginBottom (theme.spacing 1)
            ]
        |}
    )


    let private comp =
        React.functionComponent("statistics", fun (props : {| stats : Types.Statistics |}) ->
            // let model, dispatch = React.useElmish(init, update, [||])

            let classes = useStyles ()

            
            Markdown.markdown [
              markdown.source props.stats.Html
              markdown.escapeHtml false
              //markdown.renderers [
              //  markdown.renderers.paragraph (fun props ->
              //    Mui.typography [
              //      typography.paragraph true
              //      typography.children props.children
              //    ]
              //  )
              //  markdown.renderers.link (fun props ->
              //    Mui.link [
              //      prop.href props.href
              //      link.children props.children
              //    ]
              //  )
              //  markdown.renderers.heading (fun props ->
              //    Mui.typography [
              //      match props.level with
              //      | 1 -> typography.variant.h1
              //      | 2 -> typography.variant.h2
              //      | 3 -> typography.variant.h3
              //      | 4 -> typography.variant.h4
              //      | 5 -> typography.variant.h5
              //      | 6 -> typography.variant.h6
              //      | _ -> ()
              //      typography.paragraph true
              //      typography.children props.children
              //    ]
              //  )
              //]
            ]        
        )


    let render stats = comp ({| stats = stats |})
module App

open Sutil
open Sutil.DOM
open Sutil.Html
open Fable.Core
open Fable.Core.JsInterop

// TODO
let useRoutes (routes : obj) : SutilElement option =
    let f : unit -> SutilElement = routes?``/``
    Some (f())

let app() =
    let routes = {|
        ``/`` = fun _ -> InboxPage.page()
        ``/Inbox`` = fun _ -> InboxPage.page()
        ``/Users`` = fun _ -> UsersPage.page()
    |}

    let content =
        useRoutes routes
        |> Option.defaultValue (Html.h1 [text "Not Found"])

    Html.div [
        Attr.className "container"
        Html.div [
            Attr.className "row"
            Html.div [
                Attr.className "col"
                content
            ]
        ]
    ]

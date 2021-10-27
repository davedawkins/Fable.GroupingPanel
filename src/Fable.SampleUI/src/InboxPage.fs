module InboxPage

open Fable
open GroupingPanel
open System
open Sutil
open Sutil.Html
open Sutil.DOM
open type Feliz.length
type Email = {
    From: string
    Subject: string
    Received: DateTime
}

let email from subject rcvd =
    { From = from; Subject = subject; Received = rcvd }

let today = DateTime.Today
let day (days: int) = DateTime.Today.AddDays(float days)

let getEmails() =
    [ email "Jackson Blevins" "whuut?!" (day -2)
      email "Jason Stafford" "Training Deadline" (day -2)
      email "Cameron Clay" "RE: App registrations..." (day -1)
      email "Jackson Blevin" "Incoming memes!" (day -1)
      email "Jackson Blevin" "Funny cat stuff" (day -1)
      email "Lamar Shaw" "Meeting Request" today
      email "Xander Cruz" "RE: Next Set of Features" today ]
    |> List.sortBy (fun e -> e.Received)


let page() =
    Html.div [
        Attr.className "container"
        Html.div [
            Attr.className "row"
            Html.div [
                Attr.className "col-3"
                Attr.style [Css.backgroundColor "#ececec"]
                Html.table [
                    Attr.className "table"
                    Html.tbody [
                        groupingPanel {
                            for email in getEmails() do
                            groupBy (
                                match email.Received with
                                | r when r = today  -> "Today"
                                | r when r >= (day -1) -> "Yesterday"
                                | _ -> "Older"
                            )
                            groupSortByDescending (email.Received)
                            groupHeader (fun header ->
                                Html.tr [
                                    Ev.onClick header.ToggleOnClick
                                    Html.td [
                                        Attr.style [ Css.width (px 10) ]
                                        header.Chevron
                                    ]
                                    Html.td [text header.GroupKey]
                                ]
                            )
                            groupCollapsedIf (email.Received < today)
                            select (
                                fun group email ->
                                    Html.tr [
                                        Bind.className (group.Expanded .> function true -> "expanded"|false -> "collapsed")
                                        Html.td [ ]
                                        Html.td [
                                            Html.div [ // Wrapper div that lets us control max-height of the td
                                                Html.div [text email.From]
                                                Html.div [
                                                    Attr.style [Css.fontWeight 700]
                                                    text email.Subject]
                                                Html.div [text (email.Received.ToShortDateString())]
                                            ]
                                        ]
                                    ]
                            )
                        }
                    ]
                ]
            ]
            Html.div [
                Attr.classes ["col-9"; "p-2" ]
                Attr.style [Css.backgroundColor "whitesmoke"]
                text "Message Body..."
            ]
        ]
    ]

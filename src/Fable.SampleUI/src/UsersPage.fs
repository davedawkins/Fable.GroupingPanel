module UsersPage

open Fable
open GroupingPanel
open System
open Sutil
open Sutil.Html
open type Feliz.length

type User = {
    Username: string
    Email: string
    IsEnabled: bool
}

let usr nm eml enbld =
    { Username = nm; Email = eml; IsEnabled = enbld }

let getFilteredUsers() =
    [ usr "Peter Gibbons" "pgibbons@initech.com" true
      usr "Bill Lumbergh" "blumbergh@initech.com" true
      usr "Michael Scott" "mscott@dmifflin.com" true
      usr "Dwight Schrute" "dschrute@dmifflin.com" true
      usr "Angela Martin" "amartin@dmifflin.com" false
      usr "Pam Beesly" "pbeesly@dmifflin.com" false ]
    |> List.sortBy (fun u -> u.Username)

let getCompany user =
    match user.Email with
    | email when email.EndsWith("initech.com") -> "Initech"
    | email when email.EndsWith("dmifflin.com") -> "Dunder Mifflin"
    | _ -> "Other"

let page() =
    //let users, setUsers = React.useState([])

    //React.useEffectOnce(fun () ->
    //    getFilteredUsers() |> setUsers
    //)

    Html.div [
        Attr.className "container"
        Html.div [
            Attr.className "row"
            Html.div [
                Attr.className "col"
                Html.table [
                    Attr.classes ["table"; "mt-4"]
                    Html.tbody [

                        let headerTemplate header =
                            Html.tr [
                                Attr.style [
                                    Css.backgroundColor "#ececec"]
                                Ev.onClick header.ToggleOnClick
                                Html.td [
                                    Attr.colSpan 4
                                    header.Chevron
                                    Html.span [text (sprintf "%s (%i)" header.GroupKey header.Group.Length)]
                                ]
                            ]

                        groupingPanel {
                            for user in getFilteredUsers() do
                            groupBy (if user.IsEnabled then "Active Users" else "Inactive Users")
                            groupHeader headerTemplate
                            groupCollapsedIf (not user.IsEnabled)
                            groupBy (getCompany user)
                            groupHeader headerTemplate
                            select (
                                fun group user ->
                                    Html.tr [
                                        Attr.custom("Key","usr_" + user.Email)
                                        Html.td []
                                        Html.td [
                                            Attr.style [ Css.lineHeight (px 30) ]
                                            text user.Email
                                        ]
                                        Html.td [
                                            text user.Username
                                        ]
                                        Html.td [
                                            Html.input [
                                                Attr.type' "checkbox"
                                                Attr.style [Css.width (px 20); Css.height (px 32)]
                                                Attr.className "form-control"
                                                Attr.isChecked (user.IsEnabled)
                                                Ev.onChange (fun (e: bool) -> ())
                                                // Ev.onChange (fun e ->
                                                //     users |> List.map (fun u ->
                                                //         if u.Email = user.Email
                                                //         then { u with IsEnabled = not u.IsEnabled }
                                                //         else u
                                                //     ) |> ignore //setUsers
                                                // )
                                            ]
                                        ]
                                    ]
                            )
                        }
                    ]
                ]
            ]
        ]
    ]

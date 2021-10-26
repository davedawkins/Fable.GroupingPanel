module Fable.GroupingPanel

open Sutil
open Sutil.Html
open Sutil.DOM
open Sutil.Svg
open type Feliz.length;
open Sutil.Transition

type Props<'T, 'SortKey> = {
    /// A sequence of items in a group.
    Items: 'T seq
    /// This is set to a storage key if local storage option is enabled.
    LocalStorageKey: string option
    /// Defines one or more grouping levels.
    GroupingLevels: GroupingLevel<'T, 'SortKey> list
    /// Provides an React template to render each item in a group.
    ItemTemplate: 'T -> SutilElement
}

and GroupingLevel<'T, 'SortKey> = {
    /// A function that must provide a unique key for each group.
    KeySelector: 'T -> string
    /// Determines the default collapsed state of a node (can use a static bool or a predicate function).
    Collapsed: Collapsed<'T>
    /// Sorts group according to the selection.
    SortBy: SortBy<'T, 'SortKey>
    /// Creates a group header.
    HeaderTemplate: GroupInfo<'T> -> SutilElement
    /// Creates an optional group footer.
    FooterTemplate: (GroupInfo<'T> -> SutilElement) option
}

and SortBy<'T, 'SortKey> =
    | SortAsc of ('T -> 'SortKey)
    | SortDesc of ('T -> 'SortKey)
    | SortDefault

and Collapsed<'T> =
   | Collapse of bool
   | CollapseIf of ('T -> bool)

and GroupInfo<'T> = {
    GroupKey: string
    Group: 'T list
    FirstItem: 'T
    Chevron: SutilElement
    ToggleOnClick: Browser.Types.MouseEvent -> unit
}

module private Chevron =
    let icon d =
        Svg.svg [
            Attr.custom("viewBox","0 0 15 15")
            Attr.style [Css.width (px 16); Css.margin(px 2,px 8); Css.fill "rgb(63 132 213)"]
            svgel "path" [ Attr.custom("d", d) ]
        ]
    let right = icon "M 4.646 1.646 a 0.5 0.5 0 0 1 0.708 0 l 6 6 a 0.5 0.5 0 0 1 0 0.708 l -6 6 a 0.5 0.5 0 0 1 -0.708 -0.708 L 10.293 8 L 4.646 2.354 a 0.5 0.5 0 0 1 0 -0.708 Z"
    let down = icon "M 1.646 4.646 a 0.5 0.5 0 0 1 0.708 0 L 8 10.293 l 5.646 -5.647 a 0.5 0.5 0 0 1 0.708 0.708 l -6 6 a 0.5 0.5 0 0 1 -0.708 0 l -6 -6 a 0.5 0.5 0 0 1 0 -0.708 Z"

let mutable expandedMap : Map<string,IStore<bool>> = Map.empty

let getIsExpandedStore key =
    if not (expandedMap.ContainsKey key) then
        expandedMap <- expandedMap.Add(key, Store.make false)
    expandedMap.[key]

/// Component implementation
let private render<'T, 'SortKey when 'SortKey : comparison> (props: Props<'T, 'SortKey>) =

    let rec renderGroups (level: int, aggregateKey: string, items: 'T list) =
        let grpLvl = props.GroupingLevels.[level]

        let items =
            match grpLvl.SortBy with
            | SortDefault ->        items |> List.sortBy grpLvl.KeySelector
            | SortAsc selector ->   items |> List.sortBy selector
            | SortDesc selector ->  items |> List.sortByDescending selector

        items
        |> List.groupBy grpLvl.KeySelector
        |> List.map (fun (groupKey, group) ->

            let firstItem = group.[0]

            let aggregateKey = sprintf "%s > %s" aggregateKey groupKey

            let onClick (e: Browser.Types.MouseEvent) =
                e.stopPropagation()
                getIsExpandedStore aggregateKey |> Store.modify not

            let chevronButton =
                let style = Attr.style [Css.padding (px 0); Css.paddingLeft (px (25 * level)); Css.cursorPointer ]

                Bind.el (getIsExpandedStore aggregateKey, fun isExpanded ->
                    if isExpanded //getIsCollapsed(aggregateKey, firstItem, grpLvl)
                    then Html.span [Ev.onClick onClick; Attr.alt "Collapse Group"; style ; Chevron.down]
                    else Html.span [Ev.onClick onClick; Attr.alt "Expand Group"; style; Chevron.right])

            let groupInfo =
                { GroupKey = groupKey
                  Group = group
                  FirstItem = firstItem
                  Chevron = chevronButton
                  ToggleOnClick = onClick }

            let header = grpLvl.HeaderTemplate groupInfo

            let footer =
                match grpLvl.FooterTemplate with
                | Some tmpl -> tmpl groupInfo
                | None -> fragment []

            fragment [
                //FragmentProp.Key aggregateKey
                yield header

                yield (transition [InOut fade] (getIsExpandedStore aggregateKey) (fragment [
                    if props.GroupingLevels.Length > (level + 1) then
                        // Render next group
                        yield renderGroups(level + 1, aggregateKey, group)
                    else
                        // Render items
                        yield
                            group
                            |> Seq.filter (fun item -> groupKey = grpLvl.KeySelector item)
                            |> Seq.map props.ItemTemplate
                            |> fragment

                ]))

                yield footer
            ]
        )
        |> fragment

    if props.GroupingLevels.Length = 0 then failwith "GroupingPanel must have at least one 'groupBy' defined."
    renderGroups(0, "", props.Items |> Seq.toList)


// Creates a memoized _generic_ component (required for proper performance; else component will always re-render).
// https://github.com/fable-compiler/fable-react/issues/162
//let private makeComponent<'T, 'SortKey when 'SortKey : comparison>() =
//    FunctionComponent.Of(render<'T, 'SortKey>, memoizeWith=equalsButFunctions)

/// A 'tr' header template.
let headerRowTemplate (color: string) header =
    Html.tr [
        Ev.onClick header.ToggleOnClick
        Attr.style [Css.backgroundColor color]
        Html.td [
            header.Chevron
            Html.span [
                Attr.style [ Css.lineHeight (px 30) ]
                Html.text (sprintf "%s (%i)" header.GroupKey header.Group.Length)
            ]
        ]
    ]

/// A 'div' header template.
let headerDivTemplate (color: string) header =
    Html.div [
        Ev.onClick header.ToggleOnClick
        Attr.style [Css.backgroundColor color]
        header.Chevron
        Html.span [
            Attr.style[ Css.lineHeight (px 30)]
            Html.text (sprintf "%s (%i)" header.GroupKey header.Group.Length)]
    ]

type GroupingPanelBuilder() =
    let def =
        { Items = Seq.empty
          LocalStorageKey = None
          GroupingLevels = []
          ItemTemplate = fun item -> fragment [] }

    let getLastAndRest lst =
        match lst with
        | [] -> failwith "A 'groupBy' clause must be added before a 'group' modifier clause can be used."
        | _ -> (List.last lst), (List.take (lst.Length - 1) lst)

    /// Implements "for" loops.
    member this.For (sequence: seq<'T>, f: 'T -> Props<'T, 'SortKey>) =
        { def with Items = sequence }

    // Default props
    member this.Yield _ =
        def

    /// Stores the collapsed state in local storage.
    [<CustomOperation("localStorageKey", MaintainsVariableSpace=true)>]
    member this.LocalStorageKey (props, key) =
        { props with LocalStorageKey = Some key }

    /// Groups the data by selecting a grouping key.
    /// This key will be displayed in the default group header template.
    [<CustomOperation("groupBy", MaintainsVariableSpace=true)>]
    member this.GroupBy (props, [<ProjectionParameter>] keySelector) =
        let color =
            match props.GroupingLevels.Length with
            | 0 -> "#ececec"
            | 1 -> "whitesmoke"
            | _ -> "white"

        { props with
            GroupingLevels =
                props.GroupingLevels @ [
                    { GroupingLevel.KeySelector = keySelector
                      GroupingLevel.HeaderTemplate = (headerRowTemplate color)
                      GroupingLevel.FooterTemplate = None
                      GroupingLevel.SortBy = SortDefault
                      GroupingLevel.Collapsed = Collapse true }
                ] }

    /// Modifies the default group header template color (requires a 'groupBy').
    [<CustomOperation("groupColor", MaintainsVariableSpace=true)>]
    member this.GroupColor (props, (color: string)) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with HeaderTemplate = (headerRowTemplate color) } ] }

    /// Determines the initial collapsed state of a group (requires a 'groupBy').
    [<CustomOperation("groupCollapsed", MaintainsVariableSpace=true)>]
    member this.GroupCollapsed (props, collapsed) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with Collapsed = Collapse collapsed } ] }

    /// A function that determines the initial collapsed state of a group (requires 'groupBy').
    [<CustomOperation("groupCollapsedIf", MaintainsVariableSpace=true)>]
    member this.GroupCollapsedIf (props, [<ProjectionParameter>] collapsedIf) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with Collapsed = CollapseIf collapsedIf } ] }

    /// Overrides the default group header template (requires a 'groupBy').
    [<CustomOperation("groupHeader", MaintainsVariableSpace=true)>]
    member this.GroupHeader (props, headerTemplate) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with HeaderTemplate = headerTemplate } ] }

    /// Defines an optionsl group footer (requires a 'groupBy').
    [<CustomOperation("groupFooter", MaintainsVariableSpace=true)>]
    member this.GroupFooter (props, footerTemplate) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with FooterTemplate = Some footerTemplate } ] }

    /// Sorts a group in ascending order by the given sort expression.
    [<CustomOperation("groupSortBy", MaintainsVariableSpace=true)>]
    member this.GroupSortBy (props, [<ProjectionParameter>] sortExpr) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with SortBy = SortAsc sortExpr } ] }

    /// Sorts a group in descending order by the given sort expression.
    [<CustomOperation("groupSortByDescending", MaintainsVariableSpace=true)>]
    member this.GroupSortByDescending (props, [<ProjectionParameter>] sortExpr) =
        let last, rest = props.GroupingLevels |> getLastAndRest
        { props with
            GroupingLevels = rest @ [ { last with SortBy = SortDesc sortExpr } ] }

    /// Creates an item template.
    [<CustomOperation("select", MaintainsVariableSpace=true)>]
    member this.Select (props, [<ProjectionParameter>] tmpl) =
        { props with ItemTemplate = tmpl }

    member this.Run props =
        //render props
        let cmp = render //makeComponent()
        cmp props

/// Builds a grouping panel.
let groupingPanel = GroupingPanelBuilder()

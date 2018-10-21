module GodotUtils

open Godot
open LanguagePrimitives


type Node with
    member this.GetNode<'a when 'a :> Node> (path : string) =
        lazy((this.GetNode(new NodePath(path))) :?> 'a)

let physicallyEquals a b =
    PhysicalEquality a b

let findItemIndexInArray item array =
    array
    |> Array.findIndex (fun a -> physicallyEquals item a)

let findItemIndexInOptionArray (item : 'a, inputArray : 'a option array) =
    inputArray
    |> Array.findIndex (fun a ->
                        match a.IsSome with
                            | false ->
                                false
                            | true ->
                                physicallyEquals item a.Value)

let sortObjectListClosestFirst (bodies : Array) (closestTo : Spatial) =
    let isAFurtherAwayFromToSelection a b =
        let getDistanceToSelection (spatial : Spatial) =
            spatial.GetGlobalTransform().origin.DistanceTo(closestTo.GetGlobalTransform().origin)
        let aDistance = getDistanceToSelection a
        let bDistance = getDistanceToSelection b

        match aDistance > bDistance with
            | true -> 1
            | false ->
                match aDistance = bDistance with
                    | true -> 0
                    | false -> -1

    match bodies.Count with
        | 0 -> []
        | _ ->
            bodies
            |> Seq.toList
            // Remove any non-actors
            |> List.choose (fun currentBody ->
                            match currentBody :? Spatial with
                                | true ->
                                    let currentBodySpatial = (currentBody :?> Spatial)
                                    // Make sure node isn't pulled from tree (could happen on item pickup, and any time an object needs to be temporarily disabled)
                                    match currentBodySpatial.IsInsideTree() with
                                        | true ->
                                            Some currentBodySpatial
                                        | false ->
                                            None
                                | false -> None)
            |> List.sortWith isAFurtherAwayFromToSelection

module GodotUtils

open Godot

type Node with
    member this.getNode<'a when 'a :> Node> (path : string) =
        lazy((this.GetNode(new NodePath(path))) :?> 'a)

let sortObjectListClosestFirst (bodies : Array) (closestTo : Spatial) =
    let isAFurtherAwayFromToSelection a b =
        let getDistanceToSelection (spatial : Spatial) =
            spatial.GetGlobalTransform().origin.DistanceTo(closestTo.GetGlobalTransform().origin)
        let aDistance = getDistanceToSelection a
        let bDistance = getDistanceToSelection b

        if aDistance > bDistance
        then 1
        else if aDistance = bDistance then 0
        else -1
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
            |> List.sortWith (fun a b -> isAFurtherAwayFromToSelection a b)

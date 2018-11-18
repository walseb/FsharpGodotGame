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

let vector2To3(vector : Vector2) =
    Vector3(vector.x, 0.0f, vector.y)

let vector3To2(vector : Vector3) =
    Vector2(vector.x, vector.z)

let sin90Degree = Mathf.Sin(Mathf.Deg2Rad(90.0f))
let cos90Degree = Mathf.Cos(Mathf.Deg2Rad(90.0f))

let rotateVector90Degrees (vector : Vector2) =
    vector
    |> (fun a ->
        Vector2((a.x * cos90Degree - a.y * sin90Degree), (a.x * sin90Degree + a.y * cos90Degree)))

let sin180Degree = Mathf.Sin(Mathf.Deg2Rad(180.0f))
let cos180Degree = Mathf.Cos(Mathf.Deg2Rad(180.0f))

let rotateVector180Degrees (vector : Vector2) =
    vector
    |> (fun a ->
        Vector2((a.x * cos180Degree - a.y * sin180Degree), (a.x * sin180Degree + a.y * cos180Degree)))

let rotateVector (vector : Vector2, degrees : float32) =
    vector
    |> (fun a ->
        let sin = Mathf.Sin(Mathf.Deg2Rad(degrees))
        let cos = Mathf.Cos(Mathf.Deg2Rad(degrees))
        Vector2((a.x * cos - a.y * sin), (a.x * sin + a.y * cos)))

let radToVector2 rad =
    Vector2(cos(rad), sin(rad))

let objectsEqual IDa IDb =
    IDa = IDb

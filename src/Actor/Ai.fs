namespace Actor.Ai

open System.Collections.Generic
open References
open Actor
open Godot
open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils

type ActorAi() as this =
    inherit Spatial()

    let attachedActor : Lazy<ActorObject> =
        lazy (this.GetParent() :?> ActorObject)

    let navigation : Lazy<Navigation> =
        lazy (this.GetParent().GetParent().GetNode(new NodePath("Navigation")) :?> Navigation)

    let mutable navPoints : Vector3 [] option = None
    let mutable selectedNav = 0
    let nextNavpointDistance = 0.5f

    let updateMovementInterval = 0.1f
    let mutable updateMovementTimer = 0.0f

    let mutable playerWithinViewRange = false

    let setNavTarget targetPos =
        let thisPosition = this.GetGlobalTransform().origin
        navPoints <- Some (navigation.Force().GetSimplePath(thisPosition, targetPos))
        selectedNav <- 0

    //let nav thisPosition = navigation.Force().GetClosestPointToSegment(thisPosition, Vector3(0.0f,0.0f,0.0f))
    let setActorMoveDirection newDir =
        attachedActor.Force().ActorButtons.MoveDirection <- newDir
        attachedActor.Value.InputUpdated()

    let updateMovementDirection() =
        let thisPos = attachedActor.Force().GetGlobalTransform().origin

        match navPoints.IsSome with
            | false ->
                ()
            | true ->
                match selectedNav < navPoints.Value.Length with
                    | false ->
                        setActorMoveDirection Vector2.Zero
                        selectedNav <- 0
                        navPoints <- None
                    | true ->
                        let distance = thisPos.DistanceSquaredTo(navPoints.Value.[selectedNav])
                        match distance <= nextNavpointDistance with
                            | true ->
                                GD.Print "Next nav!!"
                                selectedNav <- selectedNav + 1
                            | false ->
                                let this2DPos = (vector3To2 thisPos)
                                let nav2DPos = (vector3To2 navPoints.Value.[selectedNav])
                                let directionToClosestNav = (Vector2 ((this2DPos.x - nav2DPos.x), (this2DPos.y - nav2DPos.y)))
                                setActorMoveDirection(rotateVector180Degrees directionToClosestNav)

    let isBodyActor (body : Object) =
        // the (Body :? ActorObject) check should be slightly more performant if parameter body is terrain, etc
        body :? ActorObject && ReferencesStored.PlayerBoxed.IsSome && physicallyEquals ReferencesStored.PlayerBoxed.Value body

    member this._on_DetectionArea_body_entered(body : Object) =
        match isBodyActor body with
            | true ->
                playerWithinViewRange <- true
            | false ->
                ()

    member this._on_DetectionArea_body_exited(body : Object) =
        match isBodyActor body with
            | true ->
                playerWithinViewRange <- false
            | false ->
                ()

    override this._PhysicsProcess(delta : float32) =
        let spaceState = this.GetWorld().GetDirectSpaceState()
        match playerWithinViewRange && ReferencesStored.Player.IsSome with
            | false ->
                ()
            | true ->
                let hits = spaceState.IntersectRay(this.GetGlobalTransform().origin, ReferencesStored.Player.Value.GetGlobalTransform().origin)

                match hits.Count <> 0 && isBodyActor (hits.Item("collider") :?> Object) with
                    | true ->
                        ()
                    | false ->
                        match ReferencesStored.Player.Value.IsInCombatState with
                            | true ->
                                // Make sure they don't spam the set nav target function
                                match navPoints.IsNone with
                                    | true ->
                                        setNavTarget(ReferencesStored.Player.Value.GetGlobalTransform().origin)
                                    | false ->
                                        ()
                            | false ->
                                ()

    override this._Process(delta : float32) =
        match updateMovementTimer > updateMovementInterval with
            | true ->
                updateMovementDirection()
                updateMovementTimer <- 0.0f
            | false ->
                updateMovementTimer <- updateMovementTimer + delta

    override this._Ready() =
        // let thisPos = attachedActor.Force().GetGlobalTransform().origin
        // updateNavPoints thisPos Vector3.Zero
        ()

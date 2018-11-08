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

    // ** Vars
    let attachedActor : Lazy<ActorObject> =
        lazy (this.GetParent() :?> ActorObject)

    let updateMoveDirectionInterval = 0.1f
    let mutable updateMoveDirectionTimer = 0.0f

    // ** Navigation
    let mutable navPoints : Vector3 [] option = None
    let mutable selectedNav = 0
    let nextNavpointDistance = 0.5f
    let playerDetectionMask = (lazy ((this.GetNode(NodePath "PlayerDetectionMask") :?> Area).CollisionMask))

    let setNavTarget targetPos =
        let thisPosition = this.GetGlobalTransform().origin
        match ReferencesStored.NavigationMesh.IsSome with
            | true ->
                navPoints <- Some (ReferencesStored.NavigationMesh.Value.GetSimplePath(thisPosition, targetPos))
                selectedNav <- 0
            | false ->
                ()

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

    // ** Player detection
    let mutable playerWithinViewRange = false

    // *** Is player blocked
    // How many seconds to wait before scanning for player
    let scanViewForPlayerInterval = 0.5f
    let mutable scanViewForPlayerTimer = 0.0f

    let isPlayerVisible () =
        let spaceState = this.GetWorld().GetDirectSpaceState()
        match ReferencesStored.Player.IsSome with
            | false ->
                false
            | true ->
                // The layer 25 SHOULD be terrain AND player BUT IS ATLEAST terrain, actor, player
                let hits = spaceState.IntersectRay(this.GetGlobalTransform().origin, ReferencesStored.Player.Value.GetGlobalTransform().origin, Array(), playerDetectionMask.Force())
                hits.Count <> 0 && isBodyActor (hits.Item("collider") :?> Object)

    // *** Is player within view range
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

    // ** Update
    override this._PhysicsProcess(delta : float32) =
        match playerWithinViewRange with
            | false ->
                ()
            | true ->
                match scanViewForPlayerTimer > scanViewForPlayerInterval with
                    | true ->

                        scanViewForPlayerTimer <- 0.0f
                        match isPlayerVisible() && ReferencesStored.Player.Value.IsInCombatState && navPoints.IsNone with
                            | true ->
                                GD.Print "FOUND!!"
                                setNavTarget(ReferencesStored.Player.Value.GetGlobalTransform().origin)
                            | false ->
                                ()
                    | false ->
                        scanViewForPlayerTimer <- scanViewForPlayerTimer + delta

    override this._Process(delta : float32) =
        match updateMoveDirectionTimer > updateMoveDirectionInterval with
            | true ->
                updateMovementDirection()
                updateMoveDirectionTimer <- 0.0f
            | false ->
                updateMoveDirectionTimer <- updateMoveDirectionTimer + delta

    override this._Ready() =
        // let thisPos = attachedActor.Force().GetGlobalTransform().origin
        // updateNavPoints thisPos Vector3.Zero
        ()

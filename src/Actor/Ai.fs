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

    let mutable enemyInView : Spatial option = None

    let mutable hasSeenPlayerInCombat = false

    // ** Navigation
    let mutable navPoints : Vector3 [] option = None
    let mutable selectedNavPoint = 0
    let nextNavpointDistance = 0.5f
    let playerDetectionMask = (lazy ((this.GetNode(NodePath "PlayerDetectionMask") :?> Area).CollisionMask))

    let getNavPath targetPos =
        let thisPosition = this.GetGlobalTransform().origin
        (ReferencesStored.NavigationMesh.Value.GetSimplePath(thisPosition, targetPos))

    let setNavPoints targetPos =
        navPoints <- Some (getNavPath targetPos)
        selectedNavPoint <- 0

    let setActorMoveDirection newDir =
        attachedActor.Force().ActorButtons.MoveDirection <- newDir
        attachedActor.Value.InputUpdated()

    let updateMovementDirection() =
        let thisPos = attachedActor.Force().GetGlobalTransform().origin

        match navPoints.IsSome with
            | false ->
                ()
            | true ->
                match selectedNavPoint < navPoints.Value.Length with
                    | false ->
                        setActorMoveDirection Vector2.Zero
                        selectedNavPoint <- 0
                        navPoints <- None
                    | true ->
                        let distance = thisPos.DistanceSquaredTo(navPoints.Value.[selectedNavPoint])
                        match distance <= nextNavpointDistance with
                            | true ->
                                GD.Print "Next nav!!"
                                selectedNavPoint <- selectedNavPoint + 1
                            | false ->
                                let this2DPos = (vector3To2 thisPos)
                                let nav2DPos = (vector3To2 navPoints.Value.[selectedNavPoint])
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

// * Attack player
    let engageTarget engage =
        match engage with
            | true ->
                let selectedItem = attachedActor.Force().Inventory.[attachedActor.Force().SelectedItem]

                match selectedItem.IsSome && selectedItem.Value :? Items.Gun with
                    | true ->
                        let magazine = (selectedItem.Value :?> Items.Gun).Magazine
                        match magazine.IsSome with
                            | true ->
                                attachedActor.Force().ActorButtons.ReloadPressed <- magazine.Value.StoredAmmo = 0
                            | false ->
                                attachedActor.Force().ActorButtons.ReloadPressed <- true
                    | false ->
                        ()
                let whatWouldFiringHit = attachedActor.Force().WhatWouldFiringHit()
                // This is probably REALLY expensive, FIXME
                match whatWouldFiringHit.IsSome && physicallyEquals whatWouldFiringHit.Value ReferencesStored.PlayerBoxed.Value with
                    | true ->
                        attachedActor.Force().ActorButtons.PrimaryAttackPressed <- true
                    | false ->
                        attachedActor.Force().ActorButtons.PrimaryAttackPressed <- false

                attachedActor.Force().ActorButtons.AimPressed <- true
            | false ->
                attachedActor.Force().ActorButtons.AimPressed <- false
                attachedActor.Force().ActorButtons.PrimaryAttackPressed <- false

        attachedActor.Force().InputUpdated()

    let enemyDirectViewGained () =
        match (hasSeenPlayerInCombat || ReferencesStored.Player.Value.IsInCombatState) with
            | false ->
                ()
            | true ->
                match enemyInView.IsNone with
                    | true ->
                        hasSeenPlayerInCombat <- true
                        GD.Print "FOUND!!"
                        enemyInView <- Some (ReferencesStored.Player.Value :> Spatial)
                        navPoints <- None
                        setActorMoveDirection Vector2.Zero
                    | false ->
                        GD.Print "ENGAGING"
                        engageTarget true

    let enemyDirectViewLost () =
        match enemyInView.IsSome with
            | true ->
                engageTarget false
                GD.Print "I LOST HIM!!"
                // Move towards last seen position
                setNavPoints (enemyInView.Value.GetGlobalTransform().origin)
            | false ->
                ()
        enemyInView <- None

    let updateWatchBehaviour (delta : float32) =
        let isTimeToUpdate () =
                match scanViewForPlayerTimer > scanViewForPlayerInterval with
                    | true ->
                        scanViewForPlayerTimer <- 0.0f
                        true
                    | false ->
                        scanViewForPlayerTimer <- scanViewForPlayerTimer + delta
                        false

        match playerWithinViewRange && isTimeToUpdate() with
            | false ->
                ()
            | true ->
                match isPlayerVisible() with
                    | true ->
                        enemyDirectViewGained()
                    | false ->
                        enemyDirectViewLost()
                        // Enemy is not in view

    // *** Attack logic
    let updateAttackBehaviourInterval = 0.4f
    let mutable updateAttackBehaviourTimer = 0.0f

    let updateAttackBehaviour (delta : float32) =
        let isTimeToUpdate() =
            match updateAttackBehaviourInterval > updateAttackBehaviourTimer with
                | true ->
                    updateAttackBehaviourTimer <- 0.0f
                    true
                | false ->
                    updateAttackBehaviourTimer <- updateAttackBehaviourTimer + delta
                    false
        match isTimeToUpdate() with
            | false ->
                ()
            | true ->
                match enemyInView.IsSome with
                    | true ->
                        // Adjust aim
                        attachedActor.Force().ActorButtons.AimTarget <- vector3To2(enemyInView.Value.GetGlobalTransform().origin)
                    | false ->
                        ()

    // ** Is player within view range
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
                enemyDirectViewLost()
            | false ->
                ()

    // ** Update
    override this._PhysicsProcess(delta : float32) =
        updateWatchBehaviour delta
        ()

    override this._Process(delta : float32) =
        updateAttackBehaviour delta

        match updateMoveDirectionTimer > updateMoveDirectionInterval with
            | true ->
                updateMovementDirection()
                updateMoveDirectionTimer <- 0.0f
            | false ->
                updateMoveDirectionTimer <- updateMoveDirectionTimer + delta

    override this._Ready() =
        // Give actor weapons and ammo
        GD.Print ("NAME:", this.GetParent().Name)
        attachedActor.Force().Inventory.[0] <- Some ((this.GetParent().GetNode(NodePath "ItemAk47") :?> Items.Item))
        // attachedActor.Force().Inventory.[1] <- Some ((this.GetParent().GetNode(NodePath "ItemAk47") :?> Items.Item))
        // attachedActor.Force().Inventory.[9] <- Some ((this.GetParent().GetNode(NodePath "ItemAk47") :?> Items.Item))
        attachedActor.Force().Inventory.[1] <- Some ((this.GetParent().GetNode(NodePath "ItemRifleAmmo") :?> Items.Item))
        ()

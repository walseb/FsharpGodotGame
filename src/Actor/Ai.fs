namespace Actor.Ai

open System.Collections.Generic
open References
open Actor
open Godot
open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils

// * Ai

[<AbstractClass>]
type Ai() as this =
    inherit Spatial()

    let attachedActor : Lazy<ActorObject> =
        lazy (this.GetParent() :?> ActorObject)

    // ** Navigation
    let mutable navPoints : Vector3 [] option = None
    let mutable selectedNavPoint = 0
    let nextNavpointDistance = 2.0f

    let getNavPath targetPos =
        let thisPosition = this.GetGlobalTransform().origin
        ReferencesStored.NavigationMesh.Value.GetSimplePath(thisPosition, targetPos)

    let setActorMoveDirection newDir =
        attachedActor.Force().ActorButtons.MoveDirection <- newDir
        attachedActor.Value.InputUpdated()

    let updateMoveInterval = 0.1f
    let mutable updateMoveTimer = 0.0f

    member this.NextNavPoint
        with get () =
            match navPoints.IsSome && navPoints.Value.Length > selectedNavPoint with
                | true ->
                    Some navPoints.Value.[selectedNavPoint]
                | false ->
                    None

    member this.AttachedActor
        with get () = attachedActor.Force()

    member this.ResetNavPath() =
        navPoints <- None
        setActorMoveDirection Vector2.Zero

    member this.SetNavGoal targetPos =
        navPoints <- Some (getNavPath targetPos)
        selectedNavPoint <- 0

    member this.UpdateMovement delta =
        let isTimeToUpdate() =
            match updateMoveTimer > updateMoveInterval with
                | true ->
                    updateMoveTimer <- 0.0f
                    true
                | false ->
                    updateMoveTimer <- updateMoveTimer + delta
                    false

        match isTimeToUpdate() with
            | false ->
                ()
            | true ->
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

type AiStates =
    // Will act as civilian
    | NormalState
    | CombatState
    //| FindWeaponState
    | EscapeState

// * Combat AI
type CombatAi() as this =
    inherit Ai()

    let mutable state = AiStates.NormalState

    // ** Player detection
    let mutable lastSeenEnemyPos : Vector3 option = None

    // Used to aim
    let mutable enemyInDirectView : ActorObject option =
        None

    // Used to decide if should aim, contains instance IDs
    let mutable enemiesInViewRange : List<ActorObject> =
        new List<ActorObject>()

    // Used to not forget people pulled gun before, contains instance IDs
    let mutable actorsSeenInCombat : List<int> =
        new List<int>()

    let isEnemy (actor : ActorObject) =
        actor.IsOnPlayerTeam

    let getEnemyInView (actorID : int) =
        enemiesInViewRange
        |> Seq.tryPick (fun enemy ->
            match enemy.GetInstanceId() = actorID with
                | true ->
                    Some enemy
                | false ->
                    None)

    // *** Is player blocked
    // How many seconds to wait before scanning for player
    let scanViewForPlayerInterval = 0.5f
    let mutable scanViewForPlayerTimer = 0.0f

    let enemyDetectionMask = (lazy ((this.GetNode(new NodePath "PlayerDetectionMask") :?> Area).CollisionMask))

    let isEnemyInSight (delta : float32) : AiStates option =
        let getEnemyIfVisable () : ActorObject option =
            let hitToActor (hits : Dictionary) =
                match hits.Count <> 0 with
                    | false ->
                       None
                    | true ->
                        let hit = (hits.Item("collider") :?> Object)
                        let instanceID = hit.GetInstanceId()
                        match getEnemyInView instanceID with
                            | Some x ->
                                Some x
                            | None ->
                                None

            let isEnemyHostile (enemy : ActorObject option) =
                match enemy.IsSome && (enemy.Value.IsInCombatState || actorsSeenInCombat.Contains (enemy.Value.GetInstanceId())) with
                    | true ->
                        enemy
                    | false ->
                        None

            let spaceState = (lazy this.GetWorld().GetDirectSpaceState())

            enemiesInViewRange
            |> Seq.tryPick (fun enemySpatial ->
                         enemySpatial.GetGlobalTransform().origin
                         |> (fun currEnemyPos -> spaceState.Force().IntersectRay(this.GetGlobalTransform().origin, currEnemyPos, new Array(), enemyDetectionMask.Force()))
                         |> hitToActor
                         |> isEnemyHostile)

        let isTimeToUpdate () =
            match scanViewForPlayerTimer > scanViewForPlayerInterval with
                | true ->
                    scanViewForPlayerTimer <- 0.0f
                    true
                | false ->
                    scanViewForPlayerTimer <- scanViewForPlayerTimer + delta
                    false

        // Begin
        match isTimeToUpdate() with
            | false ->
                None
            | true ->
                match enemiesInViewRange.Count = 0 with
                    | true ->
                        None
                    | false ->
                        Some getEnemyIfVisable()

    let enemyDirectViewLost () =
        let lastEnemyPos = enemyInDirectView.Value.GetGlobalTransform().origin
        lastSeenEnemyPos <- Some lastEnemyPos
        this.SetNavGoal lastEnemyPos
        enemyInDirectView <- None

// * Attack enemies

    let checkIfGunIsReadyInterval = 0.5f
    let mutable checkIfGunIsReadyTimer = 0.0f

    // Sponge, if ai run out of mags, there are gonna be problems
    let readyGun delta : bool =
        let getSelectedGun (selectedItem : Items.Item option) =
            match selectedItem.IsSome && selectedItem.Value :? Items.Gun with
                | false ->
                    fail "Actor has no gun in hand but is trying to use it"
                | true ->
                      ok (selectedItem.Value :?> Items.Gun)

        let reloadIfNeeded (gun : Items.Gun) =
            let magazine = gun.Magazine
            match magazine.IsSome && magazine.Value.StoredAmmo > 0 with
                | true ->
                    this.AttachedActor.ActorButtons.ReloadPressed <- false
                    ok gun
                | false ->
                    this.AttachedActor.ActorButtons.ReloadPressed <- true
                    fail "Reloading!"

        let boltIfNeeded (gun : Items.Gun) =
            match gun.IsBulletInChamber with
                | true ->
                    this.AttachedActor.ActorButtons.BoltPressed <- false
                    ok gun
                | false ->
                    this.AttachedActor.ActorButtons.BoltPressed <- true
                    fail "Bolting gun!"

        let isTimeToUpdate () =
            match checkIfGunIsReadyTimer > checkIfGunIsReadyInterval with
                | true ->
                    checkIfGunIsReadyTimer <- 0.0f
                    true
                | false ->
                    checkIfGunIsReadyTimer <- checkIfGunIsReadyTimer + delta
                    false

        match isTimeToUpdate() with
            | false ->
                false
            | true ->
                this.AttachedActor.Inventory.[this.AttachedActor.SelectedItem]
                |> getSelectedGun
                |> bind reloadIfNeeded
                |> bind boltIfNeeded
                |> isOk
                |> tee (fun inputUpdated ->
                        match inputUpdated with
                            | false ->
                                ()
                            | true ->
                                this.AttachedActor.InputUpdated())

    // *** Attack logic
    let updateAttackBehaviourInterval = 0.4f
    let mutable updateAttackBehaviourTimer = 0.0f

    let updateAttackBehaviour (delta : float32) =
        let attemptToShoot() =
            this.AttachedActor.WhatWouldFiringHit()
            |> (fun potentialHit -> potentialHit.IsSome && (getEnemyInView (potentialHit.Value)).IsSome)
            |> (fun enemyPotentialHit ->
                this.AttachedActor.ActorButtons.PrimaryAttackPressed <- enemyPotentialHit
                this.AttachedActor.InputUpdated())

        let aimTowardsPlayer () =
            this.AttachedActor.ActorButtons.AimTarget <- vector3To2(enemyInDirectView.Value.GetGlobalTransform().origin)
            this.AttachedActor.InputUpdated()

        let aimTowardsWalkDir () =
            // OLD fixes bug??: match this.AttachedActor.ActorButtons.MoveDirection.LengthSquared() < 1.0f with
            match this.NextNavPoint.IsSome with
                | true ->
                    this.AttachedActor.ActorButtons.AimTarget <- vector3To2 this.NextNavPoint.Value
                    this.AttachedActor.InputUpdated()
                | false ->
                    ()

        match enemyInDirectView.IsSome with
            | true ->
                //GD.Print "SHOOTING!!??!??!"
                aimTowardsPlayer()
                attemptToShoot()
            | false ->
                aimTowardsWalkDir()

    // ** States

    // *** Normal state
    let normalStateStart() : AiStates option =
        GD.Print "STARTING NORMAL"
        None

    let normalStateProcess delta : AiStates option =
        this.UpdateMovement delta
        None

    let normalStatePhysicsProcess delta : AiStates option =
        // IF STATEMENT
        isEnemyInSight delta
        |> (fun enemy ->
            match enemy.IsSome with
                | true ->
                    enemyInDirectView <- Some enemy.Value
                    actorsSeenInCombat.Add (enemy.Value.GetInstanceId())
                    Some AiStates.CombatState
                | false ->
                    match enemyInDirectView.IsSome with
                        | true ->
                            enemyDirectViewLost()
                            enemyInDirectView <- None
                            None
                        | false ->
                            None)


    // *** Combat state
    let combatStateStart() : AiStates option =
        GD.Print "STARTING COMBAT"
        this.AttachedActor.ActorButtons.AimPressed <- true
        this.AttachedActor.InputUpdated()
        None

    let combatStateProcess delta : AiStates option =
        match this.AttachedActor.LastShootAttemptWorked with
            | true ->
                this.UpdateMovement delta
                updateAttackBehaviour delta
            | false ->
                this.AttachedActor.LastShootAttemptWorked <- readyGun delta
        None

    let combatStatePhysicsProcess delta : AiStates option =
        updateVisableEnemies delta

    // *** Find weapon state
    // let mutable stateBeforeFindingWeapon = None
//
    let mutable weaponsNearby : List<Items.Item> =
        new List<Items.Item>()
//
    // let findNeedWeapon () =
        // Items.GetWeapon(this.AttachedActor.Inventory)
        // // currently selected slot on actor
        // // like we need to maybe designate slot 1 to weapons or something, or create a auto-slot selector
        // // maybe create function select weapon slot, which changes current selected slot on actor to one containing a weapon
//
    // let findWeaponStateStart() : AiStates option =
        // stateBeforeFindingWeapon <- Some state
        // GD.Print "FINDING WEAPON"
        // None
//
    // let exitFindWeaponState (state : AiStates option) : AiStates option =
        // this.AttachedActor.ActorButtons.PickupPressed <- false
        // this.AttachedActor.InputUpdated()
//
        // match state.IsSome with
            // | true ->
                // state
            // | false ->
                // stateBeforeFindingWeapon
//
    // let findWeaponStateProcess delta : AiStates option =
        // this.UpdateMovement delta
        // updateAttackBehaviour delta
        // None
//
    // let findWeaponStatePhysicsProcess delta : AiStates option =
        // updateVisableEnemies delta

    // *** Escape state
    let escapeStateStart() : AiStates option =
        None

    let escapeStateProcess delta : AiStates option =
        None

    let escapeStatePhysicsProcess delta : AiStates option =
        None

    // ** Keep states updated
    let processAiState delta : AiStates option =
        match state with
            | AiStates.NormalState -> normalStateProcess delta
            | AiStates.CombatState -> combatStateProcess delta
            | AiStates.EscapeState -> escapeStateProcess delta
            //| AiStates.FindWeaponState -> findWeaponStateProcess delta

    let physicsProcessAiState delta : AiStates option =
        match state with
            | AiStates.NormalState -> normalStatePhysicsProcess delta
            | AiStates.CombatState -> combatStatePhysicsProcess delta
            | AiStates.EscapeState -> escapeStatePhysicsProcess delta
            //| AiStates.FindWeaponState -> findWeaponStatePhysicsProcess delta

    let changeAiState (newState : AiStates option) =
        match newState.IsSome && newState.Value = state with
            | true ->
                None
            | false ->
                match newState with
                    | Some AiStates.NormalState ->
                        state <- newState.Value
                        normalStateStart()
                    | Some AiStates.CombatState ->
                        state <- newState.Value
                        combatStateStart()
                    | Some AiStates.EscapeState ->
                        state <- newState.Value
                        escapeStateStart()
                    // | Some AiStates.FindWeaponState ->
                        // state <- newState.Value
                        // findWeaponStateStart()
                    | None ->
                        None
        |> ignore

    // ** Is player within view range
    member this._on_DetectionArea_body_entered(body : Object) =
        match (body :? ActorObject) with
            | false ->
                ()
            | true ->
                let actorObject = body :?> ActorObject
                match isEnemy actorObject with
                    | true ->
                        enemiesInViewRange.Add actorObject
                    | false ->
                        ()

    member this._on_DetectionArea_body_exited(body : Object) =
        match getEnemyInView (body.GetInstanceId()) with
            | Some x ->
                enemiesInViewRange.Remove x
                |> ignore
            | None ->
                ()

    // **  Log items nearby
    member this._on_ItemFindArea_body_entered(body : Object) =
        match (body :? Items.Item) && ((body :? Items.Weapon) || (body :? Items.Magazine)) with
            | false ->
                ()
            | true ->
                weaponsNearby.Add(body :?> Items.Item)

    member this._on_ItemFindArea_body_exited(body : Object) =
        match (body :? Items.Item) && ((body :? Items.Weapon) || (body :? Items.Magazine)) with
            | false ->
                ()
            | true ->
                weaponsNearby.Remove (body :?> Items.Item)
                |> ignore
                ()

    // ** Update
    override this._PhysicsProcess(delta : float32) =
        physicsProcessAiState delta
        |> changeAiState

    override this._Process(delta : float32) =
        processAiState delta
        |> changeAiState

    override this._Ready() =
        // Give actor weapons and ammo
        //GD.Print ("NAME:", this.GetParent().Name)
        //this.AttachedActor.Inventory.[1] <- Some ((this.GetParent().GetNode(new NodePath "ItemAk47") :?> Items.Item))
        //this.AttachedActor.Inventory.[0] <- Some ((this.GetParent().GetNode(new NodePath "ItemRifleAmmo") :?> Items.Item))
        ()

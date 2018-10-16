namespace Actor

open System.Collections.Generic
open Godot
open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils
open Items

type ActorState =
    | IdleState
    | MoveState
    | RunState
    | UnholsterState
    | HolsterState
    | HoldState
    | HoldMoveState
    | ReloadState
    | AttackState

type ActorMaxStats =
    {
        mutable MaxStrength : float
        mutable MaxAgility : float
        mutable MaxShooting : float
        mutable MaxCommandChildren : int
    }
type ActorCurrentStats =
    {
        mutable CurrentStrength : float
        mutable CurrentAgility : float
        mutable CurrentShooting : float
    }

type ActorButtons =
    {
        mutable MoveDirection : Vector2

        mutable PickupPressed : bool
        mutable DropPressed : bool
        mutable RunPressed : bool
        mutable AttackPressed : bool
        mutable AimPressed : bool

        mutable Select1Pressed : bool
        mutable Select2Pressed : bool
        mutable Select3Pressed : bool
        mutable Select4Pressed : bool
        mutable Select5Pressed : bool
        mutable Select6Pressed : bool
        mutable Select7Pressed : bool
        mutable Select8Pressed : bool
        mutable Select9Pressed : bool
        mutable Select0Pressed : bool
     }

type ActorObject() as this =
    inherit RigidBody()

    let handReachArea =
        lazy(this.GetNode(new NodePath("HandReachArea")) :?> Area)

    let mutable selectedItem : int = 0;

    let mutable inventorySize : int = 9;

    /// returns true if item has changed
    let changeSelectedItem (number : int) =
        match number < inventorySize && number >= 0 && number <> selectedItem with
            | false ->
                false
            | true ->
                selectedItem <- number
                true

    let mutable items : Item option array = Array.create 9 None

    let animatedSprite =
        lazy(this.GetNode(new NodePath("AnimatedSprite3D")) :?> AnimatedSprite3D)

    let mutable actorMaxStats : ActorMaxStats option =
        None

    let mutable actorCurrentStats : ActorCurrentStats option =
        None

    let mutable commandParent : ActorObject option =
        None

    let mutable commandChildren =
        new List<ActorObject>()

    let mutable state = ActorState.IdleState

    let mutable actorButtons : ActorButtons =
        {
            MoveDirection = Vector2(0.0f,0.0f)

            PickupPressed = false
            DropPressed = false
            RunPressed = false
            AttackPressed = false
            AimPressed = false

            Select1Pressed = false
            Select2Pressed = false
            Select3Pressed = false
            Select4Pressed = false
            Select5Pressed = false
            Select6Pressed = false
            Select7Pressed = false
            Select8Pressed = false
            Select9Pressed = false
            Select0Pressed = false
        }

    let throwItemForce = 10.0f

    // Used to tweak the gobal movement speed in case of gravity change, etc
    let physicsMoveMultiplier = 100.0f

    let toggleItemAttachedNextPhysicsUpdate =
        new List<RigidBody>()

    // Returns true if item is dropeed
    let attemptToggleItemAttached (item : RigidBody) : bool =
        match toggleItemAttachedNextPhysicsUpdate.Contains item with
            | true ->
                false
            | false ->
                true

    //////////////////
    //State actions //
    //////////////////

    let move (physicsState : PhysicsDirectBodyState) (multiplier : float32) =
        physicsState.ApplyImpulse(Vector3(0.0f, 0.0f, 0.0f), (Vector3 (actorButtons.MoveDirection.x, 0.0f, actorButtons.MoveDirection.y)).Normalized() * physicsMoveMultiplier * multiplier)

    let drop() =
        this.GetTree().SetInputAsHandled();
        match items.[selectedItem].IsSome with
            | false ->
                ()
            | true ->
                // Make sure an item isn't added twice
                match toggleItemAttachedNextPhysicsUpdate.Contains items.[selectedItem].Value with
                    | false ->
                        toggleItemAttachedNextPhysicsUpdate.Add items.[selectedItem].Value
                        GD.Print "DROP"
                    | true ->
                        ()
        items.[selectedItem] <- None

    let pickupDelay = 1.0f
    let mutable pickupTimer = 0.0f

    /// Returns true if new item has been picked up
    let pickup() =
        this.GetTree().SetInputAsHandled();
        match  pickupTimer > pickupDelay with
            | false ->
                false
            | true ->
                pickupTimer <- 0.0f
                sortObjectListClosestFirst (handReachArea.Force().GetOverlappingBodies()) this
                |> List.choose (fun a ->
                    match a :? Item with
                        | true ->
                            Some (a :?> Item)
                        | false ->
                            None)
                |> (fun a ->
                    match a.IsEmpty with
                    | true ->
                        false
                    | false ->
                            drop()
                            items.[selectedItem] <- Some (a.Head)
                            toggleItemAttachedNextPhysicsUpdate.Add a.Head
                            GD.Print "PICKUP"
                            true)

    //let mutable lastPos = this.GetGlobalTransform().origin
    // Remove this later
    let mutable value = 0.0f

    let rotateTowardsMoveDirection(delta : float32) =
        ()
        //  let thisTransform = this.GetGlobalTransform()
//
        //  // Create vector with only x and z coordinates
        //  let lookDir = Vector3(this.LinearVelocity.x + thisTransform.origin.x, 0.0f, this.LinearVelocity.z + thisTransform.origin.z)
//
        //  // Get target transform for looking at last pos
        //  let rotTransform = thisTransform.LookingAt(lookDir,Vector3.Up)
        //  // Slerp it
//
        //  let thisRotation = thisTransform.basis.Quat().Slerp(rotTransform.basis.Quat(),value)
        //  value <- value + delta
        //  match value > 1.0f with
            //  | true ->
                //  value <- 1.0f
            //  | false -> ()
//
        //  this.SetGlobalTransform(Transform(thisRotation, thisTransform.origin))

    let aim() =
        GD.Print("Aim not implemented")
        this.GetTree().SetInputAsHandled();

    let attack() =
        GD.Print("Attack not implemented")
        this.GetTree().SetInputAsHandled();

    let reload() =
        GD.Print("Attack not implemented")
        this.GetTree().SetInputAsHandled();

    let isMoveDirectionZero () =
        actorButtons.MoveDirection.x = 0.0f && actorButtons.MoveDirection.y = 0.0f

    // Mutable state machine data
    let mutable timer = 0.0f

    let mutable selectedWeaponOnCombatEnter = selectedItem

    let mutable selectedWeaponObject : Weapon option = None

    //////////////////
    // Animation helpers //
    //////////////////

    let setAnimation name speed =
        animatedSprite.Force().Play name
        animatedSprite.Value.GetSpriteFrames().SetAnimationSpeed(name, speed)

    let getHeldWeaponAnimationName animationStateName =
        let weaponType = ItemHelperFunctions.getWeaponType(items.[selectedWeaponOnCombatEnter].Value :?> Weapon)
        match weaponType.IsSome with
            | true ->
                (weaponType.Value + animationStateName)
            | false ->
                ""

    let setWeaponAnimation animationStateName speed =
                setAnimation (getHeldWeaponAnimationName animationStateName) speed

    let setWeaponAnimationTimed animationStateName time =
        let animationStateName = getHeldWeaponAnimationName animationStateName
        GD.Print ("name: " ,animationStateName)
        animatedSprite.Force().Frames.GetFrameCount animationStateName
        |> (fun a -> GD.Print("FRAME COUNT: ", a); a)
        |> (fun a -> (float32 a) / time)
        |> (fun a -> GD.Print("TIME: ", a); a)
        |> setAnimation animationStateName

    ////////////////////////////
    //Basic state conditions  //
    ////////////////////////////

    let hasWeaponSelected() =
        items.[selectedItem].IsSome && items.[selectedItem].Value :? Weapon

    /// Returns false if combat state failed to init
    let initializeCombatState() =
        match items.[selectedItem].IsSome && items.[selectedItem].Value :? Weapon with
            | true ->
                selectedWeaponOnCombatEnter <- selectedItem
                selectedWeaponObject <- Some (items.[selectedItem].Value :?> Weapon)
                true
             | false ->
                 false

    ///////////////////////
    //Common state keys  //
    ///////////////////////

    /// Returns true if different item is selected
    let selectItem () =
        match actorButtons.Select1Pressed with
            | true ->
                changeSelectedItem 1 |> ignore
                Some 1
            | false ->
                match actorButtons.Select2Pressed with
                    | true ->
                        changeSelectedItem 2 |> ignore
                        Some 2
                    | false ->
                        match actorButtons.Select3Pressed with
                            | true ->
                                changeSelectedItem 3 |> ignore
                                Some 3
                            | false ->
                                match actorButtons.Select4Pressed with
                                    | true ->
                                        changeSelectedItem 4 |> ignore
                                        Some 4
                                    | false ->
                                        match actorButtons.Select5Pressed with
                                            | true ->
                                                changeSelectedItem 5 |> ignore
                                                Some 5
                                            | false ->
                                                match actorButtons.Select6Pressed with
                                                    | true ->
                                                        changeSelectedItem 6 |> ignore
                                                        Some 6
                                                    | false ->
                                                        match actorButtons.Select7Pressed with
                                                            | true ->
                                                                changeSelectedItem 7 |> ignore
                                                                Some 7
                                                            | false ->
                                                                match actorButtons.Select8Pressed with
                                                                    | true ->
                                                                        changeSelectedItem 8 |> ignore
                                                                        Some 8
                                                                    | false ->
                                                                        match actorButtons.Select9Pressed with
                                                                            | true ->
                                                                                changeSelectedItem 9 |> ignore
                                                                                Some 9
                                                                            | false ->
                                                                                match actorButtons.Select0Pressed with
                                                                                    | true ->
                                                                                        changeSelectedItem 0 |> ignore
                                                                                        Some 0
                                                                                    | false ->
                                                                                        None

    //////////////
    //Idle state//
    //////////////

    let startIdle() =
        setAnimation "Idle" 100.0f
        None

    let updateKeysIdle() =
        //GD.Print (isMoveDirectionZero())
        //GD.Print (actorButtons.MoveDirection.x, actorButtons.MoveDirection.y)
        match isMoveDirectionZero() with
            | false ->
                Some MoveState
            | true ->
                    match actorButtons.AimPressed && hasWeaponSelected() with
                        | true ->
                            initializeCombatState() |> ignore
                            Some UnholsterState
                        | false ->
                            match actorButtons.PickupPressed with
                                | true ->
                                    pickup() |> ignore
                                    None
                                | false ->
                                    match actorButtons.DropPressed with
                                        | true ->
                                            drop()
                                            None
                                        | false ->
                                            selectItem() |> ignore
                                            None

    ///////////////
    // Move state//
    ///////////////

    let startMove() =
        setAnimation "Move" 10.0f
        None

    let updateKeysMove() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.RunPressed with
                    | true ->
                            Some RunState
                    | false ->
                        match actorButtons.AimPressed && hasWeaponSelected() with
                            | true ->
                                initializeCombatState() |> ignore
                                Some UnholsterState
                            | false ->
                                match actorButtons.PickupPressed with
                                    | true ->
                                        pickup() |> ignore
                                        None
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                None
                                            | false ->
                                                None

    let integrateForcesMove (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (3.0f * delta)

    let physicsProcessMove (delta : float32) =
        rotateTowardsMoveDirection(delta)

    ///////////////
    //Run state //
    ///////////////

    let startRun() =
        setAnimation "Run" 50.0f
        None

    let updateKeysRun() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.RunPressed with
                    | false ->
                        Some MoveState
                    | true ->
                        match actorButtons.AimPressed && hasWeaponSelected() with
                            | true ->
                                initializeCombatState() |> ignore
                                Some UnholsterState
                            | false ->
                                match actorButtons.PickupPressed && pickup() with
                                    | true ->
                                        None
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                None
                                            | false ->
                                                None

    let integrateForcesRun (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (5.0f * delta)

    let physicsProcessRun (delta : float32) =
        rotateTowardsMoveDirection(delta)

    //////////////
    //Hold state//
    //////////////

    let startHold() =
        setWeaponAnimation "Hold" 5.0f
        None

    let updateKeysHold() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | false ->
                Some HoldMoveState
            | true ->
                match actorButtons.AimPressed && selectedItem = selectedWeaponOnCombatEnter with
                    | false ->
                        Some HolsterState
                    | true ->
                        match actorButtons.AttackPressed with
                            | true ->
                                attack()
                                None
                            | false ->
                                match actorButtons.PickupPressed && pickup() with
                                    | true ->
                                        Some IdleState
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                Some IdleState
                                            | false ->
                                                None

    ///////////////////
    //Hold move state//
    ///////////////////

    let startHoldMove() =
        setWeaponAnimation "HoldMove" 5.0f
        None

    let updateKeysHoldMove() =
        match isMoveDirectionZero() with
            | true ->
                Some HoldState
            | false ->
                match actorButtons.AimPressed && selectedItem = selectedWeaponOnCombatEnter with
                    | false ->
                        Some HolsterState
                    | true ->
                        match actorButtons.AttackPressed with
                            | true ->
                                attack()
                                None
                            | false ->
                                match actorButtons.PickupPressed && pickup() with
                                    | true ->
                                        Some IdleState
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                Some IdleState
                                            | false ->
                                                None

    let integrateForcesHoldMove (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (1.5f * delta)

    ///////////////////
    //Reload state//
    ///////////////////

    let reloadTime : float32 = 2.0f

    let startReload() =
        setWeaponAnimationTimed "Reload" reloadTime
        None

    let updateReload  (delta : float32)  =
        timer <- timer + delta
        match timer > reloadTime with
            | true ->
                GD.Print "RELOAD DONE NOT IMPLEMENTED YET"
                //items.[selectedItem].ammoCapacity <- 2
                Some HoldState
            | false ->
                None

    let updateKeysReload() =
        match actorButtons.AimPressed && selectedItem = selectedWeaponOnCombatEnter with
            | false ->
                Some HolsterState
            | true ->
                match actorButtons.AttackPressed with
                    | true ->
                        attack()
                        None
                    | false ->
                        match actorButtons.PickupPressed && pickup() with
                            | true ->
                                Some IdleState
                            | false ->
                                match actorButtons.DropPressed with
                                    | true ->
                                        drop()
                                        Some IdleState
                                    | false ->
                                        None

    let integrateForcesReload (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    ///////////////
    //Attack state //
    ///////////////

    let startAttack() =
        setWeaponAnimationTimed "Attack" 5.0f
        None

    // Holster state
    let attackTime : float32 = 2.0f

    let updateAttack  (delta : float32)  =
        timer <- timer + delta
        match timer > attackTime with
            | true ->
                GD.Print "ATTACK DONE NOT IMPLEMENTED YET"
                //items.[selectedItem].ammoCapacity <- 2
                Some HoldState
            | false ->
                None

    let updateKeysAttack() =
        match actorButtons.PickupPressed && pickup() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        None

    let integrateForcesAttack (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    /////////////////
    //Holster state//
    /////////////////

    let holsterTime : float32 = 1.0f

    let startHolster() =
        setWeaponAnimationTimed "Holster" holsterTime
        None

    let updateHolster  (delta : float32)  =
        timer <- timer + delta
        match timer > holsterTime with
            | true ->
                timer <- 0.0f
                Some IdleState
            | false ->
                None

    let updateKeysHolster () =
        selectItem() |> ignore
        match actorButtons.AimPressed && selectedItem = selectedWeaponOnCombatEnter with
            | true ->
                Some HoldState
            | false ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        match actorButtons.PickupPressed && pickup() with
                            | true ->
                                Some IdleState
                            | false ->
                                None

    let integrateForcesHolster (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    ///////////////////
    //Unholster state//
    ///////////////////

    let unHolsterTime : float32 = 1.0f

    let startUnholster() =
        match items.[selectedItem].IsSome with
            | false ->
                Some IdleState
            | true ->
                setWeaponAnimationTimed "Unholster" unHolsterTime
                None

    let updateUnholster(delta : float32) =
        timer <- timer + delta
        match timer > unHolsterTime with
            | true ->
                timer <- 0.0f
                Some HoldState
            | false ->
                None

    let updateKeysUnholster () =
        selectItem() |> ignore
        match actorButtons.AimPressed && selectedItem = selectedWeaponOnCombatEnter with
            | false ->
                Some IdleState
            | true ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        match actorButtons.PickupPressed && pickup() with
                            | true ->
                                Some IdleState
                                | false ->
                                    None

    let integrateForcesUnholster (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    //////////////////////////////////////
    // End of statemachine definitions  //
    //////////////////////////////////////

    let switchStateStateMachine (actorState : ActorState) :  ActorState option =
        timer <- 0.0f
        match actorState with
            | IdleState -> startIdle()
            | MoveState -> startMove()
            | RunState -> startRun()
            | UnholsterState -> startUnholster()
            | HoldState -> startHold()
            | HoldMoveState -> startHoldMove()
            | HolsterState -> startHolster()
            | ReloadState -> startReload()
            | AttackState -> startAttack()

    let updateStateMachine (delta : float32) (actorState : ActorState) =
        match actorState with
            | IdleState -> None
            | MoveState -> None
            | RunState -> None
            | UnholsterState -> updateUnholster delta
            | HoldState -> None
            | HoldMoveState -> None
            | HolsterState -> updateHolster delta
            | ReloadState -> updateReload delta
            | AttackState -> updateAttack delta

    let physicsProcessForcesStateMachine  (delta : float32) (actorState : ActorState)  =
        match actorState with
            | IdleState -> ()
            | MoveState -> physicsProcessMove delta
            | RunState -> physicsProcessRun delta
            | UnholsterState -> ()
            | HoldState -> ()
            | HoldMoveState -> ()
            | HolsterState -> ()
            | ReloadState -> ()
            | AttackState -> ()

    let integrateForcesStateMachine  (delta : float32) (physicsState : PhysicsDirectBodyState) (actorState : ActorState)  =
        match actorState with
            | IdleState -> ()
            | MoveState -> integrateForcesMove delta physicsState
            | RunState -> integrateForcesRun delta physicsState
            | UnholsterState -> integrateForcesUnholster delta physicsState
            | HoldState -> ()
            | HoldMoveState -> integrateForcesHoldMove delta physicsState
            | HolsterState -> integrateForcesHolster delta physicsState
            | ReloadState -> integrateForcesReload delta physicsState
            | AttackState -> integrateForcesAttack delta physicsState

    let updateKeysStateMachine (actorState : ActorState) =
        match actorState with
            | IdleState -> updateKeysIdle()
            | MoveState -> updateKeysMove()
            | RunState -> updateKeysRun()
            | UnholsterState -> updateKeysUnholster()
            | HoldState -> updateKeysHold()
            | HoldMoveState -> updateKeysHoldMove()
            | HolsterState -> updateKeysHolster()
            | ReloadState -> updateKeysReload()
            | AttackState -> updateKeysAttack()

    let initializeStats() =
        actorMaxStats <-
                   Some {
                     MaxStrength = 0.0
                     MaxAgility = 0.0
                     MaxShooting = 0.0
                     MaxCommandChildren = 0
                   }

        actorCurrentStats <-
                   Some {
                     CurrentStrength = actorMaxStats.Value.MaxStrength
                     CurrentAgility = actorMaxStats.Value.MaxAgility
                     CurrentShooting = actorMaxStats.Value.MaxShooting
                   }

    let rec switchState (changeToState : ActorState option) =
        match changeToState with
            | Some x ->
                match switchStateStateMachine(x) with
                    | Some y ->
                        switchState (Some y)
                    | None ->
                        // Update keys right after switching state, since there might be no key events for a while after this
                        match updateKeysStateMachine(x) with
                            | Some z ->
                                switchState (Some z)
                            | None  ->
                                GD.Print("Switching actor state");
                                state <- x
            | None ->
                ()

    // Read-write property
    member this.ActorButtons
        with get () = actorButtons
        and set (value) = actorButtons <- value

    member this.CommandChildren
        with get () = commandChildren

    member this.AddActorUnderCommand(actorObject : ActorObject) =
        match commandChildren.Contains actorObject with
            | true ->
                ()
            | false ->
                commandChildren.Add actorObject

    member this.CommandParent
        with get () = commandParent
        and set (value) = commandParent <- value

    member this.InputUpdated() =
        switchState (updateKeysStateMachine state)

    member this.ResetActorButtons() =
        actorButtons.MoveDirection <- Vector2(0.0f,0.0f)
        actorButtons.PickupPressed <- false
        actorButtons.DropPressed <- false
        actorButtons.RunPressed <- false
        actorButtons.AttackPressed <- false
        actorButtons.AimPressed <- false

    override this._Ready() =
        initializeStats()
        this.SetProcessInput true

    override this._Process(delta : float32) =
        switchState (updateStateMachine delta state)
        pickupTimer <- pickupTimer + delta

    override this._PhysicsProcess(delta : float32) =
        let toggleQueuedAttachedState() =
            let toggleItem (item : RigidBody)=
                let itemParent = item.GetParent()
                match itemParent with
                    | null ->
                        // Drop
                        let thisTransform = this.GetGlobalTransform()
                        item.SetGlobalTransform (Transform (thisTransform.basis, thisTransform.origin))
                        this.Owner.AddChild item
                        // Impulse to make sure it's not sleeping, otherwise the collision somehow gets disabled and the item is bugged. Other solution is disabling "can sleep"
                        item.ApplyImpulse(Vector3(0.0f, 0.0f, 0.0f), (Vector3 (actorButtons.MoveDirection.x, 0.0f, actorButtons.MoveDirection.y)).Normalized() * throwItemForce)
                    | _ ->
                        // Pickup
                        item.SetLinearVelocity(Vector3(0.0f,0.0f,0.0f))
                        itemParent.RemoveChild(item)

            match toggleItemAttachedNextPhysicsUpdate.Count with
                | 0 ->
                    ()
                | _ ->
                    toggleItemAttachedNextPhysicsUpdate
                    |> Seq.iter toggleItem
                    toggleItemAttachedNextPhysicsUpdate.Clear()

        toggleQueuedAttachedState()

        physicsProcessForcesStateMachine delta state
        |> ignore

    override this._IntegrateForces(physicsState : PhysicsDirectBodyState) =
        let delta = physicsState.Step
        integrateForcesStateMachine delta physicsState state
        |> ignore

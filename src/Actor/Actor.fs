namespace ActorManager

open System.Collections.Generic
open Godot
open Chessie.ErrorHandling
open RailwayUtils
open InputManager
open GodotUtils
open Items

type ActorState =
    | IdleState
    | MoveState
    | RunState
    | UnHolsterState
    | HolsterState
    | HoldState
    | HoldMoveState

type ActorGunState =
    | Reload
    | Fire

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

module ActorStatesUtils =
    let actorGunStateToString(actorGunState : ActorGunState) : string =
        match actorGunState with
            | Reload ->
                "Reload"
            | Fire ->
                "Fire"

type ActorObject() as this =
    inherit RigidBody()

    let handReachArea =
        lazy(this.GetNode(new NodePath("HandReachArea")) :?> Area)

    let mutable selectedItem : int = 0;

    let mutable inventorySize : int = 9;

    let changeSelectedItem (number : int) =
        match number < inventorySize && number > inventorySize with
            | false ->
                ()
            | true ->
                selectedItem <- number

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

    let setAnimation(name : string, speed : float32) =
        animatedSprite.Force().Play name
        animatedSprite.Value.GetSpriteFrames().SetAnimationSpeed(name, speed)

    let toggleItemAttachedNextPhysicsUpdate =
        new List<RigidBody>()

    let attemptToggleItemAttached (item : RigidBody) : bool =
        match toggleItemAttachedNextPhysicsUpdate.Contains item with
            | true ->
                false
            | false ->
                toggleItemAttachedNextPhysicsUpdate.Add item
                true


    ///////////////////////
    //State actions state//
    ///////////////////////

    let move (physicsState : PhysicsDirectBodyState) (multiplier : float32) =
        physicsState.ApplyImpulse(Vector3(0.0f, 0.0f, 0.0f), (Vector3 (actorButtons.MoveDirection.x, 0.0f, actorButtons.MoveDirection.y)).Normalized() * physicsMoveMultiplier * multiplier)

    let drop() =
        this.GetTree().SetInputAsHandled();
        match items.[selectedItem].IsSome with
            | false ->
                ()
            | true ->
                match attemptToggleItemAttached items.[selectedItem].Value with
                    | true ->
                        items.[selectedItem] <- None
                        GD.Print "DROP"
                    | false ->
                        ()

    let pickup() =
        this.GetTree().SetInputAsHandled();
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
                    ()
               | false ->
                   // Attempt to drop held item
                   drop()
                   match attemptToggleItemAttached a.Head with
                       | false ->
                           ()
                       | true ->
                           items.[selectedItem] <- Some (a.Head)
                           GD.Print "PICKUP")

    let aim() =
        GD.Print("Aim not implemented")
        this.GetTree().SetInputAsHandled();

    let attack() =
        GD.Print("Attack not implemented")
        this.GetTree().SetInputAsHandled();

    let mutable timer = 0.0f

    let isMoveDirectionZero () =
        actorButtons.MoveDirection.x = 0.0f && actorButtons.MoveDirection.y = 0.0f

    ///////////////
    //Idle state//
    ///////////////

    let startIdle() =
        setAnimation("Idle", 100.0f)
        None

    let updateIdle() =
        None

    let updateKeysIdle() =
        match isMoveDirectionZero() with
            | false ->
                Some MoveState
            | true ->
                    match actorButtons.AimPressed with
                        | true ->
                            Some UnHolsterState
                        | false ->
                            match actorButtons.PickupPressed with
                                | true ->
                                    pickup()
                                    None
                                | false ->
                                    match actorButtons.DropPressed with
                                        | true ->
                                            drop()
                                            None
                                        | false ->
                                            None

    ///////////////
    // Move state//
    ///////////////

    let startMove() =
        setAnimation("Move", 10.0f)
        None

    let updateMove() =
        None

    let updateKeysMove() =
        match isMoveDirectionZero() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.RunPressed with
                    | true ->
                            Some RunState
                    | false ->
                        match actorButtons.AimPressed with
                            | true ->
                                Some UnHolsterState
                            | false ->
                                match actorButtons.PickupPressed with
                                    | true ->
                                        pickup()
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

    ///////////////
    //Run state //
    ///////////////

    let startRun() =
        setAnimation("Run", 50.0f)
        None

    let updateRun() =
        None

    let updateKeysRun() =
        match isMoveDirectionZero() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.RunPressed with
                    | false ->
                        Some MoveState
                    | true ->
                        match actorButtons.AimPressed with
                            | true ->
                                Some UnHolsterState
                            | false ->
                                match actorButtons.PickupPressed with
                                    | true ->
                                        pickup()
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

    //////////////
    //Move Hold state//
    //////////////

    let startHoldMove() =
        None

    let updateHoldMove()  =
        None

    let updateKeysHoldMove() =
        match isMoveDirectionZero() with
            | true ->
                Some HoldState
            | false ->
                match actorButtons.AimPressed with
                    | false ->
                        Some HolsterState
                    | true ->
                        match actorButtons.AttackPressed with
                            | true ->
                                attack()
                                None
                            | false ->
                                match actorButtons.PickupPressed with
                                    | true ->
                                        pickup()
                                        None
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                None
                                            | false ->
                                                None

    let integrateForcesHoldMove (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (2.5f * delta)

    //////////////
    //Hold state//
    //////////////

    let startHold() =
        None

    let updateHold()  =
        None

    let updateKeysHold() =
        match isMoveDirectionZero() with
            | false ->
                Some HoldMoveState
            | true ->
                match actorButtons.AimPressed with
                    | false ->
                        Some HolsterState
                    | true ->
                        match actorButtons.AttackPressed with
                            | true ->
                                attack()
                                None
                            | false ->
                                match actorButtons.PickupPressed with
                                    | true ->
                                        pickup()
                                        None
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                None
                                            | false ->
                                                None

    /////////////////
    //Holster state//
    /////////////////

    let startHolster() =
        None

    // Holster state
    let holsterTime : float32 = 2.0f
    let updateHolster  (delta : float32)  =
        timer <- timer + delta
        if timer > holsterTime then
            Some IdleState
        else None

    let updateKeysHolster () =
        match actorButtons.AimPressed with
            | true ->
                Some HoldState
            | false ->
                match items.[selectedItem].IsSome with
                    | true ->
                        None
                    | false ->
                        Some IdleState

    let integrateForcesHolster (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (1.0f * delta)

    ///////////////////
    //Unholster state//
    ///////////////////

    let startUnholster() =
        None

    // UnHolster state
    let unHolsterTime : float32 = 2.0f
    let updateUnHolster(delta : float32) =
        timer <- timer + delta
        if timer > unHolsterTime then
            Some HoldState
        else None

    let updateKeysUnHolster () =
        match actorButtons.AimPressed with
            | true ->
                match items.[selectedItem].IsSome with
                    | true ->
                        None
                    | false ->
                        Some IdleState
            | false ->
                Some IdleState

    let integrateForcesUnHolster (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (1.0f * delta)

    let switchStateStateMachine (actorState : ActorState) :  ActorState option =
        // reset timer
        timer <- 0.0f
        match actorState with
            | IdleState -> startIdle()
            | MoveState -> startMove()
            | RunState -> startRun()
            | UnHolsterState -> startUnholster()
            | HoldState -> startHold()
            | HoldMoveState -> startHoldMove()
            | HolsterState -> startHolster()

    let updateStateMachine (delta : float32) (actorState : ActorState) =
        match actorState with
            | IdleState -> updateIdle()
            | MoveState -> updateMove()
            | RunState -> updateRun()
            | UnHolsterState -> updateUnHolster delta
            | HoldState -> updateHold()
            | HoldMoveState -> updateHoldMove()
            | HolsterState -> updateHolster delta

    let integrateForcesStateMachine  (delta : float32) (physicsState : PhysicsDirectBodyState) (actorState : ActorState)  =
        match actorState with
            | IdleState -> ()
            | MoveState -> integrateForcesMove delta physicsState
            | RunState -> integrateForcesRun delta physicsState
            | UnHolsterState -> integrateForcesUnHolster delta physicsState
            | HoldState -> ()
            | HoldMoveState -> integrateForcesHoldMove delta physicsState
            | HolsterState -> integrateForcesHolster delta physicsState

    let updateKeysStateMachine (actorState : ActorState) =
        match actorState with
            | IdleState -> updateKeysIdle()
            | MoveState -> updateKeysMove()
            | RunState -> updateKeysRun()
            | UnHolsterState -> updateKeysUnHolster()
            | HoldState -> updateKeysHold()
            | HoldMoveState -> updateKeysHoldMove()
            | HolsterState -> updateKeysHolster()

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
                                state <- x
                GD.Print("Switching actor state");
                ()
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

    override this._IntegrateForces(physicsState : PhysicsDirectBodyState) =
        let toggleQueuedAttachedState() =
            let toggleItem (item : RigidBody)=
                let itemParent = item.GetParent()
                match itemParent with
                    | null ->
                        // Drop
                        item.SetGlobalTransform (Transform (this.GetGlobalTransform().basis, this.GetGlobalTransform().origin))
                        this.Owner.AddChild item
                        // Impulse to make sure it's not sleeping, otherwise the collision somehow gets disabled and the item is bugged. Other solution is disabling "can sleep"
                        item.ApplyImpulse(Vector3(0.0f, 0.0f, 0.0f), (Vector3 (actorButtons.MoveDirection.x, 0.0f, actorButtons.MoveDirection.y)).Normalized() * throwItemForce)
                    | _ ->
                        // Pickup
                        itemParent.RemoveChild(item)

            match toggleItemAttachedNextPhysicsUpdate.Count with
                | 0 ->
                    ()
                | _ ->
                    toggleItemAttachedNextPhysicsUpdate
                    |> Seq.iter toggleItem
                    toggleItemAttachedNextPhysicsUpdate.Clear()

        toggleQueuedAttachedState()

        let delta = physicsState.Step
        integrateForcesStateMachine delta physicsState state
        |> ignore

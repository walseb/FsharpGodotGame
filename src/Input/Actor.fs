namespace ActorManager

open Godot
open Chessie.ErrorHandling
open RailwayUtils
open InputManager
open GodotUtils

type ActorState =
    | CivilState
    | UnHolsterState
    | HolsterState
    | HoldWeaponState

type ActorButtons =
    {
        mutable MoveDirection : Vector3

        mutable PickupPressed : bool
        mutable SprintPressed : bool
        mutable AttackPressed : bool
        mutable AimPressed : bool
     }

type ActorObject() as this =
    inherit RigidBody()

    let mutable state = ActorState.CivilState

    let mutable actorButtons : ActorButtons =
        {
            MoveDirection = Vector3(0.0f,0.0f,0.0f)

            PickupPressed = false
            SprintPressed = false
            AttackPressed = false
            AimPressed = false
        }

    // Used to tweak the gobal movement speed in case of gravity change, etc
    let physicsMoveMultiplier = 500.0f

    //let kinematicBody = lazy (this.GetNode(new NodePath("Mesh")).GetNode(new NodePath("Body")) :?> KinematicBody)

    let move (physicsState  : PhysicsDirectBodyState) (multiplier : float32) =
        physicsState.ApplyImpulse(Vector3(0.0f, 0.0f, 0.0f), actorButtons.MoveDirection * physicsMoveMultiplier * multiplier)

    let pickup() =
        GD.Print("Pickup not implemented")
        this.GetTree().SetInputAsHandled();

    let aim() =
        GD.Print("Aim not implemented")
        this.GetTree().SetInputAsHandled();

    let attack() =
        GD.Print("Attack not implemented")
        this.GetTree().SetInputAsHandled();

    let updateCivil() =
        None

    let mutable timer = 0.0f

    // Civil state start
    let inputCivil() =
        match actorButtons.AimPressed with
            | true ->
                Some UnHolsterState
            | false ->
                match actorButtons.PickupPressed with
                    | true ->
                        pickup()
                        None
                    | false ->
                        None

    let integrateForcesCivil (delta : float32) (physicsState  : PhysicsDirectBodyState) =
        move physicsState (1.0f * delta)

    // Hold weapon state start
    let updateHold()  =
        None

    let inputHold() =
        match actorButtons.AimPressed with
            | true ->
                Some HolsterState
            | false ->
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
                                None

    let integrateForcesHold (delta : float32) (physicsState  : PhysicsDirectBodyState) =
        move physicsState (0.8f * delta)

    // Holster state
    let holsterTime : float32 = 2.0f
    let updateHolster  (delta : float32)  =
        timer <- timer + delta
        if timer > holsterTime then
            timer <- 0.0f
            Some CivilState
        else None

    let inputHolster () =
        match actorButtons.AimPressed with
            | true ->
                timer <- 0.0f
                Some HoldWeaponState
            | false ->
                None

    let integrateForcesHolster (delta : float32) (physicsState  : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    // UnHolster state
    let unHolsterTime : float32 = 2.0f
    let updateUnHolster(delta : float32) =
        GD.Print("UNHOLSTER")
        timer <- timer + delta
        if timer > unHolsterTime then
            timer <- 0.0f
            Some HoldWeaponState
        else None

    let inputUnHolster () =
        match actorButtons.AimPressed with
            | true ->
                timer <- 0.0f
                Some CivilState
            | false ->
                None

    let integrateForcesUnHolster (delta : float32) (physicsState  : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    let updateStateMachine (delta : float32) (actorState : ActorState) =
        match actorState with
            | CivilState -> updateCivil()
            | UnHolsterState -> updateUnHolster delta
            | HoldWeaponState -> updateHold()
            | HolsterState -> updateHolster delta

    let integrateForcesStateMachine  (delta : float32) (physicsState : PhysicsDirectBodyState) (actorState : ActorState)  =
        match actorState with
            | CivilState -> integrateForcesCivil delta physicsState
            | UnHolsterState -> integrateForcesUnHolster delta physicsState
            | HoldWeaponState -> integrateForcesHold delta physicsState
            | HolsterState -> integrateForcesHolster delta physicsState

    let inputStateMachine  (actorState : ActorState) =
        match actorState with
            | CivilState -> inputCivil()
            | UnHolsterState -> inputUnHolster()
            | HoldWeaponState -> inputHold()
            | HolsterState -> inputHolster()

    let updateKeys =
        match (inputStateMachine state) with
            | Some x ->
                state <- x
                ()
            | None ->
                ()

    // Read-write property
    member this.ActorButtons
        with get () = actorButtons
        and set (value) = actorButtons <- value

    member this.ResetActorButtons() =
        actorButtons.MoveDirection <- Vector3(0.0f,0.0f,0.0f)
        actorButtons.PickupPressed <- false
        actorButtons.SprintPressed <- false
        actorButtons.AttackPressed <- false
        actorButtons.AimPressed <- false

    override this._Ready() =
        this.SetProcessInput true

    override this._Process(delta : float32) =
        //GD.Print actorButtons.MoveDirection
        match (updateStateMachine delta state) with
            | Some x ->
                state <- x
                ()
            | None ->
                ()

    override this._IntegrateForces(physicsState : PhysicsDirectBodyState) =
        let delta = physicsState.Step
        //GD.Print delta
        integrateForcesStateMachine delta physicsState state
        |> ignore

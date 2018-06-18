namespace Actor

open Godot
open Chessie.ErrorHandling
open RailwayUtils
open InputManager
open InputManager.PlayerInputActions

module StateMachine =
    type ActorState =
    | CivilState
    | UnHolsterState
    | HolsterState
    | HoldWeaponState

    module Actions =
        let Move (stateMultiplier : float32) =
            let getMoveAxis() : Vector2 =
                Vector2(0.0f, 0.0f)
                |> fun axis -> if Input.IsActionPressed PlayerInputActions.moveUp then Vector2(axis.x + 1.0f, axis.y) else axis
                |> fun axis -> if Input.IsActionPressed PlayerInputActions.moveDown then Vector2(axis.x - 1.0f, axis.y)  else axis
                |> fun axis -> if Input.IsActionPressed PlayerInputActions.moveLeft then Vector2(axis.x, axis.y + 1.0f) else axis
                |> fun axis -> if Input.IsActionPressed PlayerInputActions.moveRight then Vector2(axis.x, axis.y - 1.0f) else axis

            getMoveAxis()
            |> fun moveDir -> if Input.IsActionPressed PlayerInputActions.sprint then moveDir * 1.25f else moveDir
            |>  fun moveDir -> moveDir * stateMultiplier

        let Pickup() =
            if Input.IsActionPressed PlayerInputActions.pickup then GD.Print "Pickup isn't implemented yet"

        let Aim() =
            if Input.IsActionPressed PlayerInputActions.pickup then GD.Print "Aim isn't implemented yet"

    module UpdateCivilState =
        let mutable moveDirection = Vector2(0.0f, 0.0f)

        let UpdateState() : ActorState =
            moveDirection <- Actions.Move 1.0f
            Actions.Pickup()

            if Input.IsActionPressed PlayerInputActions.aim
            then UnHolsterState
            else CivilState

        let PhysicsUpdateState(node : KinematicBody) =
            node.MoveAndSlide(Vector3(moveDirection.x, moveDirection.y, 0.0f))

    module UpdateHoldWeaponState =
        let mutable moveDirection = Vector2(0.0f, 0.0f)
        let UpdateState() : ActorState =
            moveDirection <- Actions.Move 1.0f
            Actions.Pickup()
            HoldWeaponState

        let PhysicsUpdateState(node : KinematicBody) =
            node.MoveAndSlide(Vector3(moveDirection.x, moveDirection.y, 0.0f))

    module UpdateHolsterState =
        let mutable moveDirection = Vector2(0.0f, 0.0f)
        let UpdateState() : ActorState =
            moveDirection <- Actions.Move 1.0f
            HolsterState

        let PhysicsUpdateState(node : KinematicBody) =
            node.MoveAndSlide(Vector3(moveDirection.x, moveDirection.y, 0.0f))

    module UpdateUnHolsterState =
        let mutable moveDirection = Vector2(0.0f, 0.0f)
        let UpdateState() : ActorState =
            moveDirection <- Actions.Move 1.0f
            UnHolsterState

        let PhysicsUpdateState(node : KinematicBody) =
            node.MoveAndSlide(Vector3(moveDirection.x, moveDirection.y, 0.0f))

    let UpdateStateMachine (actorState : ActorState) : ActorState =
        match actorState with
            | CivilState -> UpdateCivilState.UpdateState()
            | UnHolsterState -> UpdateUnHolsterState.UpdateState()
            | HoldWeaponState -> UpdateHoldWeaponState.UpdateState()
            | HolsterState -> UpdateHolsterState.UpdateState()

    let PhysicsUpdateStateMachine (actorState : ActorState) (kinematicBody : KinematicBody) =
        match actorState with
            | CivilState -> UpdateCivilState.PhysicsUpdateState kinematicBody
            | UnHolsterState -> UpdateUnHolsterState.PhysicsUpdateState kinematicBody
            | HoldWeaponState -> UpdateHoldWeaponState.PhysicsUpdateState kinematicBody
            | HolsterState -> UpdateHolsterState.PhysicsUpdateState kinematicBody

type ActorObject() as this =
    inherit Node()

    let mutable state = StateMachine.ActorState.CivilState

    override this._Ready() =
        this.SetProcessInput true

    override this._Process(delta : float32) =
        StateMachine.UpdateStateMachine state
        ()

    override this._PhysicsProcess(delta : float32) =
        StateMachine.PhysicsUpdateStateMachine state
        ()
    // override this._Input(inputEvent : InputEvent) =
        // this.GetTree().SetInputAsHandled();
        // ()

        //state <- StateMachine.InputUpdateStateMachine inputEvent

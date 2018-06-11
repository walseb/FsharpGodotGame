namespace Actor

open Godot
open Chessie.ErrorHandling
open RailwayUtils
open InputManager
open InputManager.PlayerInputActions

module StateMachine =
    type ActorState =
    | CivilState
    | HoldWeaponState
    | HolsterState
    | UnHolsterState

    module UpdateCivilState =
        let mutable pressedMoveUp : bool = false
        let mutable pressedMoveDown : bool = false
        let mutable pressedMoveLeft : bool = false
        let mutable pressedMoveRight : bool = false

        let mutable pressedPickup : bool = false
        let mutable pressedSprint : bool = false

        let mutable pressedAttack : bool = false
        let mutable pressedAim : bool = false

        let UpdateCivilState() =
            1

        let PhysicsUpdateCivilState() =
            1

        let InputUpdateCivilState(inputEvent : InputEvent) =
            match inputEvent :? InputEventKey with
            | true ->
                let inputEventKey = inputEvent :?> InputEventKey

                let updateKeyState pressed (scancode : string) =
                    match scancode with
                        | moveUp ->
                            pressedMoveUp <- pressed
                        | moveDown ->
                            pressedMoveDown <- pressed
                        | moveLeft ->
                            pressedMoveLeft <- pressed
                        | moveRight ->
                            pressedMoveRight <- pressed
                        | pickup ->
                            pressedPickup <- pressed
                        | sprint ->
                            pressedSprint <- pressed
                        | attack ->
                            pressedAttack <- pressed
                        | aim ->
                            pressedAim <- pressed

                // If pressed signal, or released signal
                match inputEventKey.Pressed with
                    | true ->
                        OS.GetScancodeString(inputEventKey.Scancode)
                        |> updateKeyState true
                    | false ->
                        OS.GetScancodeString(inputEventKey.Scancode)
                        |> updateKeyState false
            // Is not key, fix later
            | false ->
                ()
            ()

    module UpdateHoldWeaponState =
        let UpdateHoldWeaponState() =
            1

        let InputUpdateHoldWeaponState(inputEvent : InputEvent) =
            1

    module UpdateHolsterState =
        let UpdateHolsterState() =
            1

        let InputUpdateHolsterState(inputEvent : InputEvent) =
            1

    module UpdateUnHolsterState =
        let UpdateUnHolsterState() =
            1

        let InputUpdateUnHolsterState(inputEvent : InputEvent) =
            1

    let UpdateStateMachine() (state : ActorState) : ActorState =
        CivilState

    let PhysicsUpdateStateMachine(state : ActorState) : ActorState =
        // match state with
        // | CivilState -> CivilState.PhysicsUpdateCivilState() |> ignore
        // | _ -> 1 |> ignore
         CivilState

    let InputUpdateStateMachine(inputEvent : InputEvent) : ActorState =
        UpdateCivilState.InputUpdateCivilState inputEvent
        CivilState


type ActorObject() as this =
    inherit Node()

    let mutable state : StateMachine.ActorState =
        StateMachine.CivilState


    override this._Ready() =
        this.SetProcessInput true


    override this._Process(delta : float32) =
        1 |> ignore

    override this._Input(inputEvent : InputEvent) =
        this.GetTree().SetInputAsHandled();
        StateMachine.InputUpdateStateMachine inputEvent |> ignore
        ()

        //state <- StateMachine.InputUpdateStateMachine inputEvent

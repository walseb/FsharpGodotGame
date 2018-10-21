namespace Input.Management

open Chessie.ErrorHandling
open RailwayUtils
open Godot
open GodotUtils

module PlayerInputActions =
    let MoveUp = "MoveUp"
    let MoveDown = "MoveDown"
    let MoveLeft = "MoveLeft"
    let MoveRight = "MoveRight"
    let Pickup = "Pickup"
    let Reload = "Reload"
    let Drop = "Drop"
    let Run = "Run"
    let Attack = "Attack"
    let Aim = "Aim"
    // Camera
    let CameraAttach = "CameraAttach"
    let CameraDetach = "CameraDetach"
    let ZoomIn = "ZoomIn"
    let ZoomOut = "ZoomOut"
    let AddToUnderAttachedCommandDebug = "AddToUnderAttachedCommandDebug"
    let Select1 = "Select1"
    let Select2 = "Select2"
    let Select3 = "Select3"
    let Select4 = "Select4"
    let Select5 = "Select5"
    let Select6 = "Select6"
    let Select7 = "Select7"
    let Select8 = "Select8"
    let Select9 = "Select9"
    let Select0 = "Select0"

    let allActionNames : string array =
        [|
            MoveUp
            MoveDown
            MoveLeft
            MoveRight
            Pickup
            Reload
            Drop
            Run
            Attack
            Aim
            // Camera
            CameraDetach
            CameraAttach
            ZoomIn
            ZoomOut
            AddToUnderAttachedCommandDebug

            // Selections
            Select1
            Select2
            Select3
            Select4
            Select5
            Select6
            Select7
            Select8
            Select9
            Select0
        |]

module InputHandling =
    let InputEventTee (inputEvent : InputEvent) keyAction mouseButtonAction =
        match inputEvent :? InputEventKey with
            | true ->
                keyAction()
                |> ok 
            | false ->
                match inputEvent :? InputEventMouseButton with
                    | true ->
                        mouseButtonAction()
                        |> ok 
                    | false ->
                        fail "Input is not bindable"

    let ScancodeToReadable (inputEvent : InputEvent) scancode =
        let buttonIndexToReadable (buttonIndex : int) =
            match buttonIndex with
                | 1 -> "Left Mouse Button"
                | 2 -> "Right Mouse Button"
                | 3 -> "Middle Mouse Button"
                // Wheel
                | 4 -> "Mouse wheel up"
                | 5 -> "Mouse wheel down"
                | 6 -> "Mouse wheel left"
                | 7 -> "Mouse wheel right"
                | _ -> "Unknown mouse key"

        InputEventTee inputEvent ( fun _ -> (OS.GetScancodeString scancode)) ( fun _ -> (buttonIndexToReadable scancode))

    let InputEventToScancode (inputEvent : InputEvent) =
        InputEventTee inputEvent ( fun _ -> ((inputEvent :?> InputEventKey).Scancode)) (fun _ -> ((inputEvent :?> InputEventMouseButton).GetButtonIndex()))

    let inputEventToReadable (inputEvent : InputEvent) =
        inputEvent
        |> InputEventToScancode
        |> bind (ScancodeToReadable inputEvent)

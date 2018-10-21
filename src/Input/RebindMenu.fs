namespace Input.Rebind

open Godot
open GodotUtils
open RailwayUtils
open Chessie.ErrorHandling
open Input
open Input.ConfigManagement
open Input.Management

// GUI for handling rebinds
type RebindMenu() as this =
    inherit Node()

    let mutable action = ""
    let label = this.GetNode ("contextual_help")

    // Back button
    let _on_BackButton_pressed() : unit =
        this.GetTree().ChangeScene("res://Assets/Scenes/MainMenu/MainMenu.tscn") |> ignore

    let getButton action =
        this.GetNode(new NodePath("bindings")).GetNode(new NodePath(action)).GetNode(new NodePath("Button")) :?> Button

    let startPolling (actionKey : string)=
        action <- actionKey
        (label.Force() : Label).Text <- ("Press a key to assign to the '" + actionKey + "' action.")
        this.SetProcessInput true

    let stopPolling() =
        (label.Force() : Label).Text <- "Click a key binding to reassign it, or press the Cancel action."
        this.SetProcessInput false

    let changeButtonText actionName (inputEvent : InputEvent) =
        let button = getButton actionName
        InputHandling.inputEventToReadable inputEvent
        |> map (fun (actionName : string) ->  button.SetText(actionName))
        |> map (fun _ -> button)

    override this._Input(inputEvent : InputEvent) =
        let registerEvent() =
            // A legitimate key has been pressed (I.E a key that isn't mouse movement)
            stopPolling()

            match inputEvent.IsAction "ui_cancel" with
                | true ->
                    ok ()
                | false ->
                    ConfigManagement.ConfigFileIO.LoadConfigWithFileSystemConfig()
                    |> bind (ConfigManagement.AddKeyToConfig action inputEvent)
                    |> bind (ConfigManagement.ConfigFileIO.WriteConfigToFileSystem)
                    |> map (fun _ -> inputEvent)
                    |> bind (changeButtonText action)
                    // Workaround, ok result needs to be nil
                    |> map (fun _ -> ())

        this.GetTree().SetInputAsHandled();

        InputHandling.InputEventTee inputEvent registerEvent registerEvent
        |> logErr
        |> ignore

    override this._Ready() =
        stopPolling()

        let updateButton controllerLayerIndex actionName =
            let inputEvent = InputMap.GetActionList(actionName).[controllerLayerIndex] :?> InputEvent

            let setupButton (inputEvent : InputEvent) =
                let handleConnectError (error : Error) =
                    match error with
                    | Error.Ok ->
                        ok ()
                    | Error.DoesNotExist -> fail "Could not find button with action name, object is missing in rebind scene"
                    | Error.LinkFailed -> fail "Link connect failed"
                    | _ -> fail "Rebind menu can't connect button"

                inputEvent
                |> changeButtonText actionName
                |> map (fun button ->
                        let actionNameArray : Array = new Array()
                        actionNameArray.Add actionName
                        handleConnectError (button.Connect("pressed", this, "startPolling", actionNameArray))
                        |> logErr
                        |> ignore)

            InputHandling.InputEventTee inputEvent (fun _ -> setupButton(inputEvent)) (fun _ -> (setupButton inputEvent))
            |> logErr
            |> ignore

        ConfigManagement.LoadOrCreateConfig()

        PlayerInputActions.allActionNames
        |> Array.iter (fun (actionName : string) -> (updateButton 0 actionName))

        this.SetProcessInput(false)

namespace InputManager

open Microsoft.FSharp.Reflection

open Chessie.ErrorHandling
open RailwayUtils
open Godot
open GodotUtils

module PlayerInputActions =
    type PlayerActions =
        // Movement
        | MoveUp
        | MoveDown
        | MoveLeft
        | MoveRight
        // Civil
        | Pickup
        | Sprint
        // Uncivil
        | Attack
        | Aim

    let allActionNames : string array =
        [|
            "MoveUp"
            "MoveDown"
            "MoveLeft"
            "MoveRight"
            "Pickup"
            "Sprint"
            "Attack"
            "Aim"
         |]

    let getPlayerActionName (action : PlayerActions)  : string =
        match action with
        | PlayerActions.MoveUp -> allActionNames.[0]
        | PlayerActions.MoveDown -> allActionNames.[1]
        | PlayerActions.MoveLeft -> allActionNames.[2]
        | PlayerActions.MoveRight -> allActionNames.[3]
        | PlayerActions.Pickup -> allActionNames.[4]
        | PlayerActions.Sprint -> allActionNames.[5]
        | PlayerActions.Attack -> allActionNames.[6]
        | PlayerActions.Aim -> allActionNames.[7]

module ConfigFileManagement =
    module Internal =
        let configPath = "user://input.cfg"

        let HandleLoadError (error : Error) config =
            match error with
            | Error.Ok ->
                ok config
            | Error.FileAlreadyInUse -> fail "File already in use"
            | Error.FileNoPermission -> fail "No permission to write"
            | _ -> fail "Can't write config"

        let LoadConfigFromInputMap (config : ConfigFile)=
            HandleLoadError (config.Load(configPath)) config

        let WriteConfigToFileSystem (config : ConfigFile) =
            HandleLoadError (config.Save(configPath)) config

        let ClearDuplicateInputMapEvents actionName =
            let removeEvent actionName (eventToRemove : InputEvent)  =
                InputMap.ActionEraseEvent(actionName, eventToRemove)

            InputMap.GetActionList(actionName)
            |> Array.iter (fun currentEvent -> if currentEvent :? InputEventKey then removeEvent actionName (currentEvent :?> InputEventKey))

        let AddEventToInputMap actionName actionEvent =
            InputMap.ActionAddEvent(actionName, actionEvent)

        let GetDefaultConfig ( config: ConfigFile) : ConfigFile=
            let addActionNameToConfig (config : ConfigFile) actionName controllerLayerIndex =
                let inputEvent = InputMap.GetActionList(actionName).[controllerLayerIndex]
                if inputEvent :? InputEventKey then
                    let scancode = OS.GetScancodeString((inputEvent :?> InputEventKey).Scancode)
                    config.SetValue("input", actionName, scancode)

            let config = new ConfigFile()

            PlayerInputActions.allActionNames
            |> Array.iter (fun actionName -> addActionNameToConfig config actionName 0)
            config

        let CopyConfigToInputMap (config : ConfigFile) =
            let addActionToInputMap (config : ConfigFile) actionName =
                let createEvent scancode =
                    let newEvent = new InputEventKey();
                    newEvent.Scancode <- scancode
                    newEvent

                let getScancodeIfActionExists (config : ConfigFile) actionName =
                    let scancodeString = config.GetValue("input", actionName) :?> string

                    match scancodeString with
                    | null -> fail ("No such action in config: " + actionName)
                    | _ -> OS.FindScancodeFromString(scancodeString) |> ok

                ClearDuplicateInputMapEvents actionName

                getScancodeIfActionExists config actionName
                |> map createEvent
                |> map (AddEventToInputMap actionName)

            PlayerInputActions.allActionNames
            |> Array.iter (fun string -> addActionToInputMap config string |> log |> ignore)
            config

    let LoadOrCreateConfig =
        let load (config : ConfigFile) =
            config
            |> Internal.CopyConfigToInputMap
            |> ok

        let create (config : ConfigFile) =
            config
            |> Internal.GetDefaultConfig
            |> Internal.CopyConfigToInputMap
            |> Internal.WriteConfigToFileSystem

        let config = new ConfigFile()

        match config.Load(Internal.configPath) with
        | Error.Ok ->
            load config
        | _ ->
            create config

    let AddKeyToConfig (key : InputEventKey) action =
        let updateConfigWithCurrentMaps (config : ConfigFile) =
            Internal.LoadConfigFromInputMap config
            |> log
            |> ignore
            config

        let setValueInConfig action scancode (config : ConfigFile) : ConfigFile =
            config.SetValue("input", action, scancode)
            config

        Internal.ClearDuplicateInputMapEvents action
        Internal.AddEventToInputMap action key

        let scancode = OS.GetScancodeString(key.Scancode)

        new ConfigFile()
        |> updateConfigWithCurrentMaps
        |> setValueInConfig action scancode
        |> Internal.WriteConfigToFileSystem
        |> log
        |> ignore
        key

// GUI for handling rebinds
type RebindMenu() as this =
    inherit Node()

    let mutable action = ""
    let label = this.getNode ("contextual_help")

    // Back button
    let _on_BackButton_pressed() : unit =
        this.GetTree().ChangeScene("res://Assets/Scenes/MainMenu/MainMenu.tscn") |> ignore

    let getButton action =
        this.GetNode(new NodePath("bindings")).GetNode(new NodePath(action)).GetNode(new NodePath("Button")) :?> Button

    let waitForInput (actionKey : string)=
        action <- actionKey

        (label.Force() : Label).Text <- ("Press a key to assign to the '" + actionKey + "' action.")

        this.SetProcessInput true

    override this._Input(inputEvent : InputEvent) =

        match inputEvent :? InputEventKey with
        | true ->
            let changeButtonText (key : InputEventKey) =
                let scancode = OS.GetScancodeString(key.Scancode)
                (getButton action).Text <- scancode

            this.GetTree().SetInputAsHandled();
            this.SetProcessInput false

            (label.Force() : Label).Text <- "Click a key binding to reassign it, or press the Cancel action."

            match inputEvent.IsAction "ui_cancel" with
            | true ->
                ()
            | false ->
                ConfigFileManagement.AddKeyToConfig(inputEvent :?> InputEventKey) action
                |> changeButtonText
                |> ignore
        | false ->
            GD.Print("WARNING: Input is not a key");

    override this._Ready() =
        ConfigFileManagement.LoadOrCreateConfig
        |> ignore

        let updateButton controllerLayerIndex actionName =
            let inputEvent = InputMap.GetActionList(actionName).[controllerLayerIndex]

            match inputEvent :? InputEventKey with
            | true ->
                let button = getButton actionName
                button.Text <- OS.GetScancodeString((inputEvent :?> InputEventKey).Scancode)
                button.Connect("pressed", this, "waitForInput", [|actionName|]) |> ignore
            | false ->
                GD.Print("WARNING: Key in default input map isn't a key")

        PlayerInputActions.allActionNames
        |> Array.iter (fun (actionName : string) -> (updateButton 0 actionName))

        this.SetProcessInput false

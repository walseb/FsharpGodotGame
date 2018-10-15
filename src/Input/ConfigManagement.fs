namespace Input.ConfigManagement

open Chessie.ErrorHandling
open RailwayUtils
open Godot
open GodotUtils
open Input.Management

module ConfigManagement =
    let ConfigFileToInputMap (config : ConfigFile) =
        let removeInputEventsWithAction (actionName : string) =
            let removeEvent actionName (eventToRemove : InputEvent)  =
                InputMap.ActionEraseEvent(actionName, eventToRemove)

            InputMap.GetActionList(actionName)
            |> Seq.iter (fun currentEvent -> if currentEvent :? InputEventKey then removeEvent actionName (currentEvent :?> InputEventKey))

            InputMap.GetActionList(actionName)
            |> Seq.iter (fun currentEvent -> if currentEvent :? InputEventMouseButton then removeEvent actionName (currentEvent :?> InputEventMouseButton))

        let addAction(config : ConfigFile) actionName =
            let getInputEventFromConfig (config : ConfigFile) actionName =
                let createInputEventKey scancode =
                    let newEvent = new InputEventKey();
                    newEvent.Scancode <- scancode
                    newEvent

                let createInputEventMouseButton scancode =
                    let newEvent = new InputEventMouseButton();
                    newEvent.ButtonIndex <- scancode
                    newEvent

                let getScancodeFromConfig actionName =
                    let value = config.GetValue("input", actionName)
                    match value :? int with
                    | true ->
                        ok (value :?> int)
                    | false ->
                        fail "Config corrupted"

                let scancodeToInputEvent scancode =
                    if scancode > 7
                    then (createInputEventKey scancode :> InputEvent)
                    else (createInputEventMouseButton scancode :> InputEvent)

                getScancodeFromConfig actionName
                |> map scancodeToInputEvent

            let addEventToInputMap actionName (actionEvent : InputEvent) =
                InputMap.ActionAddEvent(actionName, actionEvent)

            getInputEventFromConfig config actionName
            |> map (addEventToInputMap actionName)

        let mutable failText = ""
        GD.Print("1")
        PlayerInputActions.allActionNames
        // First remove all events with action, then add action event to action
        |> Array.iter (fun action ->
                        removeInputEventsWithAction action
                        GD.Print "OVER"
                        addAction config action
                        // If fail
                        |> failureTee (fun text -> (failText <- (String.concat "" text)))
                        |> logErr
                        |> ignore
                        |> ignore)


        match failText with
            | "" -> ok config
            | _ -> fail failText

    module ConfigFileIO =
        let configPath = "user://input.cfg"

        let HandleIOError (error : Error) =
            match error with
            | Error.Ok ->
                ok ()
            | Error.FileAlreadyInUse -> fail "File already in use"
            | Error.FileNoPermission -> fail "No permission to write"
            | Error.DoesNotExist -> fail "Config does not exist"
            | _ -> fail "Can't write config"

        let WriteConfigToFileSystem (config : ConfigFile) =
            HandleIOError (config.Save(configPath))
            |> map (fun _ -> config)

        let LoadConfigWithFileSystemConfig() =
            let config = new ConfigFile()

            HandleIOError (config.Load(configPath))
            |> map (fun _ -> config)

    let AddKeyToConfig action (inputEvent : InputEvent) (config : ConfigFile)=
        let setValueInConfig action (config : ConfigFile) =
            InputHandling.InputEventToScancode inputEvent
            |> map (fun scancode -> (config.SetValue("input", action, scancode)))
            // We need to return a ConfigFile
            |> map (fun _ -> config)

        setValueInConfig action config

    let GetDefaultConfig ( config: ConfigFile) : ConfigFile=
        let addActionNameToConfig (config : ConfigFile) actionName controllerLayerIndex =
            let inputEvent = (InputMap.GetActionList(actionName).[controllerLayerIndex] :?> InputEvent)
            AddKeyToConfig actionName inputEvent config
            |> bind ConfigFileIO.WriteConfigToFileSystem

        let config = new ConfigFile()

        // Just in case any input maps have been changed, add those back
        InputMap.LoadFromGlobals()

        PlayerInputActions.allActionNames
        |> Array.iter (fun actionName -> addActionNameToConfig config actionName 0 |> logErr |> ignore)
        config

    let LoadOrCreateConfig() =
        let load (config : ConfigFile) =
            config
            |> ConfigFileToInputMap

        let create (config : ConfigFile) =
            config
            |> GetDefaultConfig
            |> ConfigFileToInputMap
            |> bind ConfigFileIO.WriteConfigToFileSystem

        let config = new ConfigFile()

        match config.Load(ConfigFileIO.configPath) with
        | Error.Ok ->
            match (load config) with
                | Bad(msgs) ->
                    create config
                    |> logErr
                    |> ignore
                | _ ->
                    ()
        | _ ->
            create config
            |> logErr
            |> ignore

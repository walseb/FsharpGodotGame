namespace Scenes.MainMenu

open Godot
open Input.ConfigManagement

type MainMenuButtons() as this =
    inherit VBoxContainer()

    let _on_start_pressed() : unit =
        ConfigManagement.LoadOrCreateConfig() |> ignore
        this.GetTree().ChangeScene("res://Assets/Scenes/TestScene.tscn") |> ignore

    let _on_keys_pressed() : unit =
        this.GetTree().ChangeScene("res://Assets/Scenes/MainMenu/RebindKeys/RebindKeys.tscn") |> ignore

    let _on_options_pressed() : unit =
        GD.Print "Button not implemented yet" |> ignore

    let _on_exit_pressed() : unit =
        this.GetTree().Quit();

namespace CameraManager

open Godot
open InputManager

module CameraControls =
    let Zoom(zoomAmount : int) =
        ()

    let ZoomOut() =
        ()

type CameraObject() as this =
    inherit Camera()

    override this._UnhandledInput(inputEvent : InputEvent) =
        //match InputManager.ConfigManagement.ScancodeHandling.inputEventToReadable inputEvent
        ()

    // override this._Process(delta : float32) =

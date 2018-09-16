namespace PlayerManager

open LanguagePrimitives

open Godot
open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils
open ActorManager
open InputManager

open System.Collections.Generic

type CameraButtons =
    {
        mutable MoveUpPressed : bool
        mutable MoveDownPressed : bool
        mutable MoveLeftPressed : bool
        mutable MoveRightPressed : bool

        mutable RunPressed : bool
     }

type PlayerCamera() as this =
    inherit Camera()

    let mutable commander : ActorObject Option =
        None

    let mutable attachedActor : ActorObject Option =
        None

    let mutable attachActorNextPhysicsUpdate =
        false

    /// Workaround because the "area" node is used for selection and it can't be read the same update/frame it's moved, move it one update/frame before
    let attemptGetActorTimeout =
        3
    let mutable attemptGetActorTimeoutTimer =
        0

    let mutable detachActorNextPhysicsUpdate =
        false

    let mutable addActorInSelectionToUnderCommandNextPhysicsUpdate =
        false

    let mutable initObjectReferences =
        true

    let mouseSelectionArea : Lazy<Area> =
        lazy (this.GetParent().GetNode(new NodePath("MouseSelectedArea")) :?> Area)

    let areaCollisionShape : Lazy<CollisionShape> =
        lazy (mouseSelectionArea.Force().GetChild 0 :?> CollisionShape)

    let mutable cameraButtons : CameraButtons =
        {
            MoveUpPressed = false
            MoveDownPressed = false
            MoveLeftPressed = false
            MoveRightPressed = false
            RunPressed = false
        }

    ///////////////////////////////////////////////////////////////////////////////
    //                                  Functions                                //
    ///////////////////////////////////////////////////////////////////////////////

    let getMoveAxis() : Vector2 =
        Vector2(0.0f, 0.0f)
        |> fun axis -> if cameraButtons.MoveUpPressed then Vector2(axis.x, axis.y - 1.0f) else axis
        |> fun axis -> if cameraButtons.MoveDownPressed then Vector2(axis.x, axis.y + 1.0f)  else axis
        |> fun axis -> if cameraButtons.MoveLeftPressed then Vector2(axis.x - 1.0f,axis.y) else axis
        |> fun axis -> if cameraButtons.MoveRightPressed then Vector2(axis.x + 1.0f, axis.y) else axis

    let inputHandled() =
        this.GetTree().SetInputAsHandled()
        attachedActor.Value.InputUpdated()

    let setMovementButtons (inputEvent : InputEvent) =
        match inputEvent.IsAction PlayerInputActions.MoveUp with
            | true ->
                cameraButtons.MoveUpPressed <- inputEvent.IsPressed()
                inputHandled()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.MoveDown with
                    | true ->
                        cameraButtons.MoveDownPressed <- inputEvent.IsPressed()
                        inputHandled()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.MoveLeft with
                            | true ->
                                cameraButtons.MoveLeftPressed <- inputEvent.IsPressed()
                                inputHandled()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.MoveRight with
                                    | true ->
                                        cameraButtons.MoveRightPressed <- inputEvent.IsPressed()
                                        inputHandled()
                                        true
                                    | false ->
                                        false

    ///////////////////////////////////////////////////////////////////////////////
    //                                  Camera Actions                           //
    ///////////////////////////////////////////////////////////////////////////////

    // Might help get global position: mouseSelectionArea.Force().origin
    let moveSelectAreaToMouse() =
        let raycastFromMouse() : Vector3 option =
            let maxRayRange = 1000.0f

            let mousePosition = this.GetViewport().GetMousePosition()

            // Get from and to positions for ray to use
            let rayFrom = this.ProjectRayOrigin (mousePosition)
            let rayTo = rayFrom + this.ProjectRayNormal (mousePosition) * maxRayRange

            // Shoot the physics based ray
            let resultRay =
                let spaceState = this.GetWorld().GetDirectSpaceState()
                spaceState.IntersectRay(rayFrom, rayTo, null, 2)

            match resultRay.Count > 0 with
                | false ->
                    None
                | true ->
                    Some (resultRay.["position"] :?> Vector3)

        raycastFromMouse()
        |> fun mousePos ->
            match mousePos with
                | None ->
                    ()
                | Some x ->
                    //let child = mouseSelectionArea.Force().GetChild 0 :?> Spatial
                    areaCollisionShape.Force().SetGlobalTransform (Transform (mouseSelectionArea.Force().GetGlobalTransform().basis, mousePos.Value))
                   // mouseSelectionArea.Force().SetGlobalTransform (new Transform (mouseSelectionArea.Force().GetGlobalTransform().basis, mousePos.Value))

    /// Returns list sorted by distance (closest first)

    let getActorsInSelection() : ActorObject list option =
        let list = sortObjectListClosestFirst (mouseSelectionArea.Force().GetOverlappingBodies()) (areaCollisionShape.Force())
        match list.Length with
            | 0 ->
                None
            | _ ->
                list
                |> List.choose (fun a ->
                            match a :? ActorObject with
                            | true ->
                                Some (a :?> ActorObject)
                            | false ->
                                None)
                |> (fun a -> Some a)

    let makeSureCommanderIsSome() =
        match commander.IsSome with
            | true ->
                ()
            | false ->
                let findCommander() =
                    Some (this.GetTree().GetRoot().GetNode(new NodePath "Spatial").GetNode(new NodePath "Actor") :?> ActorObject)
                let foundCommander = findCommander()
                match foundCommander.IsSome with
                    | true ->
                        commander <- foundCommander
                    | false ->
                        GD.Print "There isn't a commander on the scene"

    let getClosestAttachableActor(actorList : ActorObject list) =
        let isAttachable (actor : ActorObject) =
            // Check if actor already is selected, the player wants to switch to annother one, not stay on the same
                    match attachedActor.IsSome && (PhysicalEquality actor attachedActor.Value) with
                    | true ->
                        false
                    | false ->
                        let mutable possibleActors = commander.Value.CommandChildren
                        possibleActors.Add commander.Value

                        possibleActors
                        // Only if
                        |> Seq.tryFind (PhysicalEquality actor)
                        |> fun actor ->
                            actor.IsSome
        makeSureCommanderIsSome()
        actorList
        |> Seq.tryFind isAttachable

    let addActorInSelectionToUnderCommand() =
        makeSureCommanderIsSome()
        let selectedActors = getActorsInSelection()
        match selectedActors.IsSome with
            | false ->
                ()
            | true ->
                match commander.Value.CommandChildren.Contains selectedActors.Value.Head && PhysicalEquality commander.Value selectedActors.Value.Head with
                    | true ->
                        ()
                    | false ->
                        commander.Value.AddActorUnderCommand selectedActors.Value.Head

    let attachActor (actor : ActorObject) =
        let transferCameraButtonStateToActor (actor : ActorObject) =
            actor.ActorButtons.MoveDirection <- getMoveAxis()
            actor.ActorButtons.RunPressed <- cameraButtons.RunPressed

        let makeActorParent (actor : ActorObject) =
            (this.GetParent()).RemoveChild this
            actor.AddChild this
            this.SetOwner actor

        match attachedActor.IsSome with
            | true ->
                attachedActor.Value.ResetActorButtons()
            | false ->
                ()

        makeActorParent actor
        // Detach current actor properly before attaching a new one
       // detachActor actor
        attachedActor <- Some actor
        transferCameraButtonStateToActor actor

    let attachActorUnderSelection() =
        let actorsInSelection = getActorsInSelection()
        match actorsInSelection.IsSome with
            | false ->
                ()
            | true ->
                let closestAttachableActor = getClosestAttachableActor actorsInSelection.Value
                match closestAttachableActor.IsSome with
                    | false ->
                        ()
                    | true ->
                        attachActor closestAttachableActor.Value

    let attachCommander() =
        match commander.IsSome with
            | true ->
                attachActor commander.Value
            | false ->
                GD.Print "ERROR: NO COMMANDER IS ATTACHED!!!"

    let zoom (zoomAmount : float32) =
        let zoomCamera (parent : ActorObject) =
            let maxZoomHeight = 40.0f
            let minZoomHeight = 5.0f

            let actorLocalTransform = this.GetTransform()
            let actorCameraHeightDelta = actorLocalTransform.origin.y
            // let cameraPos = this.GetGlobalTransform().origin
            // let actorPos = attachedActor.Value.GetGlobalTransform().origin
            // let actorCameraHeightDelta = cameraPos.y - actorPos.y
            GD.Print actorCameraHeightDelta

            if ((actorCameraHeightDelta > maxZoomHeight) || (actorCameraHeightDelta < minZoomHeight))
            then this.SetTransform (new Transform (actorLocalTransform.basis, Vector3(0.0f, minZoomHeight, 0.0f)))
            // If the result wouldn't go outside the max and min height
            else (if ((actorCameraHeightDelta + zoomAmount < maxZoomHeight) && (actorCameraHeightDelta + zoomAmount > minZoomHeight))
            // Then increase global .z position with zoom amount
            //then this.SetGlobalTransform (new Transform (this.GetGlobalTransform().basis, cameraPos + Vector3(0.0f,zoomAmount ,0.0f)))
                then this.SetTransform (new Transform (actorLocalTransform.basis, actorLocalTransform.origin + Vector3(0.0f, zoomAmount, 0.0f))))

            // FIX make camera reset if outside of range
            // TODO Make this work, it broke because i made camera not root of player camera scene

        match attachedActor with
            | None ->
                match commander with
                    | None ->
                        ()
                    | Some x ->
                        zoomCamera commander.Value
            | Some x ->
                zoomCamera attachedActor.Value
                //GD.Print ("Zoomed by: ", zoomAmount)

    /// Buttons
    let setCameraSpecialButtons(inputEvent : InputEvent) =
        match inputEvent.IsAction PlayerInputActions.ZoomIn with
            | true ->
                zoom -1.0f
                inputHandled()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.ZoomOut with
                    | true ->
                        zoom 1.0f
                        inputHandled()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.CameraAttach with
                        | true ->
                            match inputEvent.IsPressed() with
                            | true ->
                                attachActorNextPhysicsUpdate <- true
                            | false ->
                                ()
                            inputHandled()
                            true
                        | false ->
                            match inputEvent.IsAction PlayerInputActions.CameraDetach with
                            | true ->
                                match inputEvent.IsPressed() with
                                | true ->
                                    detachActorNextPhysicsUpdate <- true
                                | false ->
                                    ()
                                inputHandled()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.AddToUnderAttachedCommandDebug with
                                | true ->
                                    match inputEvent.IsPressed() with
                                    | true ->
                                        addActorInSelectionToUnderCommandNextPhysicsUpdate <- true
                                    | false ->
                                        ()
                                    inputHandled()
                                    true
                                | false ->
                                    false

    let setActorSelectionButtons (inputEvent : InputEvent) (actorButtons : Lazy<ActorButtons>) : bool =
        match inputEvent.IsAction PlayerInputActions.Select1 with
            | true ->
                actorButtons.Force().Select1Pressed <- inputEvent.IsPressed()
                inputHandled()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.Select2 with
                    | true ->
                        actorButtons.Force().Select2Pressed <- inputEvent.IsPressed()
                        inputHandled()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.Select3 with
                            | true ->
                                actorButtons.Force().Select3Pressed <- inputEvent.IsPressed()
                                inputHandled()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.Select4 with
                                    | true ->
                                        actorButtons.Force().Select4Pressed <- inputEvent.IsPressed()
                                        inputHandled()
                                        true
                                    | false ->
                                        match inputEvent.IsAction PlayerInputActions.Select5 with
                                            | true ->
                                                actorButtons.Force().Select5Pressed <- inputEvent.IsPressed()
                                                inputHandled()
                                                true
                                            | false ->
                                                match inputEvent.IsAction PlayerInputActions.Select6 with
                                                    | true ->
                                                        actorButtons.Force().Select6Pressed <- inputEvent.IsPressed()
                                                        inputHandled()
                                                        true
                                                    | false ->
                                                        match inputEvent.IsAction PlayerInputActions.Select7 with
                                                            | true ->
                                                                actorButtons.Force().Select7Pressed <- inputEvent.IsPressed()
                                                                inputHandled()
                                                                true
                                                            | false ->
                                                                match inputEvent.IsAction PlayerInputActions.Select8 with
                                                                    | true ->
                                                                        actorButtons.Force().Select8Pressed <- inputEvent.IsPressed()
                                                                        inputHandled()
                                                                        true
                                                                    | false ->
                                                                        match inputEvent.IsAction PlayerInputActions.Select9 with
                                                                            | true ->
                                                                                actorButtons.Force().Select9Pressed <- inputEvent.IsPressed()
                                                                                inputHandled()
                                                                                true
                                                                            | false ->
                                                                                match inputEvent.IsAction PlayerInputActions.Select0 with
                                                                                    | true ->
                                                                                        actorButtons.Force().Select0Pressed <- inputEvent.IsPressed()
                                                                                        inputHandled()
                                                                                        true
                                                                                    | false ->
                                                                                        false

    let setActorButtons (inputEvent : InputEvent) (actorButtons : Lazy<ActorButtons>) : bool =
        match setMovementButtons inputEvent with
            | false ->
                match inputEvent.IsAction PlayerInputActions.Pickup with
                    | true ->
                        actorButtons.Force().PickupPressed <- inputEvent.IsPressed()
                        inputHandled()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.Drop with
                            | true ->
                                actorButtons.Force().DropPressed <- inputEvent.IsPressed()
                                inputHandled()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.Attack with
                                    | true ->
                                        actorButtons.Force().AttackPressed <- inputEvent.IsPressed()
                                        inputHandled()
                                        true
                                    | false ->
                                        match inputEvent.IsAction PlayerInputActions.Aim with
                                            | true ->
                                                actorButtons.Force().AimPressed <- inputEvent.IsPressed()
                                                inputHandled()
                                                true
                                            | false ->
                                                match inputEvent.IsAction PlayerInputActions.Run with
                                                    | true ->
                                                        actorButtons.Force().RunPressed <- inputEvent.IsPressed()
                                                        inputHandled()
                                                        true
                                                    | false ->
                                                        setActorSelectionButtons inputEvent actorButtons
            | true ->
                actorButtons.Force().MoveDirection <- getMoveAxis()
                inputHandled()
                true


    override this._Ready() =
        makeSureCommanderIsSome()
        // //this.CallDeferred("detachActorAttachCommander")
        // // Zoom out
        zoom 5.0f
        ()

    override this._PhysicsProcess(delta : float32) =
        match initObjectReferences with
            | true ->
                initObjectReferences <- false
                mouseSelectionArea.Force() |> ignore
                areaCollisionShape.Force() |> ignore
                attachActor commander.Value
                ()
            | false ->
                ()

        match detachActorNextPhysicsUpdate with
            | true ->
                detachActorNextPhysicsUpdate <- false
                attachCommander()
            | false ->
                ()

        // Newer way
        match addActorInSelectionToUnderCommandNextPhysicsUpdate || attachActorNextPhysicsUpdate with
            | false ->
                ()
            | true ->
                match attemptGetActorTimeoutTimer = 0 with
                    | false ->
                        ()
                    | true ->
                        moveSelectAreaToMouse()

                match attemptGetActorTimeoutTimer > attemptGetActorTimeout with
                    | false ->
                        attemptGetActorTimeoutTimer <- attemptGetActorTimeoutTimer + 1
                        ()
                    | true ->
                        attemptGetActorTimeoutTimer <- 0

                        match addActorInSelectionToUnderCommandNextPhysicsUpdate with
                            | true ->
                                addActorInSelectionToUnderCommandNextPhysicsUpdate <- false
                                addActorInSelectionToUnderCommand()
                            | false ->
                                ()

                        match attachActorNextPhysicsUpdate with
                            | true ->
                                attachActorNextPhysicsUpdate <- false
                                attachActorUnderSelection()
                            | false ->
                                ()

    override this._UnhandledInput (inputEvent : InputEvent) =
        setCameraSpecialButtons inputEvent
        |> ignore

        match attachedActor with
            | Some x ->
                setActorButtons inputEvent (lazy attachedActor.Value.ActorButtons)
                |> ignore
                ()
            | None ->
                GD.Print "No actor attached???"
                setMovementButtons inputEvent
                |> ignore

        //GD.Print ("attachedActor", attachedActor.Value)

namespace Actor.Camera

open References
open Godot
open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils
open Actor
open Input.Management

open System.Collections.Generic

type MoveDirection =
    {
        mutable MoveUpPressed : bool
        mutable MoveDownPressed : bool
        mutable MoveLeftPressed : bool
        mutable MoveRightPressed : bool
     }


type PlayerCamera() as this =
    inherit Camera()

    // * Vars

    let mutable cameraButtons : MoveDirection =
        {
            MoveUpPressed = false
            MoveDownPressed = false
            MoveLeftPressed = false
            MoveRightPressed = false
        }

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

    let mutable zoomLevel : float32 =
        5.0f

    let rootNode : Lazy<Spatial> =
        lazy (this.GetParentSpatial())

    let mouseSelectionArea : Lazy<Area> =
        lazy (this.GetParent().GetNode(new NodePath("MouseSelectedArea")) :?> Area)

    let areaCollisionShape : Lazy<CollisionShape> =
        lazy (mouseSelectionArea.Force().GetChild 0 :?> CollisionShape)


    // * Functions

    // ** Input

    let cameraSpecificInputHandled() =
        this.GetTree().SetInputAsHandled()

    let inputHandled() =
        this.GetTree().SetInputAsHandled()
        attachedActor.Value.InputUpdated()

    // *** Movement
    let getMoveAxis() : Vector2 =
        Vector2(0.0f, 0.0f)
        |> fun axis -> if cameraButtons.MoveUpPressed then Vector2(axis.x, axis.y - 1.0f) else axis
        |> fun axis -> if cameraButtons.MoveDownPressed then Vector2(axis.x, axis.y + 1.0f)  else axis
        |> fun axis -> if cameraButtons.MoveLeftPressed then Vector2(axis.x - 1.0f,axis.y) else axis
        |> fun axis -> if cameraButtons.MoveRightPressed then Vector2(axis.x + 1.0f, axis.y) else axis

    let setMovementButtons (inputEvent : InputEvent) =
        match inputEvent.IsAction PlayerInputActions.MoveUp with
            | true ->
                cameraButtons.MoveUpPressed <- inputEvent.IsPressed()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.MoveDown with
                    | true ->
                        cameraButtons.MoveDownPressed <- inputEvent.IsPressed()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.MoveLeft with
                            | true ->
                                cameraButtons.MoveLeftPressed <- inputEvent.IsPressed()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.MoveRight with
                                    | true ->
                                        cameraButtons.MoveRightPressed <- inputEvent.IsPressed()
                                        true
                                    | false ->
                                        false

    // * Camera Actions
    let getMouse3DPosition() : Vector3 option =
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

    // Might help get global position: mouseSelectionArea.Force().origin
    let moveSelectAreaToMouse() =
        getMouse3DPosition()
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
                |> Some

    let makeSureCommanderIsSome() =
        match commander.IsSome with
            | true ->
                ()
            | false ->
                match ReferencesStored.Player.IsSome with
                    | true ->
                        commander <- Some ReferencesStored.Player.Value
                    | false ->
                        GD.Print "There isn't a commander on the scene"

    let getClosestAttachableActor(actorList : ActorObject list) =
        let isAttachable (actor : ActorObject) =
            // Check if actor already is selected, the player wants to switch to annother one, not stay on the same
                    match attachedActor.IsSome && (GodotUtils.physicallyEquals actor attachedActor.Value) with
                    | true ->
                        false
                    | false ->
                        let mutable possibleActors = commander.Value.CommandChildren
                        possibleActors.Add commander.Value

                        possibleActors
                        // Only if
                        |> Seq.tryFind (GodotUtils.physicallyEquals actor)
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
                match commander.Value.CommandChildren.Contains selectedActors.Value.Head && GodotUtils.physicallyEquals commander.Value selectedActors.Value.Head with
                    | true ->
                        ()
                    | false ->
                        commander.Value.AddActorUnderCommand selectedActors.Value.Head

    let attachActor (actor : ActorObject) =
        match attachedActor.IsSome with
            | true ->
                attachedActor.Value.ResetActorButtons()
            | false ->
                ()

        //makeActorParent actor
        // Detach current actor properly before attaching a new one
       // detachActor actor
        attachedActor <- Some actor

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
        let maxZoomHeight = 40.0f
        let minZoomHeight = 5.0f

        match ((zoomLevel + zoomAmount) < maxZoomHeight) && ((zoomLevel + zoomAmount) > minZoomHeight) with
            | true ->
                zoomLevel <- zoomLevel + zoomAmount
                // If result is outside range
            | false ->
                match (zoomLevel + zoomAmount) > minZoomHeight with
                    | false ->
                        zoomLevel <- minZoomHeight
                    | true ->
                        zoomLevel <- maxZoomHeight

    /// Buttons
    let setCameraSpecificButtons(inputEvent : InputEvent) =
        match inputEvent.IsAction PlayerInputActions.ZoomIn with
            | true ->
                match inputEvent.IsPressed() with
                    | true ->
                        zoom -1.0f
                    | false ->
                        ()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.ZoomOut with
                    | true ->
                        match inputEvent.IsPressed() with
                            | true ->
                                zoom 1.0f
                            | false ->
                                ()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.CameraAttach with
                        | true ->
                            match inputEvent.IsPressed() with
                            | true ->
                                attachActorNextPhysicsUpdate <- true
                            | false ->
                                ()
                            true
                        | false ->
                            match inputEvent.IsAction PlayerInputActions.CameraDetach with
                            | true ->
                                match inputEvent.IsPressed() with
                                | true ->
                                    detachActorNextPhysicsUpdate <- true
                                | false ->
                                    ()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.AddToUnderAttachedCommandDebug with
                                | true ->
                                    match inputEvent.IsPressed() with
                                    | true ->
                                        addActorInSelectionToUnderCommandNextPhysicsUpdate <- true
                                    | false ->
                                        ()
                                    true
                                | false ->
                                    false

    let setActorSelectionButtons (inputEvent : InputEvent) (actorButtons : Lazy<ActorButtons>) : bool =
        match inputEvent.IsAction PlayerInputActions.Select1 with
            | true ->
                actorButtons.Force().Select1Pressed <- inputEvent.IsPressed()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.Select2 with
                    | true ->
                        actorButtons.Force().Select2Pressed <- inputEvent.IsPressed()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.Select3 with
                            | true ->
                                actorButtons.Force().Select3Pressed <- inputEvent.IsPressed()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.Select4 with
                                    | true ->
                                        actorButtons.Force().Select4Pressed <- inputEvent.IsPressed()
                                        true
                                    | false ->
                                        match inputEvent.IsAction PlayerInputActions.Select5 with
                                            | true ->
                                                actorButtons.Force().Select5Pressed <- inputEvent.IsPressed()
                                                true
                                            | false ->
                                                match inputEvent.IsAction PlayerInputActions.Select6 with
                                                    | true ->
                                                        actorButtons.Force().Select6Pressed <- inputEvent.IsPressed()
                                                        true
                                                    | false ->
                                                        match inputEvent.IsAction PlayerInputActions.Select7 with
                                                            | true ->
                                                                actorButtons.Force().Select7Pressed <- inputEvent.IsPressed()
                                                                true
                                                            | false ->
                                                                match inputEvent.IsAction PlayerInputActions.Select8 with
                                                                    | true ->
                                                                        actorButtons.Force().Select8Pressed <- inputEvent.IsPressed()
                                                                        true
                                                                    | false ->
                                                                        match inputEvent.IsAction PlayerInputActions.Select9 with
                                                                            | true ->
                                                                                actorButtons.Force().Select9Pressed <- inputEvent.IsPressed()
                                                                                true
                                                                            | false ->
                                                                                match inputEvent.IsAction PlayerInputActions.Select0 with
                                                                                    | true ->
                                                                                        actorButtons.Force().Select0Pressed <- inputEvent.IsPressed()
                                                                                        true
                                                                                    | false ->
                                                                                        false

    let setActorButtons (inputEvent : InputEvent) (actorButtons : Lazy<ActorButtons>) : bool =
        match inputEvent.IsAction PlayerInputActions.Pickup with
            | true ->
                actorButtons.Force().PickupPressed <- inputEvent.IsPressed()
                true
            | false ->
                match inputEvent.IsAction PlayerInputActions.Reload with
                    | true ->
                        actorButtons.Force().ReloadPressed <- inputEvent.IsPressed()
                        true
                    | false ->
                        match inputEvent.IsAction PlayerInputActions.Drop with
                            | true ->
                                actorButtons.Force().DropPressed <- inputEvent.IsPressed()
                                true
                            | false ->
                                match inputEvent.IsAction PlayerInputActions.PrimaryAttack with
                                    | true ->
                                        actorButtons.Force().PrimaryAttackPressed <- inputEvent.IsPressed()
                                        true
                                    | false ->
                                        match inputEvent.IsAction PlayerInputActions.SecondaryAttack with
                                            | true ->
                                                actorButtons.Force().SecondaryAttackPressed <- inputEvent.IsPressed()
                                                true
                                            | false ->
                                                match inputEvent.IsAction PlayerInputActions.Aim with
                                                    | true ->
                                                        actorButtons.Force().AimPressed <- inputEvent.IsPressed()
                                                        true
                                                    | false ->
                                                        match inputEvent.IsAction PlayerInputActions.Run with
                                                            | true ->
                                                                actorButtons.Force().RunPressed <- inputEvent.IsPressed()
                                                                true
                                                            | false ->
                                                                false

    let setCameraPositionToActor() =
        match attachedActor.IsSome with
            | true ->
                let attachedActorTransform = attachedActor.Value.GetTransform()
                rootNode.Force().SetTransform(Transform (rootNode.Value.GetTransform().basis, Vector3(attachedActorTransform.origin.x, (attachedActorTransform.origin.y + zoomLevel), attachedActorTransform.origin.z)))
            | false ->
                ()

        // Smooth camera movements: (Doesn't work?)
        // let target = (Transform (rootNode.Value.GetTransform().basis, Vector3((Mathf.Lerp(rootNode.Force().GetGlobalTransform().origin.x, attachedActorTransform.origin.x, 1.0f)), (attachedActorTransform.origin.y + zoomLevel), (Mathf.Lerp(rootNode.Force().GetGlobalTransform().origin.z, attachedActorTransform.origin.z, 1.0f)))))

    override this._Ready() =
        makeSureCommanderIsSome()
        // Set starting zoom level
        zoom 5.0f
        ()

    override this._Process(delta : float32) =
        setCameraPositionToActor()

        match attachedActor.IsSome with
            | true ->
                let mouse3DPos = getMouse3DPosition()
                match mouse3DPos.IsSome with
                    | true ->
                        attachedActor.Value.AimTarget <- (Vector2(mouse3DPos.Value.x, mouse3DPos.Value.z))
                    | false ->
                        ()
            | false ->
                ()

        //rootNode.Force().LookAtFromPosition(Vector3(attachedActorTransform.origin.x, zoomLevel, attachedActorTransform.origin.z), attachedActorSpatial.Value.GetTransform().origin, (Vector3(0.0f,1.0f,0.0f)))

        //this.SetTransform (Transform (this.GetGlobalTransform().basis, attachedActorTransform.origin + Vector3(0.0f, zoomLevel, 0.0f)))

        // makeSureCommanderIsSome()
        // let attachedActorTransform = attachedActorSpatial.Value.GetTransform()
        //rootNode.Force().SetTransform(Transform (rootNode.Value.GetTransform().basis , Vector3(attachedActorTransform.origin.x, (attachedActorTransform.origin.y + zoomLevel), attachedActorTransform.origin.z)))
        ()

    override this._PhysicsProcess(delta : float32) =
        match initObjectReferences with
            | true ->
                initObjectReferences <- false
                // We need to get these in beginning, since they detach from camera when player uses them
                mouseSelectionArea.Force() |> ignore
                areaCollisionShape.Force() |> ignore
                attachCommander()
                //attachActor commander.Value
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

        makeSureCommanderIsSome()


        // let attachedActorTransform = attachedActorSpatial.Value.GetTransform()
        // let test = (Vector3(attachedActorTransform.origin.x, zoomLevel, attachedActorTransform.origin.z), attachedActorSpatial.Value.GetTransform().origin, (Vector3(0.0f,1.0f,0.0f)))
        // this.LookAtFromPosition(Vector3(attachedActorTransform.origin.x, zoomLevel, attachedActorTransform.origin.z), attachedActorSpatial.Value.GetTransform().origin, (Vector3(0.0f,1.0f,0.0f)))
        //rootNode.Force().SetTransform(Transform (rootNode.Value.GetTransform().basis , Vector3(attachedActorTransform.origin.x, zoomLevel, attachedActorTransform.origin.z)))


    override this._UnhandledInput (inputEvent : InputEvent) =

        match attachedActor.IsSome with
            | true ->
                let cameraSpecificResult = setCameraSpecificButtons inputEvent
                let actorButtonsResult = setActorButtons inputEvent (lazy attachedActor.Value.ActorButtons)
                let actorSelectionResult = setActorSelectionButtons inputEvent (lazy attachedActor.Value.ActorButtons)

                let movementResult = setMovementButtons inputEvent
                attachedActor.Value.ActorButtons.MoveDirection <- getMoveAxis()

                match movementResult || actorButtonsResult || actorSelectionResult with
                    | true ->
                        inputHandled()
                    | false ->
                        match cameraSpecificResult with
                            | true ->
                                cameraSpecificInputHandled()
                            | false ->
                                ()
            | false ->
                GD.Print "No actor attached???"

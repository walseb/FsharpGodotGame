namespace PlayerCameraManager

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

        mutable SprintPressed : bool
     }

type PlayerCamera() as this =
    inherit Camera()

    let mutable commanderActor : ActorObject Option =
        None

    let mutable attachedActor : ActorObject Option =
        None

    let mutable attachActorNextPhysicsUpdate =
        false

    /// Workaround because the "area" node is used for selection and it can't be read the same update/frame it's moved, move it one update/frame before
    let mutable selectionAreaMoved =
        false
    let mutable selectionAreaUpdatedConfirmed =
        false

    let mutable detachActorNextPhysicsUpdate =
        false

    let mouseSelectionArea : Lazy<Area> =
        lazy (this.GetNode(new NodePath("MouseSelectedArea")) :?> Area)

    let areaCollisionShape : Lazy<CollisionShape> =
        lazy (mouseSelectionArea.Force().GetChild 0 :?> CollisionShape)

    let mutable cameraButtons : CameraButtons =
        {
            MoveUpPressed = false
            MoveDownPressed = false
            MoveLeftPressed = false
            MoveRightPressed = false
            SprintPressed = false
        }

    let getMoveAxis() : Vector3 =
        Vector3(0.0f, 0.0f, 0.0f)
        |> fun axis -> if cameraButtons.MoveUpPressed then Vector3(axis.x, axis.y, axis.z - 1.0f) else axis
        |> fun axis -> if cameraButtons.MoveDownPressed then Vector3(axis.x, axis.y, axis.z + 1.0f)  else axis
        |> fun axis -> if cameraButtons.MoveLeftPressed then Vector3(axis.x - 1.0f, axis.y, axis.z) else axis
        |> fun axis -> if cameraButtons.MoveRightPressed then Vector3(axis.x + 1.0f, axis.y, axis.z) else axis
        |> fun moveDir -> if cameraButtons.SprintPressed then moveDir * 1.25f else moveDir

    let inputHandled() =
        this.GetTree().SetInputAsHandled();

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
                                        match inputEvent.IsAction PlayerInputActions.Sprint with
                                            | true ->
                                                cameraButtons.SprintPressed <- inputEvent.IsPressed()
                                                inputHandled()
                                                true
                                            |  false ->
                                                false

///////////////////////////////////////////////////////////////////////////////
//                                  Camera Actions                                                                      //
///////////////////////////////////////////////////////////////////////////////

    let zoom (zoomAmount : float32) =
        match attachedActor with
            | None ->
                ()
            | Some x ->
                let maxZoomHeight = 5.0f
                let minZoomHeight = 2.0f

                let cameraPos = this.GetGlobalTransform().origin
                let actorPos = attachedActor.Value.GetGlobalTransform().origin
                let actorCameraHeightDelta = cameraPos.y - actorPos.y
                GD.Print actorCameraHeightDelta

                // If the result wouldn't go outside the max and min height
                if ((actorCameraHeightDelta + zoomAmount < maxZoomHeight) || (actorCameraHeightDelta + zoomAmount > minZoomHeight))
                // Then increase global .z position with zoom amount
                then this.SetGlobalTransform (new Transform (this.GetGlobalTransform().basis, cameraPos + Vector3(0.0f,zoomAmount ,0.0f)))

                //GD.Print ("Zoomed by: ", zoomAmount)


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
                                false

    let setActorButtons (inputEvent : InputEvent) (actorButtons : Lazy<'ActorButtons>) : bool =
        match setMovementButtons inputEvent with
            | false ->
                match inputEvent.IsAction PlayerInputActions.Pickup with
                    | true ->
                        actorButtons.Force().PickupPressed <- inputEvent.IsPressed()
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
                                        false
            | true ->
                actorButtons.Force().MoveDirection <- getMoveAxis()
                inputHandled()
                true

    let changeMouseSelectionAreaState enable =
        // Enable the collision shape
        areaCollisionShape.Force().Disabled <- not enable
        // match enable with
            // | true ->
                // ()
            // | false ->
                // mouseSelectionArea.Force().SetGlobalTransform(new Transform (this.GetGlobalTransform().basis, Vector3(0.0f,1000.0f,0.0f)))

    // Might help get global position: mouseSelectionArea.Force().origin
    let moveMouseSelectAreaToMouse() =
        let raycastFromMouse() : Vector3 option =
            let maxRayRange = 1000.0f

            let mousePosition = this.GetViewport().GetMousePosition()

            // Get from and to positions for ray to use
            let rayFrom = this.ProjectRayOrigin (mousePosition)
            let rayTo = rayFrom + this.ProjectRayNormal (mousePosition) * maxRayRange

            // Shoot the physics based ray
            let resultRay =
                let spaceState = this.GetWorld().GetDirectSpaceState()
                spaceState.IntersectRay(rayFrom, rayTo, [| |], 2)

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
                    let child = mouseSelectionArea.Force().GetChild 0 :?> Spatial

                    mouseSelectionArea.Force().SetGlobalTransform (new Transform (mouseSelectionArea.Force().GetGlobalTransform().basis, mousePos.Value))



    // Gets actor script inside mouseSelectionArea area
    let getActorInSelection() : ActorObject option =
        let getClosestActorInArea (area : Area) =
            let mutable currentClosestActorDistanceToSelection : float32 = 0.0f
            let mutable currentClosestActor : ActorObject option = None

            area.GetOverlappingBodies()
            |> Array.iter (fun body ->
                            match body :? ActorObject with
                            | false ->
                                ()
                            | true ->
                                let actor = body :?> ActorObject

                                let isSameObject = LanguagePrimitives.PhysicalEquality
                                // Check if actor already is selected, the player wants to switch, not stay
                                match attachedActor.IsSome && (isSameObject actor attachedActor.Value) with
                                | true ->
                                       ()
                                | false ->
                                    let actorDistanceToSelection = actor.GetGlobalTransform().origin.DistanceSquaredTo(mouseSelectionArea.Force().GetGlobalTransform().origin)
                                    match currentClosestActor.IsSome with
                                    | true ->
                                            // DistanceSquaredTo should be faster than DistanceTo (atleast it was in unity)
                                            if actorDistanceToSelection < currentClosestActorDistanceToSelection
                                            then currentClosestActor <- Some actor; currentClosestActorDistanceToSelection <- actorDistanceToSelection
                                    | false ->
                                        currentClosestActor <- Some actor
                                        currentClosestActorDistanceToSelection <- actorDistanceToSelection)
            |> ignore

            changeMouseSelectionAreaState false
            currentClosestActor


        getClosestActorInArea(mouseSelectionArea.Force())

    let detachActor (actor : ActorObject) =
        match attachedActor.IsSome with
            | true ->
                let makeCommanderParent =
                    (this.GetParent()).RemoveChild this
                    commanderActor.Value.AddChild this
                    this.SetOwner commanderActor.Value

                makeCommanderParent
                attachedActor.Value.ResetActorButtons()
                attachedActor <- None
            | false ->
                ()

    let attachActor(actor : ActorObject) =
        let transferCameraButtonStateToActor (actor : ActorObject) =
            actor.ActorButtons.MoveDirection <- getMoveAxis()
            actor.ActorButtons.SprintPressed <- cameraButtons.SprintPressed

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
        GD.Print "++++ ACTOR ATTACHED ++++"
        transferCameraButtonStateToActor actor

    override this._Ready() =
        // TODO the camera needs to find a commander on start, if none was found, do something maybe (spam output), if it was found, claim that commander
        // Second thought
        // Every actor should have a list of how many they command, if they only can command 0 actors, their array is empty, the camera should not attach
        // Camera should be able to switch between commander via GUI and via selection in game(maybe???). On start camera should switch to commander with highest number of soldiers
        // BEFORE starting this, do a git commit with all progress, infact i'm doing it now.
        //commanderActor
        commanderActor <- Some (this.GetParent().GetNode(new NodePath "Actor") :?> ActorObject)
        ()


    override this._Process(delta : float32) =
        match selectionAreaMoved with
            | true ->
                selectionAreaUpdatedConfirmed <- true
            | false ->
                ()

    override this._PhysicsProcess(delta : float32) =
        match selectionAreaUpdatedConfirmed with
            | true ->
                selectionAreaMoved <- false
                selectionAreaUpdatedConfirmed <- false
                // Fetch the actor to attach
                let actor = getActorInSelection()
                match actor with
                    | Some x ->
                        attachActor actor.Value
                    | None ->
                        GD.Print("BACK AT PROCESS")
                        ()
            | false ->
                match attachActorNextPhysicsUpdate with
                    | true ->
                        GD.Print "MOVING"
                        changeMouseSelectionAreaState true
                        moveMouseSelectAreaToMouse()
                        selectionAreaMoved <- true
                        attachActorNextPhysicsUpdate <- false
                    | false ->
                        match detachActorNextPhysicsUpdate with
                            | true ->
                                detachActorNextPhysicsUpdate <- false
                                match attachedActor with
                                    | None ->
                                        ()
                                    | Some x ->
                                        detachActor attachedActor.Value
                                        GD.Print "Actor detached"
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
                // GD.Print "No actor attached"
                // GD.Print cameraButtons.MoveUpPressed
                // GD.Print cameraButtons.MoveDownPressed
                // GD.Print cameraButtons.MoveLeftPressed
                // GD.Print cameraButtons.MoveRightPressed
                setMovementButtons inputEvent
                |> ignore


        //GD.Print ("attachedActor", attachedActor.Value)

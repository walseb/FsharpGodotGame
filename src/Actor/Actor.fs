namespace Actor

open Actor.IActor
open System.Collections.Generic
open Godot
open Chessie.ErrorHandling
open RailwayUtils
open GodotUtils
open Items

type ActorState =
    | IdleState
    | MoveState
    | RunState
    | UnholsterState
    | HolsterState
    | HoldState
    | HoldMoveState
    | ReloadState
    | BoltState
    | AttackState

type ActorStats =
    {
        mutable Health : float
        mutable CommandChildren : int
    }

type ActorButtons =
    {
        mutable MoveDirection : Vector2
        mutable AimTarget : Vector2

        mutable PickupPressed : bool
        mutable DropPressed : bool
        mutable RunPressed : bool
        mutable PrimaryAttackPressed : bool
        mutable SecondaryAttackPressed : bool
        mutable AimPressed : bool
        mutable BoltPressed : bool
        mutable ReloadPressed : bool

        mutable Select1Pressed : bool
        mutable Select2Pressed : bool
        mutable Select3Pressed : bool
        mutable Select4Pressed : bool
        mutable Select5Pressed : bool
        mutable Select6Pressed : bool
        mutable Select7Pressed : bool
        mutable Select8Pressed : bool
        mutable Select9Pressed : bool
        mutable Select0Pressed : bool
     }

// * Actor
// Is nullable because if the object is destroyed in godot, it will show up as null
type ActorObject() as this =
    inherit IActor()

    // ** Vars
    let handReachArea =
        lazy(this.GetNode(new NodePath("HandReachArea")) :?> Area)

    let gunRayCast =
        lazy(this.GetNode(new NodePath("GunRayCast")) :?> RayCast)

    let mutable selectedItem : int = 1;

    let mutable inventorySize : int = 9;

    let mutable inventory : Item option array = Array.create 9 None

    let mutable isOnPlayerTeam = true

    let animatedSprite =
        lazy(this.GetNode(new NodePath("AnimatedSprite3D")) :?> AnimatedSprite3D)

    let mutable actorStats : ActorStats =
        {
            Health = 10.0
            CommandChildren = 0
        }

    let mutable commandParent : ActorObject option =
        None

    let mutable commandChildren =
        new List<ActorObject>()

    let mutable state = ActorState.IdleState

    let mutable actorButtons : ActorButtons =
        {
            MoveDirection = Vector2(0.0f,0.0f)
            AimTarget = Vector2(0.0f,0.0f)

            PickupPressed = false
            DropPressed = false
            RunPressed = false
            PrimaryAttackPressed = false
            SecondaryAttackPressed = false
            AimPressed = false
            BoltPressed = false
            ReloadPressed = false

            Select1Pressed = false
            Select2Pressed = false
            Select3Pressed = false
            Select4Pressed = false
            Select5Pressed = false
            Select6Pressed = false
            Select7Pressed = false
            Select8Pressed = false
            Select9Pressed = false
            Select0Pressed = false
        }

    let throwItemForce = 10.0f

    // Used to tweak the gobal movement speed in case of gravity change, etc
    let physicsMoveMultiplier = 100.0f

    let toggleItemAttachedNextPhysicsUpdate =
        new List<RigidBody>()

    // Returns true if item is dropeed
    let attemptToggleItemAttached (item : RigidBody) : bool =
        match toggleItemAttachedNextPhysicsUpdate.Contains item with
            | true ->
                false
            | false ->
                true

    // ** Take damage
    let die() =
        match actorStats.Health with
            | x when x >= -15.0 ->
                GD.Print "Normal death"
            | x when x >= -30.0 ->
                GD.Print "Violent death"
            | _ ->
                GD.Print "Really Violent death"
        // Delete node
        this.QueueFree()

    let takeDamage(damage) =
        actorStats.Health <- actorStats.Health - damage
        match actorStats.Health > 0.0 with
            | false ->
                die()
            | true ->
                ()

    // ** Inventory management
    /// returns true if item has changed
    let changeSelectedItem (number : int) =
        match number < inventorySize && number >= 0 && number <> selectedItem with
            | false ->
                false
            | true ->
                selectedItem <- number
                true

    let addItemToInventory item =
        inventory
        |> Array.findIndex (fun a -> a.IsNone)
        |> (fun a ->
            inventory.[a] <- Some item)

    let removeItemFromInventory item =
        findItemIndexInOptionArray(item, inventory)
        |> (fun a ->
            inventory.[a] <- None)

    let getBestMagazineAmongItems (weaponType : WeaponType) =
        let getMagazineWithMostBullets (list : seq<Magazine>) =
            list
            |> Seq.sortWith (fun a b ->
                            match (a.StoredAmmo > b.StoredAmmo) with
                                | true ->
                                    -1
                                | false ->
                                    match a.StoredAmmo = b.StoredAmmo with
                                        | true ->
                                            0
                                        | false ->
                                            1)
            |> Seq.head

        inventory
        |> Array.choose (fun a ->
                        match a.IsSome && a.Value :? Magazine && ((a.Value :?> Magazine).AmmoType = weaponType) with
                            | true ->
                                Some (a.Value :?> Magazine)
                            | false ->
                                None)
        |> (fun a ->
            match a.Length = 0 with
                | true ->
                    None
                | false ->
                    Some (getMagazineWithMostBullets a))

    // ** State actions
    let move (physicsState : PhysicsDirectBodyState) (multiplier : float32) =
        physicsState.ApplyImpulse(Vector3.Zero, (vector2To3 actorButtons.MoveDirection).Normalized() * physicsMoveMultiplier * multiplier)

    let drop() =
        this.GetTree().SetInputAsHandled();
        inventory.[selectedItem]
        |> (fun a ->
            match a.IsSome with
                | false ->
                    fail "No item selected"
                | true ->
                    ok a.Value)
        |> bind (fun a ->
            match toggleItemAttachedNextPhysicsUpdate.Contains a with
                | false ->
                    ok a
                | true ->
                    fail "Item is already in process of being dropped")
        |> map (toggleItemAttachedNextPhysicsUpdate.Add)
        |> fun _ -> inventory.[selectedItem] <- None

    let pickupDelay = 1.0f
    let mutable pickupTimer = 0.0f

    /// Returns true if new item has been picked up
    let pickup() =
        this.GetTree().SetInputAsHandled();
        match  pickupTimer > pickupDelay with
            | false ->
                false
            | true ->
                pickupTimer <- 0.0f
                sortObjectListClosestFirst (handReachArea.Force().GetOverlappingBodies()) this
                |> List.choose (fun a ->
                    match a :? Item with
                        | true ->
                            Some (a :?> Item)
                        | false ->
                            None)
                |> (fun a ->
                    match a.IsEmpty with
                    | true ->
                        false
                    | false ->
                            drop()
                            inventory.[selectedItem] <- Some (a.Head)
                            toggleItemAttachedNextPhysicsUpdate.Add a.Head
                            GD.Print "PICKUP"
                            true)

    //let mutable lastPos = this.GetGlobalTransform().origin
    // Remove this later
    //let mutable value = 0.0f

    // let OLDRotateTowards (delta : float32, lookDir : Vector3) =
         // let thisTransform = this.GetGlobalTransform()
//
         // // Get target transform
         // let targetTransform = thisTransform.LookingAt(lookDir,Vector3.Up)
         // let targetQuat = targetTransform.basis.Quat()
//
         // // let targetEuler = targetTransform.basis.GetEuler()
         // // Problem is here!! You can't set rotation at every frame, you need to use setGlobalTransform instead
         // //this.SetGlobalTransform(Transform(targetQuat, thisTransform.origin))
//
         // let targetEuler = targetTransform.basis.GetEuler()
         // let thisEuler = thisTransform.basis.GetEuler()
//
         // this.SetRotation (Vector3(0.0f, Mathf.Lerp(thisEuler.y, targetEuler.y, 0.2f),0.0f))
//
         // // this.SetRotation (Vector3(0.0f, targetEuler.y,0.0f))
//
         // // Force rotation on 2D plane
         // // let targetEuler = targetTransform.basis.GetEuler()
         // // let thisEuler = thisTransform.basis.GetEuler()
         // // GD.Print ("DIFF IS: ", Mathf.Abs(thisEuler.y - targetEuler.y))
         // // match (Mathf.Abs(thisEuler.y - targetEuler.y) <= smallestDeltaToRotateAt) with
             // // | true ->
                 // // GD.Print"TRUE"
                 // // this.SetRotation (Vector3(0.0f, targetEuler.y,0.0f))
             // // | false ->
                 // // GD.Print"FALSE"
                 // // this.SetRotation (Vector3(0.0f, Mathf.Lerp(thisEuler.y, targetEuler.y, 1.0f),0.0f))

    let rotateTowards (delta : float32, rotateSpeed : float32, lookDir : Vector3) =
        let thisTransform = this.GetGlobalTransform()

        // Get target transform
        thisTransform.LookingAt(lookDir,Vector3.Up)
        |> (fun targetTransform -> thisTransform.basis.Quat().Slerp(targetTransform.basis.Quat(), (delta * rotateSpeed)))
        |> tee (fun targetRotation -> this.SetGlobalTransform(Transform(targetRotation, thisTransform.origin)))
        |> Basis
        // Hack to lock the y axis
        |> (fun targetBasis -> this.SetRotation (Vector3(0.0f, targetBasis.GetEuler().y,0.0f)))

    // Works but not really useful
    // let setRotation (lookDir : Vector3) =
        // let thisTransform = this.GetGlobalTransform()
        // let targetTransform = thisTransform.LookingAt(lookDir,Vector3.Up)
        // let targetEuler = targetTransform.basis.GetEuler()
        // this.SetRotation (Vector3(0.0f, targetEuler.y,0.0f))

    let rotateTowardsMoveDirection(delta : float32) =
        this.GetGlobalTransform().origin
        |> (fun thisPos -> vector2To3(actorButtons.MoveDirection + vector3To2 thisPos))
        |> (fun lookDir -> rotateTowards(delta, 15.0f, lookDir))

    let rotateTowardsMousePosition(delta : float32) =
        rotateTowards(delta, 20.0f, vector2To3 actorButtons.AimTarget)

    let isMoveDirectionZero () =
        actorButtons.MoveDirection.x = 0.0f && actorButtons.MoveDirection.y = 0.0f

    // Mutable state machine data
    let mutable timer = 0.0f

    // ** Attack state helpers
    let mutable selectedWeaponSlotOnCombatEnter = selectedItem
    let mutable selectedWeaponOnCombatEnter : Weapon option = None
    let mutable usePrimaryAttackMode : bool = false

    // ** Animation helpers

    let setAnimation (name : string) speed =
        name
        |> tee (fun a -> animatedSprite.Force().Play a)
        |> (fun name -> animatedSprite.Value.GetSpriteFrames().SetAnimationSpeed(name, speed))

    let getHeldWeaponAnimationName animationStateName =
        ItemHelperFunctions.getWeaponTypeString((inventory.[selectedWeaponSlotOnCombatEnter].Value :?> Weapon).WeaponType)
        |> (fun weaponType ->
            match weaponType.IsSome with
                | true ->
                    Some (weaponType.Value + animationStateName)
                | false ->
                    None)

    let setWeaponAnimation animationStateName speed =
        getHeldWeaponAnimationName animationStateName
        |> (fun anim ->
            match anim.IsSome with
            | false ->
                fail ("Weapon animation in " + animationStateName + " missing!!")
            | true ->
                setAnimation anim.Value speed
                ok ())
        |> logErr
        |> ignore

    let setWeaponAnimationTimed animationStateName time =
        getHeldWeaponAnimationName animationStateName
        |> (fun anim ->
            match anim.IsSome with
                | false ->
                    fail ("Weapon animation in " + animationStateName + " missing!!")
                | true ->
                    ok anim)
        |> (bind (fun anim ->
            animatedSprite.Force().Frames.GetFrameCount anim.Value
            |> (fun a -> (float32 a) / time)
            |> setAnimation anim.Value
            |> ok))
        |> logErr
        |> ignore

    // ** Basic state conditions

    let hasWeaponSelected() =
        inventory.[selectedItem].IsSome && inventory.[selectedItem].Value :? Weapon

    /// Returns false if combat state failed to init
    let canEnterCombatState() =
        match inventory.[selectedItem].IsSome && inventory.[selectedItem].Value :? Weapon with
            | true ->
                selectedWeaponSlotOnCombatEnter <- selectedItem
                selectedWeaponOnCombatEnter <- Some (inventory.[selectedItem].Value :?> Weapon)
                true
             | false ->
                false

    // ** Common state keys

/// Returns true if different item is selected
    let selectItem () =
        match actorButtons.Select1Pressed with
            | true ->
                changeSelectedItem 1 |> ignore
                Some 1
            | false ->
                match actorButtons.Select2Pressed with
                    | true ->
                        changeSelectedItem 2 |> ignore
                        Some 2
                    | false ->
                        match actorButtons.Select3Pressed with
                            | true ->
                                changeSelectedItem 3 |> ignore
                                Some 3
                            | false ->
                                match actorButtons.Select4Pressed with
                                    | true ->
                                        changeSelectedItem 4 |> ignore
                                        Some 4
                                    | false ->
                                        match actorButtons.Select5Pressed with
                                            | true ->
                                                changeSelectedItem 5 |> ignore
                                                Some 5
                                            | false ->
                                                match actorButtons.Select6Pressed with
                                                    | true ->
                                                        changeSelectedItem 6 |> ignore
                                                        Some 6
                                                    | false ->
                                                        match actorButtons.Select7Pressed with
                                                            | true ->
                                                                changeSelectedItem 7 |> ignore
                                                                Some 7
                                                            | false ->
                                                                match actorButtons.Select8Pressed with
                                                                    | true ->
                                                                        changeSelectedItem 8 |> ignore
                                                                        Some 8
                                                                    | false ->
                                                                        match actorButtons.Select9Pressed with
                                                                            | true ->
                                                                                changeSelectedItem 9 |> ignore
                                                                                Some 9
                                                                            | false ->
                                                                                match actorButtons.Select0Pressed with
                                                                                    | true ->
                                                                                        changeSelectedItem 0 |> ignore
                                                                                        Some 0
                                                                                    | false ->
                                                                                        None

    // ** Actor States
// delete

    // *** Idle state

    let startIdle() =
        setAnimation "Idle" 100.0f
        // Change actor speed on idle state
        this.LinearVelocity <- this.LinearVelocity * 0.5f
        None

    let updateKeysIdle() =
        //GD.Print (isMoveDirectionZero())
        //GD.Print (actorButtons.MoveDirection.x, actorButtons.MoveDirection.y)
        match isMoveDirectionZero() with
            | false ->
                Some MoveState
            | true ->
                    match actorButtons.AimPressed && hasWeaponSelected() with
                        | true ->
                            Some UnholsterState
                        | false ->
                            match actorButtons.PickupPressed with
                                | true ->
                                    pickup() |> ignore
                                    None
                                | false ->
                                    match actorButtons.DropPressed with
                                        | true ->
                                            drop()
                                            None
                                        | false ->
                                            selectItem() |> ignore
                                            None

    // *** Move state

    let startMove() =
        setAnimation "Move" 10.0f
        None

    let updateKeysMove() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.RunPressed with
                    | true ->
                            Some RunState
                    | false ->
                        match actorButtons.AimPressed && hasWeaponSelected() with
                            | true ->
                                Some UnholsterState
                            | false ->
                                match actorButtons.PickupPressed with
                                    | true ->
                                        pickup() |> ignore
                                        None
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                None
                                            | false ->
                                                None

    let integrateForcesMove (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (3.0f * delta)

    let physicsProcessMove (delta : float32) =
        rotateTowardsMoveDirection(delta)
        ()

    let processMove  (delta : float32)  =
        //rotateTowardsMoveDirection delta
        None

    // *** Run state

    let startRun() =
        setAnimation "Run" 50.0f
        None

    let updateKeysRun() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.RunPressed with
                    | false ->
                        Some MoveState
                    | true ->
                        match actorButtons.AimPressed && hasWeaponSelected() with
                            | true ->
                                Some UnholsterState
                            | false ->
                                match actorButtons.PickupPressed && pickup() with
                                    | true ->
                                        None
                                    | false ->
                                        match actorButtons.DropPressed with
                                            | true ->
                                                drop()
                                                None
                                            | false ->
                                                None

    let integrateForcesRun (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (5.0f * delta)

    let physicsProcessRun (delta : float32) =
        rotateTowardsMoveDirection(delta)
        ()

    let processRun  (delta : float32)  =
        //rotateTowardsMoveDirection delta
        None

    // *** Hold state
    let startHold() =
        setWeaponAnimation "Hold" 5.0f
        None

    let updateKeysHold() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | false ->
                Some HoldMoveState
            | true ->
                match actorButtons.AimPressed && selectedItem = selectedWeaponSlotOnCombatEnter with
                    | false ->
                        Some HolsterState
                    | true ->
                        match actorButtons.PrimaryAttackPressed with
                            | true ->
                                usePrimaryAttackMode <- true
                                Some AttackState
                            | false ->
                                // Check for trigger release
                                match not(actorButtons.PrimaryAttackPressed) && inventory.[selectedItem].Value :? Gun with
                                    | true ->
                                        (inventory.[selectedItem].Value :?>Gun).ReleaseTrigger()
                                    | false ->
                                        ()
                                match actorButtons.SecondaryAttackPressed with
                                    | true ->
                                        usePrimaryAttackMode <- false
                                        Some AttackState
                                    | false ->
                                        match actorButtons.ReloadPressed with
                                            | true ->
                                                Some ReloadState
                                            | false ->
                                                match actorButtons.BoltPressed && inventory.[selectedItem].Value :? Gun with
                                                    | true ->
                                                        Some BoltState
                                                    | false ->
                                                        match actorButtons.PickupPressed && pickup() with
                                                            | true ->
                                                                Some IdleState
                                                            | false ->
                                                                match actorButtons.DropPressed with
                                                                    | true ->
                                                                        drop()
                                                                        Some IdleState
                                                                    | false ->
                                                                        None

    let processHold  (delta : float32)  =
        rotateTowardsMousePosition delta
        None

    // *** Hold move state

    let startHoldMove() =
        setWeaponAnimation "HoldMove" 5.0f
        None

    let updateKeysHoldMove() =
        selectItem() |> ignore
        match isMoveDirectionZero() with
            | true ->
                Some HoldState
            | false ->
                match actorButtons.AimPressed && selectedItem = selectedWeaponSlotOnCombatEnter with
                    | false ->
                        Some HolsterState
                    | true ->
                        match actorButtons.PrimaryAttackPressed with
                            | true ->
                                usePrimaryAttackMode <- true
                                Some AttackState
                            | false ->
                                match actorButtons.SecondaryAttackPressed with
                                    | true ->
                                        usePrimaryAttackMode <- false
                                        Some AttackState
                                    | false ->
                                        match actorButtons.ReloadPressed with
                                            | true ->
                                                Some ReloadState
                                            | false ->
                                                match actorButtons.BoltPressed && inventory.[selectedItem].Value :? Gun with
                                                    | true ->
                                                        Some BoltState
                                                    | false ->
                                                        match actorButtons.PickupPressed && pickup() with
                                                            | true ->
                                                                Some IdleState
                                                            | false ->
                                                                match actorButtons.DropPressed with
                                                                    | true ->
                                                                        drop()
                                                                        Some IdleState
                                                                    | false ->
                                                                        None

    let integrateForcesHoldMove (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (1.5f * delta)

    let processHoldMove  (delta : float32)  =
        rotateTowardsMousePosition delta
        None

    // *** Reload state

    let reloadTime : float32 = 0.4f

    let startReload() =
        setWeaponAnimationTimed "Reload" reloadTime
        None

    let reload() =
        // Print inventory
        // inventory
        // |> Array.map (fun a ->
                      // match a.IsSome with
                          // | true ->
                                // GD.Print (a.Value :?> Item).Name
                          // | false ->
                                // ())

        let oldMagazine = (inventory.[selectedItem].Value :?> Gun).Magazine
        let newMagazine = getBestMagazineAmongItems selectedWeaponOnCombatEnter.Value.WeaponType

        match oldMagazine.IsSome && newMagazine.IsSome with
            | true ->
                match newMagazine.Value.StoredAmmo > oldMagazine.Value.StoredAmmo with
                    | false ->
                        GD.Print "Old magazine has more ammo than any other magazine"
                    | true ->
                        (inventory.[selectedItem].Value :?> Gun).Magazine <- newMagazine

                        // Remove old mag from inventory
                        removeItemFromInventory newMagazine.Value

                        // Add old mag to inventory
                        addItemToInventory oldMagazine.Value

                        GD.Print "Swapped old mag with new mag"
            | false ->
                match newMagazine.IsSome with
                    | true ->
                        (inventory.[selectedItem].Value :?> Gun).Magazine <- newMagazine
                        GD.Print "Inserted magazine into gun!"
                    | false ->
                        GD.Print "No mags to use in reload found!"
                        ()

    let processReload  (delta : float32)  =
        timer <- timer + delta
        match timer > reloadTime with
            | true ->
                reload()
                Some HoldState
            | false ->
                None

    let updateKeysReload() =
        match actorButtons.AimPressed && selectedItem = selectedWeaponSlotOnCombatEnter with
            | false ->
                Some HolsterState
            | true ->
                match actorButtons.PickupPressed && pickup() with
                    | true ->
                        Some IdleState
                    | false ->
                        match actorButtons.DropPressed with
                            | true ->
                                drop()
                                Some IdleState
                            | false ->
                                None

    let integrateForcesReload (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    // *** Attack state

    // **** Attack state actions
    let whatWouldFiringHit() =
        gunRayCast.Force().ForceRaycastUpdate()
        match gunRayCast.Value.IsColliding() with
            | false ->
                None
            | true ->
                Some (gunRayCast.Value.GetCollider().GetInstanceId())

    let mutable lastShootAttemptWorked = false

    let gunFire(rayCast : RayCast, recoilPushbackMultiplier : float32) =
        let recoilPushback() =
            let rotation = this.GetGlobalTransform().basis.GetAxis(0)
            let vector = rotateVector90Degrees (vector3To2 rotation)
            this.ApplyImpulse(Vector3.Zero, vector2To3(vector).Normalized() * recoilPushbackMultiplier)

        match inventory.[selectedItem].IsSome && inventory.[selectedItem].Value :? Gun with
            | true ->
                match (inventory.[selectedItem].Value :?> Gun).PullTrigger (gunRayCast.Force()) with
                    | true ->
                        recoilPushback()
                        lastShootAttemptWorked <- true
                    | false ->
                        lastShootAttemptWorked <- false
            | false ->
                ()

    let attack() =
        let attackWithMode (attackMode : WeaponAttackModes) =
            match attackMode with
                | Bayonet ->
                    GD.Print "BAYONET ATTACK!!!"
                | Knife ->
                    GD.Print "KNIFE ATTACK!!!"
                | Bash ->
                    GD.Print "BASH ATTACK!!!"
                | Buckshot ->
                    GD.Print "BUCKSHOT ATTACK!!!"
                | Slug ->
                    GD.Print "SLUG ATTEMPT FIRE!!!"
                    gunFire(gunRayCast.Force(), 10.0f)

        match usePrimaryAttackMode with
            | true ->
                attackWithMode selectedWeaponOnCombatEnter.Value.PrimaryAttackMode.Value
            | false ->
                attackWithMode selectedWeaponOnCombatEnter.Value.SecondaryAttackMode.Value


    // *** Attack state
    // Handles the cooldown after attacking
    let mutable attackCooldown : float32 = 2.0f

    let startAttack() =
        attackCooldown <- selectedWeaponOnCombatEnter.Value.AttackCooldown
        setWeaponAnimationTimed "Attack" attackCooldown
        attack() |> ignore
        None

    let processAttack  (delta : float32)  =
        timer <- timer + delta
        match timer > attackCooldown with
            | true ->
                Some HoldState
            | false ->
                None

    let updateKeysAttack() =
        match actorButtons.PickupPressed && pickup() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        // Check for trigger release
                        match not(actorButtons.PrimaryAttackPressed) && inventory.[selectedItem].IsSome && inventory.[selectedItem].Value :? Gun with
                            | true ->
                                (inventory.[selectedItem].Value :?>Gun).ReleaseTrigger()
                                None
                            | false ->
                                None

    let integrateForcesAttack (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.3f * delta)

    // ** Bolt state
    // Handles the cooldown after attacking
    let mutable boltCooldown : float32 = 1.0f

    let startBolt() =
        boltCooldown <- (selectedWeaponOnCombatEnter.Value :?> Gun).BoltCooldown
        setWeaponAnimationTimed "Bolt" boltCooldown
        None

    let processBolt  (delta : float32)  =
        timer <- timer + delta
        match timer > boltCooldown with
            | true ->
                (selectedWeaponOnCombatEnter.Value :?> Gun).Bolt()
                Some HoldState
            | false ->
                None

    let updateKeysBolt() =
        match actorButtons.PickupPressed && pickup() with
            | true ->
                Some IdleState
            | false ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        // Check for trigger release
                        match not(actorButtons.PrimaryAttackPressed) && inventory.[selectedItem].IsSome && inventory.[selectedItem].Value :? Gun with
                            | true ->
                                (inventory.[selectedItem].Value :?>Gun).ReleaseTrigger()
                                None
                            | false ->
                                None

    let integrateForcesBolt (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.3f * delta)

    // *** Holster state

    let holsterTime : float32 = 1.0f

    let startHolster() =
        setWeaponAnimationTimed "Holster" holsterTime
        None

    let processHolster  (delta : float32)  =
        timer <- timer + delta
        match timer > holsterTime with
            | true ->
                timer <- 0.0f
                Some IdleState
            | false ->
                None

    let updateKeysHolster () =
        selectItem() |> ignore
        match actorButtons.AimPressed && selectedItem = selectedWeaponSlotOnCombatEnter with
            | true ->
                Some HoldState
            | false ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        match actorButtons.PickupPressed && pickup() with
                            | true ->
                                Some IdleState
                            | false ->
                                None

    let integrateForcesHolster (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    // *** Unholster state

    let unHolsterTime : float32 = 1.0f

    let startUnholster() =
        match canEnterCombatState() with
            | false ->
                Some IdleState
            | true ->
                setWeaponAnimationTimed "Unholster" unHolsterTime
                None

    let processUnholster(delta : float32) =
        timer <- timer + delta
        match timer > unHolsterTime with
            | true ->
                timer <- 0.0f
                Some HoldState
            | false ->
                None

    let updateKeysUnholster () =
        selectItem() |> ignore
        match actorButtons.AimPressed && selectedItem = selectedWeaponSlotOnCombatEnter with
            | false ->
                Some IdleState
            | true ->
                match actorButtons.DropPressed with
                    | true ->
                        drop()
                        Some IdleState
                    | false ->
                        match actorButtons.PickupPressed && pickup() with
                            | true ->
                                Some IdleState
                                | false ->
                                    None

    let integrateForcesUnholster (delta : float32) (physicsState : PhysicsDirectBodyState) =
        move physicsState (0.5f * delta)

    // ** State switchers
    let switchStateStateMachine (actorState : ActorState) :  ActorState option =
        timer <- 0.0f
        match actorState with
            | IdleState -> startIdle()
            | MoveState -> startMove()
            | RunState -> startRun()
            | UnholsterState -> startUnholster()
            | HoldState -> startHold()
            | HoldMoveState -> startHoldMove()
            | HolsterState -> startHolster()
            | ReloadState -> startReload()
            | BoltState -> startBolt()
            | AttackState -> startAttack()

    let processStateMachine (delta : float32) (actorState : ActorState) =
        match actorState with
            | IdleState -> None
            | MoveState -> processMove delta
            | RunState -> processRun delta
            | UnholsterState -> processUnholster delta
            | HoldState -> processHold delta
            | HoldMoveState -> processHoldMove delta
            | HolsterState -> processHolster delta
            | ReloadState -> processReload delta
            | BoltState -> processBolt delta
            | AttackState -> processAttack delta

    let physicsProcessForcesStateMachine  (delta : float32) (actorState : ActorState)  =
        match actorState with
            | IdleState -> ()
            | MoveState -> physicsProcessMove delta
            | RunState -> physicsProcessRun delta
            | UnholsterState -> ()
            | HoldState -> ()
            | HoldMoveState -> ()
            | HolsterState -> ()
            | ReloadState -> ()
            | BoltState -> ()
            | AttackState -> ()

    let integrateForcesStateMachine  (delta : float32) (physicsState : PhysicsDirectBodyState) (actorState : ActorState)  =
        match actorState with
            | IdleState -> ()
            | MoveState -> integrateForcesMove delta physicsState
            | RunState -> integrateForcesRun delta physicsState
            | UnholsterState -> integrateForcesUnholster delta physicsState
            | HoldState -> ()
            | HoldMoveState -> integrateForcesHoldMove delta physicsState
            | HolsterState -> integrateForcesHolster delta physicsState
            | ReloadState -> integrateForcesReload delta physicsState
            | BoltState -> integrateForcesBolt delta physicsState
            | AttackState -> integrateForcesAttack delta physicsState

    let updateKeysStateMachine (actorState : ActorState) =
        match actorState with
            | IdleState -> updateKeysIdle()
            | MoveState -> updateKeysMove()
            | RunState -> updateKeysRun()
            | UnholsterState -> updateKeysUnholster()
            | HoldState -> updateKeysHold()
            | HoldMoveState -> updateKeysHoldMove()
            | HolsterState -> updateKeysHolster()
            | ReloadState -> updateKeysReload()
            | BoltState -> updateKeysBolt()
            | AttackState -> updateKeysAttack()

    let rec switchState (changeToState : ActorState option) =
        match changeToState with
            | Some x ->
                match switchStateStateMachine(x) with
                    | Some y ->
                        switchState (Some y)
                    | None ->
                        // Update keys right after switching state, since there might be no key events for a while after this
                        match updateKeysStateMachine(x) with
                            | Some z ->
                                switchState (Some z)
                            | None  ->
                                GD.Print("Switching actor state");
                                state <- x
            | None ->
                ()

// ** Properties
    member this.ActorButtons
        with get () = actorButtons
        and set (value) = actorButtons <- value

    member this.Inventory
        with get () = inventory
        and set (value) = inventory <- value

    member this.SelectedItem
        with get () = selectedItem
        and set (value) = selectedItem <- value

    member this.CommandChildren
        with get () = commandChildren

    member this.CommandParent
        with get () = commandParent
        and set (value) = commandParent <- value

    member this.State
        with get () = state

    member this.IsOnPlayerTeam
        with get () = isOnPlayerTeam
        and set (value) = isOnPlayerTeam <- value

    member this.LastShootAttemptWorked
        with get () = lastShootAttemptWorked
        and set (value) = lastShootAttemptWorked <- value

    // ** Functions
    member this.WhatWouldFiringHit =
        whatWouldFiringHit

    member this.IsInCombatState
        with get () =
            match state with
                | ActorState.AttackState | ActorState.HoldMoveState | ActorState.HoldState | ActorState.HolsterState | ActorState.ReloadState | ActorState.UnholsterState ->
                    true
                | _ ->
                    false

    member this.AddActorUnderCommand(actorObject : ActorObject) =
        match commandChildren.Contains actorObject with
            | true ->
                ()
            | false ->
                commandChildren.Add actorObject

    member this.InputUpdated() =
        switchState (updateKeysStateMachine state)

    member this.ResetActorButtons() =
        actorButtons.MoveDirection <- Vector2(0.0f,0.0f)
        actorButtons.AimTarget <- Vector2(0.0f,0.0f)
        actorButtons.PickupPressed <- false
        actorButtons.DropPressed <- false
        actorButtons.RunPressed <- false
        actorButtons.PrimaryAttackPressed <- false
        actorButtons.SecondaryAttackPressed <- false
        actorButtons.AimPressed <- false
        actorButtons.PrimaryAttackPressed <- false
        actorButtons.ReloadPressed <- false

    // *** Damage
    override this.DamageMelee(damage : float) =
        takeDamage damage
        GD.Print ("*********I'm hit by melee for ", damage, " damage************")

    override this.DamageProjectile(damage) =
        takeDamage damage
        GD.Print ("*********I'm hit by projectile for ", damage, " damage***********")

    // *** Update
    override this._Ready() =
        ()

    override this._Process(delta : float32) =
        switchState (processStateMachine delta state)
        pickupTimer <- pickupTimer + delta

    override this._PhysicsProcess(delta : float32) =
        let toggleQueuedAttachedState() =
            let toggleItem (item : RigidBody)=
                let itemParent = item.GetParent()
                match itemParent with
                    | null ->
                        // Drop
                        let thisTransform = this.GetGlobalTransform()
                        item.SetGlobalTransform (Transform (thisTransform.basis, thisTransform.origin))
                        this.Owner.AddChild item
                        // Impulse to make sure it's not sleeping, otherwise the collision somehow gets disabled and the item is bugged. Other solution is disabling "can sleep"
                        item.ApplyImpulse(Vector3.Zero, (vector2To3 actorButtons.MoveDirection).Normalized() * throwItemForce)
                    | _ ->
                        // Pickup
                        item.SetLinearVelocity Vector3.Zero
                        itemParent.RemoveChild(item)

            match toggleItemAttachedNextPhysicsUpdate.Count with
                | 0 ->
                    ()
                | _ ->
                    toggleItemAttachedNextPhysicsUpdate
                    |> Seq.iter toggleItem
                    toggleItemAttachedNextPhysicsUpdate.Clear()

        toggleQueuedAttachedState()

        physicsProcessForcesStateMachine delta state
        |> ignore

    override this._IntegrateForces(physicsState : PhysicsDirectBodyState) =
        let delta = physicsState.Step
        integrateForcesStateMachine delta physicsState state
        |> ignore

namespace Actor

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
    | AttackState

type ActorStats =
    {
        mutable Health : float
        mutable CommandChildren : int
    }

type ActorButtons =
    {
        mutable MoveDirection : Vector2

        mutable PickupPressed : bool
        mutable DropPressed : bool
        mutable RunPressed : bool
        mutable PrimaryAttackPressed : bool
        mutable SecondaryAttackPressed : bool
        mutable AimPressed : bool
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
[<AbstractClass>]
type BaseActor() =
    inherit RigidBody()
    abstract member DamageMelee : float -> unit
    abstract member DamageProjectile : float -> unit

type ActorObject() as this =
    inherit BaseActor()

    // ** Vars
    let handReachArea =
        lazy(this.GetNode(new NodePath("HandReachArea")) :?> Area)

    let gunRayCast =
        lazy(this.GetNode(new NodePath("GunRayCast")) :?> RayCast)

    let mutable aimTarget : Vector2 =
        Vector2(0.0f,0.0f)

    let mutable selectedItem : int = 0;

    let mutable inventorySize : int = 9;

    let mutable items : Item option array = Array.create 9 None

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

            PickupPressed = false
            DropPressed = false
            RunPressed = false
            PrimaryAttackPressed = false
            SecondaryAttackPressed = false
            AimPressed = false
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
        items
        |> Array.findIndex (fun a -> a.IsNone)
        |> (fun a ->
            items.[a] <- Some item)

    let removeItemFromInventory item =
        findItemIndexInOptionArray(item, items)
        |> (fun a ->
            items.[a] <- None)

    let getBestMagazineAmongItems (weaponType : string) =
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

        items
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
        match items.[selectedItem].IsSome with
            | false ->
                ()
            | true ->
                // Make sure an item isn't added twice
                match toggleItemAttachedNextPhysicsUpdate.Contains items.[selectedItem].Value with
                    | false ->
                        toggleItemAttachedNextPhysicsUpdate.Add items.[selectedItem].Value
                        GD.Print "DROP"
                    | true ->
                        ()
        items.[selectedItem] <- None

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
                            items.[selectedItem] <- Some (a.Head)
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
        let targetTransform = thisTransform.LookingAt(lookDir,Vector3.Up)

        // Slerp it
        let targetRotation = thisTransform.basis.Quat().Slerp(targetTransform.basis.Quat(), (delta * rotateSpeed))

        this.SetGlobalTransform(Transform(targetRotation, thisTransform.origin))
        // Hack to lock the y axis
        let targetBasis = Basis(targetRotation)
        this.SetRotation (Vector3(0.0f, targetBasis.GetEuler().y,0.0f))

    // Works but not really useful
    // let setRotation (lookDir : Vector3) =
        // let thisTransform = this.GetGlobalTransform()
        // let targetTransform = thisTransform.LookingAt(lookDir,Vector3.Up)
        // let targetEuler = targetTransform.basis.GetEuler()
        // this.SetRotation (Vector3(0.0f, targetEuler.y,0.0f))

    let rotateTowardsMoveDirection(delta : float32) =
        let thisTransform = this.GetGlobalTransform()

        // Get move direction based on keys

        let lookDir = vector2To3(actorButtons.MoveDirection + (vector3To2 thisTransform.origin))

        // Get move direction based on velocity
        // let lookDir = Vector3(this.LinearVelocity.x + thisTransform.origin.x, 0.0f, this.LinearVelocity.z + thisTransform.origin.z)
        rotateTowards(delta, 15.0f, lookDir)

    let rotateTowardsMousePosition(delta : float32) =
        rotateTowards(delta, 20.0f, vector2To3 aimTarget)
        //setRotation(aimTarget)

    let isMoveDirectionZero () =
        actorButtons.MoveDirection.x = 0.0f && actorButtons.MoveDirection.y = 0.0f

    // Mutable state machine data
    let mutable timer = 0.0f

    // ** Attack state helpers
    let mutable selectedWeaponSlotOnCombatEnter = selectedItem
    let mutable selectedWeaponOnCombatEnter : Weapon option = None
    let mutable usePrimaryAttackMode : bool = false

    // ** Animation helpers

    let setAnimation name speed =
        animatedSprite.Force().Play name
        animatedSprite.Value.GetSpriteFrames().SetAnimationSpeed(name, speed)

    let getHeldWeaponAnimationName animationStateName =
        let weaponType = ItemHelperFunctions.getWeaponType(items.[selectedWeaponSlotOnCombatEnter].Value :?> Weapon)
        match weaponType.IsSome with
            | true ->
                Some (weaponType.Value + animationStateName)
            | false ->
                None

    let setWeaponAnimation animationStateName speed =
        let animationName = getHeldWeaponAnimationName animationStateName
        match animationName.IsSome with
            | false ->
                GD.Print ("Weapon animation in ", animationStateName, " missing!!")
            | true ->
                setAnimation animationName.Value speed

    let setWeaponAnimationTimed animationStateName time =
        let animationStateName = getHeldWeaponAnimationName animationStateName
        match animationStateName.IsSome with
            | false ->
                GD.Print ("Weapon animation in ", animationStateName, " missing!!")
            | true ->
                animatedSprite.Force().Frames.GetFrameCount animationStateName.Value
                |> (fun a -> (float32 a) / time)
                |> setAnimation animationStateName.Value

    // ** Basic state conditions

    let hasWeaponSelected() =
        items.[selectedItem].IsSome && items.[selectedItem].Value :? Weapon

    /// Returns false if combat state failed to init
    let initializeCombatState() =
        match items.[selectedItem].IsSome && items.[selectedItem].Value :? Weapon with
            | true ->
                GD.Print "TRUE????"
                selectedWeaponSlotOnCombatEnter <- selectedItem
                selectedWeaponOnCombatEnter <- Some (items.[selectedItem].Value :?> Weapon)
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
                                match actorButtons.SecondaryAttackPressed with
                                    | true ->
                                        usePrimaryAttackMode <- false
                                        Some AttackState
                                    | false ->
                                        match actorButtons.ReloadPressed with
                                            | true ->
                                                Some ReloadState
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
        // items
        // |> Array.map (fun a ->
                      // match a.IsSome with
                          // | true ->
                                // GD.Print (a.Value :?> Item).Name
                          // | false ->
                                // ())

        let oldMagazine = (items.[selectedItem].Value :?> Gun).Magazine
        let newMagazine = getBestMagazineAmongItems selectedWeaponOnCombatEnter.Value.WeaponType

        match oldMagazine.IsSome && newMagazine.IsSome with
            | true ->
                match newMagazine.Value.StoredAmmo > oldMagazine.Value.StoredAmmo with
                    | false ->
                        GD.Print "Old magazine has more ammo than any other magazine"
                    | true ->
                        (items.[selectedItem].Value :?> Gun).Magazine <- newMagazine

                        // Remove old mag from inventory
                        removeItemFromInventory newMagazine.Value

                        // Add old mag to inventory
                        addItemToInventory oldMagazine.Value

                        GD.Print "Swapped old mag with new mag"
            | false ->
                match newMagazine.IsSome with
                    | true ->
                        (items.[selectedItem].Value :?> Gun).Magazine <- newMagazine
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
    let gunFire(rayCast : RayCast, recoilPushbackMultiplier : float32) =
        let fireBullet() =
            let rotation = this.GetGlobalTransform().basis.GetAxis(0)
            // SPONGE a bit expensive, but only done when shooting so ok.
            // It's done because the vector by default is 90 degrees off
            let vector = rotateVector90Degrees (vector3To2 rotation)
            this.ApplyImpulse(Vector3.Zero, vector2To3(vector).Normalized() * recoilPushbackMultiplier)

            rayCast.ForceRaycastUpdate()
            let magazine = (selectedWeaponOnCombatEnter.Value :?> Gun).Magazine.Value
            magazine.StoredAmmo <- magazine.StoredAmmo - 1
            match rayCast.IsColliding() with
                | true ->
                    let body = rayCast.GetCollider()
                    match body :? BaseActor with
                        | true ->
                            (body :?> BaseActor).DamageProjectile 5.0
                        | false ->
                            ()
                | false ->
                    ()

        match selectedWeaponOnCombatEnter.Value :? Gun with
            | false ->
                GD.Print ("ERROR: WEAPON ", selectedWeaponOnCombatEnter.Value.Name, " IS NOT A GUN BUT IT CAN SHOOT!! FIX THIS IN FILE =ITEMS.FS!!!=")
            | true ->
                let gun = (selectedWeaponOnCombatEnter.Value :?> Gun)
                match gun.Magazine.IsSome with
                    | false ->
                        GD.Print "Gun has no mag"
                        // No magazine loaded
                    | true ->
                        match gun.Magazine.Value.StoredAmmo > 0 with
                            | false ->
                                GD.Print "Gun out of ammo"
                                // Gun has no ammo left
                            | true ->
                                fireBullet()
                                ()
                                // Shoot

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


    // *** Attack state body
    // Handles the cooldown after attacking
    let mutable attackCooldown : float32 = 2.0f

    let startAttack() =
        setWeaponAnimationTimed "Attack" attackCooldown
        attackCooldown <- selectedWeaponOnCombatEnter.Value.AttackCooldown
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
                        None

    let integrateForcesAttack (delta : float32) (physicsState : PhysicsDirectBodyState) =
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
        match initializeCombatState() with
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
    member this.AimTarget
        with get () = aimTarget
        and set (value) = aimTarget <- value

    member this.ActorButtons
        with get () = actorButtons
        and set (value) = actorButtons <- value

    member this.CommandChildren
        with get () = commandChildren

    member this.CommandParent
        with get () = commandParent
        and set (value) = commandParent <- value

    // ** Functions

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

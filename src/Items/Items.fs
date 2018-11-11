namespace Items

open Godot
open Actor.IActor

type WeaponType = Knife | Pistol | Rifle | Sniper | Smg | Lmg | Shotgun
type WeaponAttackModes = Bayonet | Knife | Bash | Buckshot | Slug

// module ItemTypes =
    // let Pistol = "Pistol"
    // let Rifle = "Rifle"
    // let Sniper = "Sniper"
    // let Smg = "Smg"
    // let Lmg = "Lmg"
    // let Shotgun = "Shotgun"
    // let Knife = "Knife"

[<AbstractClass>]
type Item(name) =
    inherit RigidBody()
    member this.Name : string = name

// * Arms
// ** Weapons
[<AbstractClass>]
type Weapon(name, weaponType, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Item(name)
    member this.WeaponType : WeaponType = weaponType
    member this.AttackCooldown : float32 = attackCooldown
    member this.Damage : float = damage
    member this.Force : int = force
    member this.PrimaryAttackMode : WeaponAttackModes option = primaryAttackMode
    member this.SecondaryAttackMode : WeaponAttackModes option = secondaryAttackMode

[<AbstractClass>]
type Knife(name, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Weapon(name, WeaponType.Knife, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode)

type GenericKnife() =
    inherit Knife("Generic Knife", 60.0f,18.0, 100, Some WeaponAttackModes.Knife, Some WeaponAttackModes.Bash)

// *** Guns
// **** Functions
module GunFireHelpers =
    let genericGunFire(rayCast : RayCast) =
        rayCast.ForceRaycastUpdate()
        match rayCast.IsColliding() with
            | true ->
                let body = rayCast.GetCollider()
                match body :? IActor with
                    | true ->
                        (body :?> IActor).DamageProjectile 5.0
                    | false ->
                        ()
            | false ->
                ()

// ** Helpers
module ItemHelperFunctions =
    let getWeaponTypeString (weaponType : WeaponType) =
        match weaponType with
            | WeaponType.Knife ->
                Some "Knife"
            | WeaponType.Pistol ->
                Some "Pistol"
            | WeaponType.Rifle ->
                Some "Rifle"
            | WeaponType.Smg ->
                Some "Smg"
            | WeaponType.Lmg ->
                Some "Lmg"
            | WeaponType.Shotgun ->
                Some "Pistol"
            | _ ->
                GD.Print "Weapon type not properly implemented!!!"
                None

// ** Magazines
[<AbstractClass>]
type Magazine(ammoType : WeaponType, ammoCapacity) =
    inherit Item(
         match ItemHelperFunctions.getWeaponTypeString(ammoType).IsSome with
             | true ->
                ItemHelperFunctions.getWeaponTypeString(ammoType).Value + " magazine"
             | false ->
                 " magazine")

    member this.AmmoCapacity : int = ammoCapacity
    member this.AmmoType : WeaponType = ammoType
    member val StoredAmmo : int = ammoCapacity with get,set

type PistolMagazine() =
    inherit Magazine(WeaponType.Pistol, 12)

type RifleMagazine() =
    inherit Magazine(WeaponType.Rifle, 30)

type SniperMagazine() =
    inherit Magazine(WeaponType.Sniper, 5)

type SmgMagazine() =
    inherit Magazine(WeaponType.Smg, 40)

type LmgMagazine() =
    inherit Magazine(WeaponType.Lmg, 50)

type ShotgunMagazine() =
    inherit Magazine(WeaponType.Shotgun, 8)

// **** Data
[<AbstractClass>]
type Gun(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Weapon(name, weaponType, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode)
    member this.BoltCooldown : float32 = boltCooldown
    member val Magazine : Magazine option = None with get,set
    member val IsBulletInChamber : bool = false with get,set
    member val IsHammerReady : bool = true with get,set
    member this.EjectBullet() =
        GD.Print ("Ejecting bullet!!, ammo left", this.Magazine.Value.StoredAmmo)
        this.IsBulletInChamber <- false

    member this.Bolt() =
        match this.IsBulletInChamber with
            | true ->
                this.EjectBullet()
            | false ->
                ()

        match this.Magazine.IsSome && this.Magazine.Value.StoredAmmo > 0 with
            | true ->
                this.IsBulletInChamber <- true
                this.Magazine.Value.StoredAmmo <- (this.Magazine.Value.StoredAmmo - 1)
            | false ->
                ()

    abstract Fire : RayCast -> unit
    abstract ReleaseTrigger : unit -> unit

    member this.PullTrigger rayCast : bool =
        match this.IsBulletInChamber && this.IsHammerReady with
            | false ->
                false
            | true ->
                this.IsHammerReady <- false
                this.Fire rayCast

                match this.Magazine.Value.StoredAmmo = 0 with
                    | true ->
                        this.IsBulletInChamber <- false
                    | false ->
                        ()
                true

// ***** Gun triggers
[<AbstractClass>]
type BoltAction(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Gun(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode)
    // I should have a separate key for bolting!!

    default this.Fire rayCast =
        GunFireHelpers.genericGunFire rayCast
        this.EjectBullet()

    default this.ReleaseTrigger() =
        this.IsHammerReady <- true

[<AbstractClass>]
type SemiAuto(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Gun(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode)
    // I should have a separate key for bolting!!

    default this.Fire rayCast =
        GunFireHelpers.genericGunFire rayCast
        this.Bolt()

    default this.ReleaseTrigger() =
        this.IsHammerReady <- true

[<AbstractClass>]
type FullAuto(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Gun(name, weaponType, attackCooldown, boltCooldown, damage, force, primaryAttackMode, secondaryAttackMode)

    member val IsBolted : bool = false with get,set
    // I should have a separate key for bolting!!

    default this.Fire rayCast =
        GunFireHelpers.genericGunFire rayCast
        this.Bolt()

        this.IsHammerReady <- true

    default this.ReleaseTrigger() =
        ()

// ** Weapons
type Glock18() =
    inherit SemiAuto("Glock 18", WeaponType.Pistol, 0.1f, 1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Deagle() =
    inherit SemiAuto("Deagle", WeaponType.Pistol, 0.1f, 1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type M16a1() =
    inherit SemiAuto("M16a1", WeaponType.Pistol, 0.1f, 1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Ak47() =
    inherit FullAuto("Ak47", WeaponType.Rifle, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Kar98() =
    inherit BoltAction("Kar98", WeaponType.Sniper, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

type Mp5() =
    inherit FullAuto("Mp5", WeaponType.Smg, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

type Uzi() =
    inherit FullAuto("Uzi", WeaponType.Smg, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

type M60() =
    inherit FullAuto("M60", WeaponType.Lmg, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type PKM() =
    inherit FullAuto("PKM", WeaponType.Lmg, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Spas12() =
    inherit SemiAuto("Spas12", WeaponType.Shotgun, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Buckshot, Some WeaponAttackModes.Bash)

type R870() =
    inherit SemiAuto("R870", WeaponType.Shotgun, 0.1f,  1.0f, 18.0, 100, Some WeaponAttackModes.Buckshot, Some WeaponAttackModes.Bash)

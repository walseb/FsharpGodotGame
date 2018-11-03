namespace Items

open Godot

type GunTrigger = FullAuto | SemiAuto | BoltAction
type WeaponAttackModes = Bayonet | Knife | Bash | Buckshot | Slug

module ItemTypes =
    let Pistol = "Pistol"
    let Rifle = "Rifle"
    let Sniper = "Sniper"
    let Smg = "Smg"
    let Lmg = "Lmg"
    let Shotgun = "Shotgun"
    let Knife = "Knife"

[<AbstractClass>]
type Item(name) =
    inherit RigidBody()
    member this.Name : string = name

// * Weapons
// ** Magazines
[<AbstractClass>]
type Magazine(ammoType, ammoCapacity) =
    inherit Item(ammoType + " magazine")
    member this.AmmoCapacity : int = ammoCapacity
    member this.AmmoType : string = ammoType
    member val StoredAmmo : int = ammoCapacity with get,set

type PistolMagazine() =
    inherit Magazine(ItemTypes.Pistol, 12)

type RifleMagazine() =
    inherit Magazine(ItemTypes.Rifle, 900)

type SniperMagazine() =
    inherit Magazine(ItemTypes.Sniper, 5)

type SmgMagazine() =
    inherit Magazine(ItemTypes.Smg, 40)

type LmgMagazine() =
    inherit Magazine(ItemTypes.Lmg, 50)

type ShotgunMagazine() =
    inherit Magazine(ItemTypes.Shotgun, 8)

// ** Weapons
[<AbstractClass>]
type Weapon(name, weaponType, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Item(name)
    member this.WeaponType : string = weaponType
    member this.AttackCooldown : float32 = attackCooldown
    member this.Damage : float = damage
    member this.Force : int = force
    member this.PrimaryAttackMode : WeaponAttackModes option = primaryAttackMode
    member this.SecondaryAttackMode : WeaponAttackModes option = secondaryAttackMode

[<AbstractClass>]
type Knife(name, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Weapon(name, ItemTypes.Knife, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode)

type GenericKnife() =
    inherit Knife("Generic Knife", 60.0f,18.0, 100, Some WeaponAttackModes.Knife, Some WeaponAttackModes.Bash)

// *** Guns
[<AbstractClass>]
type Gun(name, itemType, attackCooldown, gunTrigger, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Weapon(name, itemType, attackCooldown, damage, force, primaryAttackMode, secondaryAttackMode)
    member this.GunTrigger : GunTrigger = gunTrigger
    member val Magazine : Magazine option = None with get,set

[<AbstractClass>]
type Pistol(name, attackCooldown, gunTrigger, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Gun(name, ItemTypes.Pistol, attackCooldown, gunTrigger, damage, force, primaryAttackMode, secondaryAttackMode)

type Glock18() =
    inherit Pistol("Glock 18", 0.1f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Deagle() =
    inherit Pistol("Deagle", 0.1f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

[<AbstractClass>]
type Rifle(name, attackCooldown, gunTrigger , damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Rifle, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type M16a1() =
    inherit Rifle("M16a1", 0.1f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Ak47() =
    inherit Rifle("Ak47", 0.1f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

[<AbstractClass>]
type Sniper(name, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Pistol, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type Kar98() =
    inherit Sniper("Kar98", 1200.0f, GunTrigger.BoltAction, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

[<AbstractClass>]
type Smg(name, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Smg, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type Mp5() =
    inherit Smg("Mp5", 1200.0f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

type Uzi() =
    inherit Smg("Uzi", 1200.0f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

[<AbstractClass>]
type Lmg(name, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Lmg, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type M60() =
    inherit Lmg("M60", 1200.0f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type PKM() =
    inherit Lmg("PKM", 1200.0f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

[<AbstractClass>]
type Shotgun(name, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Shotgun, attackCooldown, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type Spas12() =
    inherit Shotgun("Spas12", 1200.0f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Buckshot, Some WeaponAttackModes.Bash)

type R870() =
    inherit Shotgun("R870", 1200.0f, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Buckshot, Some WeaponAttackModes.Bash)

// * Helpers
module ItemHelperFunctions =
    let getWeaponType (weapon : Weapon) =
            match weapon with
                | :? Knife ->
                    Some "Knife"
                | :? Pistol ->
                    Some "Pistol"
                | :? Rifle ->
                    Some "Rifle"
                | :? Smg ->
                    Some "Smg"
                | :? Lmg ->
                    Some "Lmg"
                | :? Shotgun ->
                    Some "Pistol"
                | _ ->
                    GD.Print "Weapon type not properly implemented!!!"
                    None

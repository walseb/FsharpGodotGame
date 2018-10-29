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
    inherit Magazine(ItemTypes.Rifle, 30)

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
type Weapon(name, weaponType, rateOfFire, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Item(name)
    member this.WeaponType : string = weaponType
    member this.RateOfFire : int = rateOfFire
    member this.Damage : float = damage
    member this.Force : int = force
    member this.PrimaryAttackMode : WeaponAttackModes option = primaryAttackMode
    member this.SecondaryAttackMode : WeaponAttackModes option = secondaryAttackMode

[<AbstractClass>]
type Knife(name, rateOfFire, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Weapon(name, ItemTypes.Knife, rateOfFire, damage, force, primaryAttackMode, secondaryAttackMode)

type GenericKnife() =
    inherit Knife("Generic Knife", 60,18.0, 100, Some WeaponAttackModes.Knife, Some WeaponAttackModes.Bash)

// *** Guns
[<AbstractClass>]
type Gun(name, itemType, rateOfFire, gunTrigger, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Weapon(name, itemType, rateOfFire, damage, force, primaryAttackMode, secondaryAttackMode)
    member this.GunTrigger : GunTrigger = gunTrigger
    member val Magazine : Magazine option = None with get,set

[<AbstractClass>]
type Pistol(name, rateOfFire, gunTrigger, damage, force, primaryAttackMode, secondaryAttackMode) =
    inherit Gun(name, ItemTypes.Pistol, rateOfFire, gunTrigger, damage, force, primaryAttackMode, secondaryAttackMode)

type Glock18() =
    inherit Pistol("Glock 18", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Deagle() =
    inherit Pistol("Deagle", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

[<AbstractClass>]
type Rifle(name, rateOfFire, gunTrigger , damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Rifle, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type M16a1() =
    inherit Rifle("M16a1", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type Ak47() =
    inherit Rifle("Ak47", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

[<AbstractClass>]
type Sniper(name, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Pistol, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type Kar98() =
    inherit Sniper("Kar98", 1200, GunTrigger.BoltAction, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

[<AbstractClass>]
type Smg(name, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Smg, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type Mp5() =
    inherit Smg("Mp5", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

type Uzi() =
    inherit Smg("Uzi", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bayonet)

[<AbstractClass>]
type Lmg(name, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Lmg, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type M60() =
    inherit Lmg("M60", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

type PKM() =
    inherit Lmg("PKM", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Slug, Some WeaponAttackModes.Bash)

[<AbstractClass>]
type Shotgun(name, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode) =
    inherit Gun(name, ItemTypes.Shotgun, rateOfFire, gunTrigger, damage, force, primaryWeaponAttackMode, secondaryWeaponAttackMode)

type Spas12() =
    inherit Shotgun("Spas12", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Buckshot, Some WeaponAttackModes.Bash)

type R870() =
    inherit Shotgun("R870", 1200, GunTrigger.SemiAuto, 18.0, 100, Some WeaponAttackModes.Buckshot, Some WeaponAttackModes.Bash)

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

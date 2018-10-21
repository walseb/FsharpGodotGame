namespace Items

open Godot

type GunTrigger = FullAuto | SemiAuto | BoltAction

module WeaponAttack =
    let GunFire(rayCast : RayCast) =
        GD.Print "GUN FIRES!!"
        rayCast.ForceRaycastUpdate()
        match rayCast.IsColliding() with
            | true ->
                let body = rayCast.GetCollider()
                ()
            | false ->
                ()

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

[<AbstractClass>]
type Magazine(ammoType, ammoCapacity : int) =
    inherit Item(ammoType + " magazine")
    member this.AmmoCapacity : int = ammoCapacity
    member val StoredAmmo : int = ammoCapacity

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

[<AbstractClass>]
type Weapon(name, weaponType, rateOfFire, damage, force) =
    inherit Item(name)
    member this.WeaponType : string = weaponType
    member this.RateOfFire : int = rateOfFire
    member this.Damage : float = damage
    member this.Force : int = force
    abstract member Attack : RayCast -> unit

[<AbstractClass>]
type Knife(name, rateOfFire, damage, force) =
    inherit Weapon(name, ItemTypes.Knife, rateOfFire, damage, force)

type GenericKnife() =
    inherit Knife("Generic Knife", 60,18.0, 100)
    override this.Attack rayCast =
        GD.Print ("Generic Knife", " attack")

[<AbstractClass>]
type Gun(name, itemType, rateOfFire, gunTrigger, damage, force) =
    inherit Weapon(name, itemType, rateOfFire, damage, force)
    member this.GunTrigger : GunTrigger = gunTrigger
    member val Magazine : Magazine option = None with get,set

[<AbstractClass>]
type Pistol(name, rateOfFire, gunTrigger, damage, force) =
    inherit Gun(name, ItemTypes.Pistol, rateOfFire, gunTrigger, damage, force)
    override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type Glock18() =
    inherit Pistol("Glock 18", 1200, GunTrigger.SemiAuto, 18.0, 100)
    override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type Deagle() =
    inherit Pistol("Deagle", 1200, GunTrigger.SemiAuto, 18.0, 100)

[<AbstractClass>]
type Rifle(name, rateOfFire, gunTrigger , damage, force) =
    inherit Gun(name, ItemTypes.Rifle, rateOfFire, gunTrigger, damage, force)
    override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type M16a1() =
    inherit Rifle("M16a1", 1200, GunTrigger.SemiAuto, 18.0, 100)

type Ak47() =
    inherit Rifle("Ak47", 1200, GunTrigger.SemiAuto, 18.0, 100)

[<AbstractClass>]
type Sniper(name, rateOfFire, gunTrigger, damage, force) =
    inherit Gun(name, ItemTypes.Pistol, rateOfFire, gunTrigger, damage, force)
    override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type Kar98() =
    inherit Sniper("Kar98", 1200, GunTrigger.BoltAction, 18.0, 100)

[<AbstractClass>]
type Smg(name, rateOfFire, gunTrigger, damage, force) =
    inherit Gun(name, ItemTypes.Smg, rateOfFire, gunTrigger, damage, force)
    override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type Mp5() =
    inherit Smg("Mp5", 1200, GunTrigger.SemiAuto, 18.0, 100)

type Uzi() =
    inherit Smg("Uzi", 1200, GunTrigger.SemiAuto, 18.0, 100)

[<AbstractClass>]
type Lmg(name, rateOfFire, gunTrigger, damage, force) =
    inherit Gun(name, ItemTypes.Lmg, rateOfFire, gunTrigger, damage, force)
     override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type M60() =
    inherit Lmg("M60", 1200, GunTrigger.SemiAuto, 18.0, 100)

type PKM() =
    inherit Lmg("PKM", 1200, GunTrigger.SemiAuto, 18.0, 100)

[<AbstractClass>]
type Shotgun(name, rateOfFire, gunTrigger, damage, force) =
    inherit Gun(name, ItemTypes.Shotgun, rateOfFire, gunTrigger, damage, force)
    override this.Attack rayCast =
        WeaponAttack.GunFire(rayCast)

type Spas12() =
    inherit Shotgun("Spas12", 1200, GunTrigger.SemiAuto, 18.0, 100)

type R870() =
    inherit Shotgun("R870", 1200, GunTrigger.SemiAuto, 18.0, 100)

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

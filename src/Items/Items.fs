namespace Items

open Godot

type GunTrigger = FullAuto | SemiAuto | BoltAction


[<AbstractClass>]
type Item(name) =
    inherit RigidBody()
    let name : string = name

[<AbstractClass>]
type Weapon(name, rateOfFire, damage, force) =
    inherit Item(name)
    let rateOfFire : int = rateOfFire
    let damage : float = damage
    let force : int = force
    abstract member Attack : unit

[<AbstractClass>]
type Knife(name, rateOfFire, damage, force) =
    inherit Weapon(name, rateOfFire, damage, force)

type GenericKnife() =
    inherit Knife("Generic Knife", 60,18.0, 100)
    override this.Attack : unit =
        GD.Print ("Generic Knife", " attack")

[<AbstractClass>]
type Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Weapon(name, rateOfFire, damage, force)
    let gunTrigger : GunTrigger = gunTrigger
    let ammoCapacity : int = ammoCapacity

[<AbstractClass>]
type Pistol(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Attack : unit =
        GD.Print ("name", " FIRE")

type Glock18() =
    inherit Pistol("Glock 18", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type Deagle() =
    inherit Pistol("Deagle", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Rifle(name, rateOfFire, gunTrigger , damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Attack : unit =
        GD.Print ("name", " FIRE")

type M16a1() =
    inherit Rifle("M16a1", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type Ak47() =
    inherit Rifle("Ak47", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Smg(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Attack : unit =
        GD.Print ("name", " FIRE")

type Mp5() =
    inherit Smg("Mp5", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type Uzi() =
    inherit Smg("Uzi", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Lmg(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
     override this.Attack : unit =
         GD.Print ("name", " FIRE")

type M60() =
    inherit Lmg("M60", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type PKM() =
    inherit Lmg("PKM", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Shotgun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Attack : unit =
        GD.Print ("name", " FIRE")

type Spas12() =
    inherit Shotgun("Spas12", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type R870() =
    inherit Shotgun("R870", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

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

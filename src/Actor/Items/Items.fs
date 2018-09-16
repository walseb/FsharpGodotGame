namespace Items

open Godot

type GunTrigger = FullAuto | SemiAuto | BoltAction

[<AbstractClass>]
type Item(name) =
    inherit RigidBody()
    let name : string = name

[<AbstractClass>]
type Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Item(name)
    let rateOfFire : int = rateOfFire
    let gunTrigger : GunTrigger = gunTrigger
    let damage : float = damage
    let force : int = force
    let ammoCapacity : int = ammoCapacity
    abstract member Fire : unit

[<AbstractClass>]
type Pistol(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Fire : unit =
        GD.Print ("name", " FIRE")

type Glock18() =
    inherit Pistol("Glock 18", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type Deagle() =
    inherit Pistol("Deagle", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Rifle(name, rateOfFire, gunTrigger , damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Fire : unit =
        GD.Print ("name", " FIRE")

type M16a1() =
    inherit Rifle("M16a1", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type Ak47() =
    inherit Rifle("Ak47", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Smg(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Fire : unit =
        GD.Print ("name", " FIRE")

type Mp5() =
    inherit Smg("Mp5", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type Uzi() =
    inherit Smg("Uzi", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type LMG(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
     override this.Fire : unit =
         GD.Print ("name", " FIRE")

type M60() =
    inherit LMG("M60", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type PKM() =
    inherit LMG("PKM", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

[<AbstractClass>]
type Shotgun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity) =
    inherit Gun(name, rateOfFire, gunTrigger, damage, force, ammoCapacity)
    override this.Fire : unit =
        GD.Print ("name", " FIRE")

type Spas12() =
    inherit Shotgun("Spas12", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

type R870() =
    inherit Shotgun("R870", 1200, GunTrigger.SemiAuto, 18.0, 100, 30)

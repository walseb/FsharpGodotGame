namespace Actor.IActor

open Godot

[<AbstractClass>]
type IActor() =
    inherit RigidBody()
    abstract member DamageMelee : float -> unit
    abstract member DamageProjectile : float -> unit

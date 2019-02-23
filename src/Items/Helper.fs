namespace Items

open Godot
open Items

module Helpers =
    let getWeapon(items : Item[]) =
        items
        |> Array.pick (fun item ->
                       match item :? Weapon with
                       | true -> Some item
                       | false -> None)

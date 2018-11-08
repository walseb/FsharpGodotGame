namespace References

open Godot
open Actor

[<AbstractClass; Sealed>]
type ReferencesStored private () =
    static member val PlayerBoxed : Object option = None with get, set
    static member val Player : ActorObject option = None with get, set

// This class HAS to be at the top of the scene so that the references can be assigned in time
type ReferenceManager() as this =
    inherit Node()

    let playerActorName = "Player"

    let getPlayer() =
        this.GetTree().GetRoot().GetNode(new NodePath "Spatial").GetNode(new NodePath (playerActorName))

    override this._Ready() =
        ReferencesStored.Player <- Some (getPlayer() :?> ActorObject)
        ReferencesStored.PlayerBoxed <- Some (getPlayer() :> Object)
        GD.Print "DONE FETCHING REFERENCES"

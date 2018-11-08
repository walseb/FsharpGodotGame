namespace References

open Godot
open Actor

[<AbstractClass; Sealed>]
type ReferencesStored private () =
    // ** References
    static member val PlayerBoxed : Object option = None with get, set
    static member val Player : ActorObject option = None with get, set
    static member val NavigationMesh : Navigation option = None with get, set

// This class HAS to be at the top of the scene so that the references can be assigned in time
type ReferenceManager() as this =
    inherit Node()
    // ** Fetch references

    // *** Functions
    let playerActorName = "Player"

    let getPlayer() =
        this.GetTree().GetRoot().GetNode(new NodePath "Spatial").GetNode(new NodePath (playerActorName))

    // *** Fetch them
    override this._Ready() =
        ReferencesStored.Player <- Some (getPlayer() :?> ActorObject)
        ReferencesStored.PlayerBoxed <- Some (getPlayer() :> Object)

        ReferencesStored.NavigationMesh <- Some (this.GetTree().GetRoot().GetChild(0).GetNode(new NodePath("Navigation")) :?> Navigation)
        GD.Print "DONE FETCHING REFERENCES"

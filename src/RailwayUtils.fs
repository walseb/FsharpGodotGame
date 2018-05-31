module RailwayUtils

    open Chessie.ErrorHandling
    open Godot

    //        -> -------
    // -----  ->
    //        -> -------
//    let map singleTrackFunction =
 //       bind (singleTrackFunction >> ok)

//    let map f x =
 //       f x
  //      |> Ok

    let map singleTrackFunction =
        bind (singleTrackFunction >> ok)


    //        ->   ---||
    // ----|| ->  /
    //        -> -------
    //let tap deadEndFunction oneTrackInput =
        //deadEndFunction oneTrackInput
        //oneTrackInput

    let tee f x =
        f x
        |> log
        |> ignore
        x


    let log twoTrackInput =
        let failure msgs = GD.Print (String.concat "ERROR. %A" msgs)
        failureTee failure twoTrackInput

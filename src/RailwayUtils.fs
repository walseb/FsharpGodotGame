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

    // Should ideally not be used
    // Doesn't work properly
    // let derail twoTrackInput =
        // let okTrack value=
            // value
//
        // twoTrackInput
        // |> map okTrack
        // okTrack

    //        ->   ---||
    // ----|| ->  /
    //        -> -------
    //let tap deadEndFunction oneTrackInput =
        //deadEndFunction oneTrackInput
        //oneTrackInput

    let tee f x =
        f x
        |> ignore
        x


    let log twoTrackInput =
        let failure msgs =
            let message = (String.concat "" msgs)
            GD.Print ("LOG: " + message)
        failureTee failure twoTrackInput

    let logErr twoTrackInput =
        let failure msgs =
            let message = (String.concat "" msgs)
            GD.Print ("ERROR: " + message)
        failureTee failure twoTrackInput

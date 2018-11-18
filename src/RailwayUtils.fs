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
let derail (twoTrackInput : Result<'a, 'b>) =
    match twoTrackInput with
        | Ok (a,b) -> Some a
        | _ -> None


        // let mutable test =
        // let okTrack value=
            // test <- value
//
        // twoTrackInput
        // |> successTee
        // test

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

let isOk (twoTrackInput : Result<'a, 'b>) =
    match twoTrackInput with
        | Ok (a,b) -> true
        | _ -> false

let okIfTrue failMessage bool =
    match bool with
        | true ->
            ok()
        | false ->
            fail failMessage

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

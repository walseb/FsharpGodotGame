namespace testestes
module test =

    // let rec loop(index : int, number : int) =
        // printfn "%A" test
//
        // test.[index] <- number
        // match index >= 3 with
            // | true ->
                // printf "END"
            // | _ ->
                // match number >= 3 with
                    // | false ->
                        // loop (index, number + 1)
                    // | true ->
                        // loop (index + 1, 0)
    // loop(0, 0)

    let mutable test : int array = Array.create 4 0

    let rec loop index number : unit =
        printfn "%A" test
        match index = 3 with
            | false ->
                test.[index] <- number + 1
                loop (index + 1) 0;
                ()
            | true ->
                match number = 3 with
                    | true ->
                        ()
                    | false ->
                        test.[index] <- number + 1
                        loop index (number+1);
                        ()

    loop 0 0

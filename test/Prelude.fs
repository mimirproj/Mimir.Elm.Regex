module Tests.Prelude

open Elm.Core
open Expecto

let describe = testList

let test = testCase

module Expect =
    let inline equal actual expected =
        "Should be equal" |> Expect.equal actual expected

let goodInt str int =
    test str <| fun _ ->
        Expect.equal (Just int) (String.toInt str)


let badInt str =
    test str <| fun _ ->
        Expect.equal
            Nothing
            (String.toInt str)


let goodFloat str float =
    test str <| fun _ ->
        Expect.equal (Just float) (String.toFloat str)


let badFloat str =
    test str <| fun _ ->
        Expect.equal
            Nothing
            (String.toFloat str)
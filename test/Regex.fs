module Tests.Regex

open Expecto
open Prelude
open Elm.Core
open Elm.Regex

[<Tests>]
let tests =
    let digit = regex "[0-9]"
    let comma = regex " *, *"
    let location = regex "[oi]n a (\\w+)"
    let places = Regex.find location "I am on a boat in a lake."
    let word = regex "\\w+"

    let userReplace userRegex replacer string =
        match Regex.fromString userRegex with
        | Nothing -> string

        | Just regex ->
            Regex.replace regex replacer string

    let devowel string =
        userReplace "[aeiou]" (fun _ -> "") string

    let reverseWords string =
        userReplace "\\w+" (fun m -> String.reverse m.Match) string

    let simpleTests =
        describe "Simple Stuff"
            [ test "contains 1" <| fun _ -> Expect.equal true (Regex.contains digit "abc123")
              test "contains 2" <| fun _ -> Expect.equal false (Regex.contains digit "abcxyz")

              test "split 1" <| fun _ -> Expect.equal ["tom";"99";"90";"85"] (Regex.split comma "tom,99,90,85")
              test "split 2" <| fun _ -> Expect.equal ["tom";"99";"90";"85"] (Regex.split comma "tom, 99, 90, 85")
              test "split 3" <| fun _ -> Expect.equal ["tom";"99";"90";"85"] (Regex.split comma "tom , 99, 90, 85")
              test "splitAtMost" <| fun _ -> Expect.equal ["tom"; "99, 90, 85"] (Regex.splitAtMost 1 comma "tom , 99, 90, 85")

              test "find 1" <| fun _ -> Expect.equal [ "on a boat"; "in a lake" ] (places |> List.map(fun m -> m.Match))
              test "find 2" <| fun _ -> Expect.equal [ Just "boat"; Just "lake" ] (places |> List.collect(fun m -> m.SubMatches))
              test "findAtMost" <| fun _ -> Expect.equal [ Just "boat" ] (Regex.findAtMost 1 location "I am on a boat in a lake." |> List.collect(fun m -> m.SubMatches))

              test "replace 1" <| fun _ -> Expect.equal  "Th qck brwn fx" (devowel "The quick brown fox")
              test "replace 2" <| fun _ -> Expect.equal  "deliver mined parts" (reverseWords "reviled denim strap")
              test "replaceAtMost" <| fun _ -> Expect.equal  "Bob's denim strap" (Regex.replaceAtMost 1 word (fun _ -> "Bob's") "reviled denim strap")
            ]

    describe "Regex" [
        simpleTests
    ]
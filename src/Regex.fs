module Elm.Regex

open System
open System.Text
open Elm.Core
open System.Text.RegularExpressions



type Options =
    { CaseInsensitive : bool
      Multiline : bool
    }

/// A regular expression.
type Regex = private Regex of System.Text.RegularExpressions.Regex

/// The details about a particular match:
///
///   * `match` - the full string of the match.
///   * `index` - the index of the match in the original string.
///   * `number` - if you find many matches, you can think of each one
///     as being labeled with a `number` starting at one. So the first time you
///     find a match, that is match `number` one. Second time is match `number` two.
///     This is useful when paired with `replace` if replacement is dependent on how
///     many times a pattern has appeared before.
///   * `submatches` - a `Regex` can have [subpatterns][sub], sup-parts that
///     are in parentheses. This is a list of all these submatches. This is kind of
///     garbage to use, and using a package like [`elm/parser`][parser] is
///     probably easier.
///
/// [sub]: https://developer.mozilla.org/en/docs/Web/JavaScript/Guide/Regular_Expressions#Using_Parenthesized_Substring_Matches
/// [parser]: /packages/elm/parser/latest
///
type Match = {
    Match: string
    SubMatches: Maybe<string> list
    Index: int
    Length: int }

module Regex =
    let private mapOptions (options: Options) =
        let mutable regexOptions = RegularExpressions.RegexOptions.None
        if options.Multiline then regexOptions <- regexOptions ||| RegularExpressions.RegexOptions.Multiline
        if options.CaseInsensitive then regexOptions <- regexOptions ||| RegularExpressions.RegexOptions.IgnoreCase
        regexOptions

    /// Create a `Regex` with some additional options. For example, you can define
    /// `fromString` like this:
    ///
    ///     import Regex
    ///
    ///     fromString : String -> Maybe Regex.Regex
    ///     fromString string =
    ///       fromStringWith { caseInsensitive = False, multiline = False } string
    ///
    let fromStringWith options pattern =
        if String.IsNullOrEmpty pattern then
            Nothing

        else
            try
                System.Text.RegularExpressions.Regex(pattern, mapOptions options)
                |> Regex
                |> Just
            with
                | :? ArgumentException ->
                    Nothing

    /// Try to create a `Regex`. Not all strings are valid though, so you get a
    /// `Maybe` back. This means you can safely accept input from users.
    ///
    ///     import Regex
    ///
    ///     lowerCase : Regex.Regex
    ///     lowerCase =
    ///       Regex.fromString "[a-z]+"
    ///       |> Maybe.withDefault Regex.never
    ///
    /// **Note:** There are some [shorthand character classes][short] like `\w` for
    /// word characters, `\s` for whitespace characters, and `\d` for digits. **Make
    /// sure they are properly escaped!** If you specify them directly in your code,
    /// they would look like `"\\w\\s\\d"`.
    ///
    /// [short]: https://www.regular-expressions.info/shorthand.html
    ///
    let fromString =
        fromStringWith { CaseInsensitive = false; Multiline = false }

    /// A regular expression that never matches any string.
    let never =
        System.Text.RegularExpressions.Regex(".^", mapOptions { CaseInsensitive = false; Multiline = false })
        |> Regex

    let private iterBack f (source:'T array) =
        for i = source.Length - 1 downto 0 do
            f (source.[i])

    let private findSeq (Regex regex) (input:string) =
        regex.Matches(input)
        |> Seq.cast<RegularExpressions.Match>
        |> Seq.map(fun m ->
            { Match = m.Value
              Index = m.Index
              Length = m.Length
              SubMatches =
                m.Groups
                |> Seq.cast<Group>
                |> Seq.skipWhile(fun g -> g.Value = m.Value) // If first group equals main match then skip it.
                |> Seq.map(fun m -> Just m.Value)
                |> Seq.toList
               })

    /// Find matches in a string:
    ///
    ///     import Regex
    ///
    ///     location : Regex.Regex
    ///     location =
    ///       Regex.fromString "[oi]n a (\\w+)"
    ///       |> Maybe.withDefault Regex.never
    ///
    ///     places : List Regex.Match
    ///     places =
    ///       Regex.find location "I am on a boat in a lake."
    ///
    ///     -- map .match      places == [ "on a boat", "in a lake" ]
    ///     -- map .submatches places == [ [Just "boat"], [Just "lake"] ]
    ///
    ///    If you need `submatches` for some reason, a library like
    ///    [`elm/parser`][parser] will probably lead to better code in the long run.
    ///
    ///    [parser]: /packages/elm/parser/latest
    ///
    let find (regex:Regex) (input:string) =
        findSeq regex input
        |> Seq.toList

    /// Just like `find` but it stops after some number of matches.
    ///
    /// A library like [`elm/parser`][parser] will probably lead to better code in
    /// the long run.
    ///
    /// [parser]: /packages/elm/parser/latest
    ///
    let findAtMost n (regex:Regex) (input:string) =
        find regex input
        |> Seq.truncate n
        |> List.ofSeq

    /// Just like `replace` but it stops after some number of matches.
    ///
    /// A library like [`elm/parser`][parser] will probably lead to better code in
    /// the long run.
    ///
    /// [parser]: /packages/elm/parser/latest
    ///
    let replaceAtMost n (regex:Regex) replace (input:string) =
        let sb = StringBuilder(input)

        findSeq regex input
        |> Seq.truncate n
        |> Seq.toArray
        |> iterBack(fun m ->
            let replacementString:string = replace m
            sb.Remove(m.Index, m.Length) |> ignore
            sb.Insert(m.Index, replacementString) |> ignore)

        sb.ToString()

    /// Replace matches. The function from `Match` to `String` lets
    /// you use the details of a specific match when making replacements.
    ///
    ///    import Regex
    ///
    ///    userReplace : String -> (Regex.Match -> String) -> String -> String
    ///    userReplace userRegex replacer string =
    ///      case Regex.fromString userRegex of
    ///        Nothing ->
    ///          string
    ///
    ///        Just regex ->
    ///          Regex.replace regex replacer string
    ///
    ///    devowel : String -> String
    ///    devowel string =
    ///      userReplace "[aeiou]" (\_ -> "") string
    ///
    ///    -- devowel "The quick brown fox" == "Th qck brwn fx"
    ///
    ///    reverseWords : String -> String
    ///    reverseWords string =
    ///      userReplace "\\w+" (.match >> String.reverse) string
    ///
    ///    -- reverseWords "deliver mined parts" == "reviled denim strap"
    ///
    let replace (regex:Regex) replace (input:string) =
        replaceAtMost Int32.MaxValue regex replace input

    /// Check to see if a Regex is contained in a string.
    ///
    ///     import Regex
    ///
    ///     digit : Regex.Regex
    ///     digit =
    ///       Regex.fromString "[0-9]"
    ///       |> Maybe.withDefault Regex.never
    ///
    ///     -- Regex.contains digit "abc123" == True
    ///     -- Regex.contains digit "abcxyz" == False
    ///
    let contains (Regex regex) (input:string) =
        regex.IsMatch(input)

    /// Just like `split` but it stops after some number of matches.
    ///
    /// A library like [`elm/parser`][parser] will probably lead to better code in
    /// the long run.
    ///
    /// [parser]: /packages/elm/parser/latest
    ///
    let splitAtMost (count:int) (Regex regex) (input:string) =
        regex.Split(input, count + 1)
        |> List.ofArray

    /// Split a string. The following example will split on commas and tolerate
    /// whitespace on either side of the comma:
    ///
    ///     import Regex
    ///
    ///     comma : Regex.Regex
    ///     comma =
    ///       Regex.fromString " *, *"
    ///       |> Maybe.withDefault Regex.never
    ///
    ///     -- Regex.split comma "tom,99,90,85"     == ["tom","99","90","85"]
    ///     -- Regex.split comma "tom, 99, 90, 85"  == ["tom","99","90","85"]
    ///     -- Regex.split comma "tom , 99, 90, 85" == ["tom","99","90","85"]
    ///
    /// If you want some really fancy splits, a library like
    /// [`elm/parser`][parser] will probably be easier to use.
    ///
    /// [parser]: /packages/elm/parser/latest
    ///
    let split (Regex regex) (input:string) =
        regex.Split(input)
        |> List.ofArray

/// Create a `Regex` which if invalid will raise an exception.
let regex pattern =
    match Regex.fromString pattern with
    | None -> invalidArg "pattern" "Not a valid regex"
    | Some v -> v
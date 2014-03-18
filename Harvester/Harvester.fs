module Harvester

open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator

type Dirty = string
type Turn = string

let private dirtyToTurn cutting words = anyOf matchStr words |>> cutting

let private normal = any

let private parser cutting dirtyWords =
  many (dirtyToTurn cutting dirtyWords <|> normal) >>= foldStrings

let private parse format dirtyWords input =
  let state = makeStringStream input
  state |> parser format dirtyWords |> fst

let harvest (format: Dirty -> Turn) dirtyWords input =
  input |> parse format dirtyWords

module Normalize =

  let harvest (format: Dirty -> Turn) dirtyWords (input: string) =
    let normalized = input.Normalize(System.Text.NormalizationForm.FormKC)
    normalized
    |> parse format dirtyWords
    |> Option.map (fun x -> if x = normalized then input else x)

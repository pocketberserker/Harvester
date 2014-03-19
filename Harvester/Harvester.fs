module Harvester

open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator

type Dirty = string
type Turn = string

let private dirtyToTurn cutting words =
  anyOf matchStr words
  |>> cutting
  .>> setUserState true

let private normal = any

let private parser cutting dirtyWords =
  many (dirtyToTurn cutting dirtyWords <|> normal)
  >>= foldStrings
  .>>. getUserState

let private parse format dirtyWords input =
  let state = StringStreamP<bool>(input, false)
  state |> parser format dirtyWords |> fst

let exists dirtyWords input =
  match input |> parse id dirtyWords with
  | Some(_, exist) -> exist
  | None -> false

let harvest (format: Dirty -> Turn) dirtyWords input =
  match input |> parse format dirtyWords with
  | Some(out, _) -> Some out
  | None -> None

module Normalize =

  let eixsts dirtyWords (input: string) =
    let normalized = input.Normalize(System.Text.NormalizationForm.FormKC)
    normalized
    |> parse id dirtyWords
    |> function | Some(_, exist) -> exist | None -> false

  let harvest (format: Dirty -> Turn) dirtyWords (input: string) =
    let normalized = input.Normalize(System.Text.NormalizationForm.FormKC)
    normalized
    |> parse format dirtyWords
    |> Option.map (fun (x, _) -> if x = normalized then input else x)

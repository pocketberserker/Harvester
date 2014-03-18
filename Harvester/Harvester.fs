module Harvester

open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator

type Dirty = string
type Turn = string

let private dirtyToTurn cutting words = anyOf matchStr words |>> cutting

let private normal = any

let private parser cutting dirtyWords =
  many (dirtyToTurn cutting dirtyWords <|> normal) >>= foldStrings

let harvest (format: Dirty -> Turn) dirtyWords input =
  let state = makeStringStream input
  state |> parser format dirtyWords |> fst

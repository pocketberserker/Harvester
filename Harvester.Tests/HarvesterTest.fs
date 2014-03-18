module Harvester.Tests

open NUnit.Framework
open FsUnit

[<TestFixture>]
module HarvesterTest =

  let format dirty =
    String.replicate (String.length dirty) "*"

  [<Test>]
  let ``filter char`` () =
    "hoge" |> Harvester.harvest format ["h"] |> should equal (Some "*oge")

  [<Test>]
  let ``not filter`` () =
    "hoge" |> Harvester.harvest format ["a"] |> should equal (Some "hoge")

  [<Test>]
  let ``filter chars`` () =
    "hoge" |> Harvester.harvest format ["h"; "e"] |> should equal (Some "*og*")

  [<Test>]
  let ``filter sign`` () =
    "!?.#" |> Harvester.harvest format ["."] |> should equal (Some "!?*#")

  [<Test>]
  let ``filter string`` () =
    "hogefugapiyo" |> Harvester.harvest format ["fuga"] |> should equal (Some "hoge****piyo")

  [<Test>]
  let ``filter strings`` () =
    "hogefugapiyo" |> Harvester.harvest format ["og"; "api"] |> should equal (Some "h**efug***yo")

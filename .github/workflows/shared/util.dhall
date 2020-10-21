-- types for GitHub Actions types
-- from https://github.com/mheiber/github-actions-dhall/
-- in a shared file so all workflow files have the same pinned version
-- note: useing mheiber's branch instead of upstream (regada's) because of missing type for "release" event
let Actions =
      https://raw.githubusercontent.com/mheiber/github-actions-dhall/master/package.dhall sha256:bbe68f2c7dad50a6cc47aa6b8008eb1e4e9670d10002ac87930d48092b5ba36b

let run =
      λ(name : Text) →
      λ(run : Text) →
        Actions.Step::{ name = Some name, run = Some run }
let run =
      λ(name : Text) →
      λ(run : Text) →
        Actions.Step::{
        , name = Some name
        , run = Some run
        }

let checkout =
      Actions.Step::{
      , name = Some "Checkout"
      , uses = Some "actions/checkout@v2"
      }

let usesWith =
      λ(name : Text) →
      λ(uses : Text) →
      λ(`with` : List { mapKey : Text, mapValue : Text }) →
        Actions.Step::{
        , name = Some name
        , uses = Some uses
        , `with` = Some `with`
        }

in  { Actions, run, checkout, usesWith }

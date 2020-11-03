-- types for GitHub Actions types
-- from https://github.com/regadas/github-actions-dhall/
let Actions =
      https://raw.githubusercontent.com/regadas/github-actions-dhall/master/package.dhall sha256:cc677589e6d7e13705d106ec6b90cbaa643942b2721ec30af6e256cfcdc925ed

let run =
      λ(name : Text) →
      λ(run : Text) →
        Actions.Step::{ name = Some name, run = Some run }

let run =
      λ(name : Text) →
      λ(run : Text) →
        Actions.Step::{ name = Some name, run = Some run }

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

let runEnv =
      λ(name : Text) →
      λ(run : Text) →
      λ(env : List { mapKey : Text, mapValue : Text }) →
        Actions.Step::{
        , name = Some name
        , run = Some run
        , env = Some env
        }

let DEFAULT_RETENTION_DAYS = "8"

in  { Actions, run, runEnv, checkout, usesWith, DEFAULT_RETENTION_DAYS }

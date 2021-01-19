-- types for GitHub Actions types
-- from https://github.com/regadas/github-actions-dhall/
let Actions =
      https://raw.githubusercontent.com/regadas/github-actions-dhall/master/package.dhall sha256:cc677589e6d7e13705d106ec6b90cbaa643942b2721ec30af6e256cfcdc925ed

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

let setUpJava =
      usesWith
        "Set up Java"
        "actions/setup-java@v1"
        (toMap { java-version = "11" })

let coursierCache =
      λ(root : Text) →
      usesWith
        "Coursier Cache"
        "coursier/cache-action@v3"
        (toMap { root = root })


let DEFAULT_RETENTION_DAYS = "8"

in  { Actions, run, checkout, coursierCache, setUpJava, usesWith, DEFAULT_RETENTION_DAYS }

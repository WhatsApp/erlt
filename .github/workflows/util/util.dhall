-- types for GitHub Actions types
-- in a shared file so all workflow files have the same pinned version
let Actions =
      https://raw.githubusercontent.com/regadas/github-actions-dhall/master/package.dhall sha256:cc677589e6d7e13705d106ec6b90cbaa643942b2721ec30af6e256cfcdc925ed

let run =
      λ(name : Text) →
      λ(run : Text) →
        Actions.Step::{ name = Some name, run = Some run }

let usesWith =
      λ(name : Text) →
      λ(uses : Text) →
      λ(`with` : List { mapKey : Text, mapValue : Text }) →
        Actions.Step::{
        , name = Some name
        , uses = Some uses
        , `with` = Some `with`
        }

in  { Actions, run, usesWith }

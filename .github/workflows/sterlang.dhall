let Actions = (./shared/util.dhall).Actions

let SterlangJobs = ./shared/gen-sterlang-jobs.dhall "-draft"

in  Actions.Workflow::{
    , name = "Sterlang CI"
    , on = Actions.On::{
      , push = Some Actions.Push::{ paths = Some [ "sterlang/**" ] }
      }
    , jobs = toMap SterlangJobs
    }

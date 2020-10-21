let Actions = (./shared/util.dhall).Actions

let SterlangJobs = ./shared/gen-sterlang-jobs.dhall "-release"

in  Actions.Workflow::{
    , name = "Sterlang CI Release"
    , on = Actions.On::{
      , push = Some Actions.Push::{branches = Some ["releases/**"]}
      }
    , jobs = toMap SterlangJobs
    }

-- see ./README.md for dhall setup
let Util = ./shared/util.dhall

let Actions = Util.Actions

let eqwalizerJob =
      Actions.Job::{
      , runs-on = Actions.RunsOn.Type.ubuntu-latest
      , steps =
        [ Util.checkout
        , Util.setUpJava
        , Util.coursierCache "eqwalizer"
        , Util.run
            "Erlang version"
            "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
        , Util.run
            "test eqwalizer"
            "cd eqwalizer && sbt 'coverage;test;coverageReport;scalafmtCheckAll'"
        ]
      }

in  Actions.Workflow::{
    , name = "Eqwalizer CI"
    , on = Actions.On::{
      , push = Some Actions.Push::{
        , paths = Some [ "eqwalizer/**", ".github/**" ]
        }
      }
    , jobs = toMap { checkEqwalizer = eqwalizerJob }
    }

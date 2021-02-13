-- see ./README.md for dhall setup
let Util = ./shared/util.dhall

let Actions = Util.Actions

let coralizerJob =
      Actions.Job::{
      , runs-on = Actions.RunsOn.Type.ubuntu-latest
      , steps =
        [ Util.checkout
        , Util.setUpJava
        , Util.coursierCache "coralizer"
        , Util.run
            "Erlang version"
            "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
        , Util.run
            "test coralizer"
            "cd coralizer && sbt 'coverage;test;coverageReport;scalafmtCheckAll'"
        ]
      }

in  Actions.Workflow::{
    , name = "Coralizer CI"
    , on = Actions.On::{
      , push = Some Actions.Push::{
        , paths = Some [ "coralizer/**", ".github/**" ]
        }
      }
    , jobs = toMap { checkCoralizer = coralizerJob }
    }

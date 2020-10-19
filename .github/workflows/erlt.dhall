-- see ./README.md for dhall setup
let Shared = ./util/util.dhall
let Actions = Shared.Actions
let run = Shared.run

in  Actions.Workflow::{
    , name = "ErlT CI"
    , on = Actions.On::{ push = Some Actions.Push::{=} }
    , jobs = toMap
        { build = Actions.Job::{
          , runs-on = Actions.RunsOn.Type.ubuntu-latest
          , steps =
            [ Actions.Step::{
              , name = Some "Checkout"
              , uses = Some "actions/checkout@v2"
              }
            , run
                "Erlang version"
                "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
            , run
                "check formatting"
                "rebar3 fmt --check || (echo \"please run 'rebar3 fmt --write'\" && \$(exit 1))"
            , run "test" "cd ./examples && make test"
            ]
          }
        }
    }

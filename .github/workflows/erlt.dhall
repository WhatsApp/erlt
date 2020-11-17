-- see ./README.md for dhall setup
let Util = ./shared/util.dhall

let Actions = Util.Actions

let checkout = Util.checkout

let run = Util.run

let setUpJava = Util.setUpJava

let coursierCache = Util.coursierCache

in  Actions.Workflow::{
    , name = "ErlT CI"
    , on = Actions.On::{ push = Some Actions.Push::{=} }
    , jobs = toMap
        { build = Actions.Job::{
          , runs-on = Actions.RunsOn.Type.ubuntu-latest
          , steps =
            [ checkout
            , setUpJava
            , coursierCache
            , run "download sbtn" "./sbtn/dl.sh"
            , run
                "Erlang OTP release"
                "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
            , run
                "Erlang version"
                "erl -version"
            , run
                "check formatting"
                "rebar3 fmt --check || (echo \"please run 'rebar3 fmt --write'\" && \$(exit 1))"
            , run "warm up sterlang" "cd sterlang; ../sbtn/sbtn sterlangd"
            , run "test" "cd ./examples && make test"
            ]
          }
        }
    }

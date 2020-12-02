-- see ./README.md for dhall setup
let Util = ./shared/util.dhall

let Actions = Util.Actions

let usesWith = Util.usesWith

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
            [ usesWith
                "checkout"
                "actions/checkout@v2"
                (toMap { fetch-depth = "0" })
            , setUpJava
            , coursierCache
            -- , run
            --     "Erlang version"
            --     "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
            -- , run
            --     "check formatting"
            --     "rebar3 fmt --check || (echo \"please run 'rebar3 fmt --write'\" && \$(exit 1))"
            -- , run "test" "make -C tests test-jar"
            , run
                "test with latest sterlang release if sterlang unchanged"
                ''
                git fetch origin master
                local STERLANG_CHANGED_LINES=$(git diff HEAD master -- sterlang | wc -l) &&
                  if [ $STERLANG_CHANGED_LINES = 0 ]; then
                    echo testing with latest sterlang.sh
                    ./scripts/fetch-and-install-sterlang-native &&
                    make -C tests test-native
                    else
                      echo sterlang changes detected
                  fi
                ''
            ]
          }
        }
    }

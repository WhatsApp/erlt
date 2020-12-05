let Util = ./util.dhall

let run = Util.run

let checkout = Util.checkout

let Actions = Util.Actions

let usesWith = Util.usesWith

let setUpJava = Util.setUpJava

let coursierCache = Util.coursierCache

let jarName = "sterlang.jar"

let test =
      Actions.Job::{
      , runs-on = Actions.RunsOn.Type.ubuntu-latest
      , steps =
        [ checkout
        , setUpJava
        , coursierCache
        , run
            "Erlang version"
            "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
        , run "assemble erltc" "rebar3 escriptize"
        , run "test stErlang" "cd sterlang && ../scripts/sbtn 'coverage;test'"
        , run "test erltc + stErlang in dev mode" "make -C sterlang/examples"
        , run "coverageReport" "cd sterlang && ../scripts/sbtn 'coverageReport'"
        ]
      }

let buildJar =
      Actions.Job::{
      , runs-on = Actions.RunsOn.Type.ubuntu-latest
      , steps =
        [ checkout
        , setUpJava
        , coursierCache
        , run "assembly sterlang.jar" "cd sterlang; sbt assembly"
        , usesWith
            "upload sterlang.jar"
            "actions/upload-artifact@v2"
            ( toMap
                { name = jarName
                , path = "sterlang/target/scala-2.13/${jarName}"
                , retention-days = Util.DEFAULT_RETENTION_DAYS
                }
            )
        ]
      }

let toNativeImageJob =
      λ(toUploadSteps : Text → List Actions.Step.Type) →
      λ(nativeName : Text) →
      λ(runsOn : Actions.RunsOn.Type) →
        Actions.Job::{
        , needs = Some [ "buildJar" ]
        , runs-on = runsOn
        , steps =
              [ checkout
              , setUpJava
              , usesWith
                  "Setup GraalVM Environment"
                  "DeLaGuardo/setup-graalvm@master"
                  (toMap { graalvm-version = "20.1.0.java11" })
              , run "Install Native Image Plugin" "gu install native-image"
              , usesWith
                  "Get JAR Artifact"
                  "actions/download-artifact@v2"
                  (toMap { name = jarName })
              , run "ls" "ls -lah"
              , run
                  "Build native image '${nativeName}'"
                  "native-image -R:MaxHeapSize=16m --no-server --no-fallback -jar ${jarName} ${nativeName}"
              , run "if erlang-less, assume we're on mac and brew install" "which erl || brew install erlang"
              ]
            # (toUploadSteps nativeName)
            # [run
                  "test erltc with native image"
                  "mkdir -p erltc/priv && cp ${nativeName} erltc/priv/sterlang && make -C tests test-native"
            ]
        }

in  { buildJar, test, toNativeImageJob }

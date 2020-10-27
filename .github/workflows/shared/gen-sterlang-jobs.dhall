let Util = ./util.dhall

let run = Util.run

let checkout = Util.checkout

let Actions = Util.Actions

let usesWith = Util.usesWith

let setUpJava =
      usesWith
        "Set up Java"
        "actions/setup-java@v1"
        (toMap { java-version = "11" })

let jarName = "sterlang.jar"

let test =
      Actions.Job::{
      , runs-on = Actions.RunsOn.Type.ubuntu-latest
      , steps =
        [ checkout
        , setUpJava
        , run
            "Erlang version"
            "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
        , run "test erlt" "cd ./examples && make test"
        , run
            "test and coverage stErlang"
            "cd sterlang; sbt clean coverage test coverageReport"
        ]
      }

let buildJar =
      Actions.Job::{
      , runs-on = Actions.RunsOn.Type.ubuntu-latest
      , steps =
        [ checkout
        , setUpJava
        , run "assembly sterlang.jar" "cd sterlang; sbt assembly"
        , usesWith
            "upload sterlang.jar"
            "actions/upload-artifact@v2"
            ( toMap
                { name = jarName
                , path = "sterlang/target/scala-2.13/${jarName}"
                , retention-period = Util.DEFAULT_RETENTION_PERIOD
                }
            )
        ]
      }

-- creates a native image.
-- The caller uses `toUploadSteps` to decide
-- what to do with the native image
let toNativeImageJob =
      λ(toUploadSteps : Text → List Actions.Step.Type) →
      λ(nativeName : Text) →
      λ(runsOn : Actions.RunsOn.Type) →
        Actions.Job::{
        , needs = Some [ "buildJar" ]
        , runs-on = runsOn
        , steps =
              [ setUpJava
              , usesWith
                  "Setup GraalVM Environment"
                  "DeLaGuardo/setup-graalvm@2.0"
                  (toMap { graalvm-version = "20.1.0.java11" })
              , run "Install Native Image Plugin" "gu install native-image"
              , usesWith
                  "Get JAR Artifact"
                  "actions/download-artifact@v2"
                  (toMap { name = jarName })
              , run "ls" "ls -lah"
              , run
                  "Build native image '${nativeName}'"
                  "native-image --no-server --no-fallback -O4 -jar ${jarName} ${nativeName}"
              ]
            # toUploadSteps nativeName
        }

in  { buildJar, test, toNativeImageJob }

-- see ./README.md for dhall setup
let Util = ./util.dhall

let run = Util.run

let Actions = Util.Actions

let usesWith = Util.usesWith

let checkout = Actions.Step::{ uses = Some "actions/checkout@v1" }

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
        , run
            "test and coverage"
            "cd sterlang; sbt clean coverage test coverageReport"
        , run "test" "cd ./examples && make test"
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
                }
            )
        ]
      }

let toNativeImageJob =
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
          , usesWith
              "upload artifacts"
              "actions/upload-artifact@v2"
              (toMap { name = nativeName, path = nativeName })
          ]
        }

let genSterlangJobs =
      λ(nativeArtifactSuffix : Text) →
        let ubuntu =
              toNativeImageJob
                ("sterlang-linux" ++ nativeArtifactSuffix)
                Actions.RunsOn.Type.ubuntu-latest

        let mac =
              toNativeImageJob
                ("sterlang-mac" ++ nativeArtifactSuffix)
                Actions.RunsOn.Type.macos-latest

        in  { buildJar, test, mac, ubuntu }

in  genSterlangJobs

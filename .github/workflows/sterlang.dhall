-- see ./README.md for dhall setup
let Shared = ./util/util.dhall

let run = Shared.run

let Actions = Shared.Actions

let usesWith = Shared.usesWith

let buildNativeImage =
      λ(descr : Text) →
      λ(jar : Text) →
      λ(out : Text) →
        run
          descr
          "native-image --no-server --no-fallback -O4 -jar ${jar} ${out}"

let prereqs
    : List Actions.Step.Type
    = [ Actions.Step::{ uses = Some "actions/checkout@v1" }
      , usesWith
          "Set up Java"
          "actions/setup-java@v1"
          (toMap { java-version = "11" })
      , usesWith
          "Setup GraalVM Environment"
          "DeLaGuardo/setup-graalvm@2.0"
          (toMap { graalvm-version = "20.1.0.java11" })
      , run "Install Native Image Plugin" "gu install native-image"
      ]

let jarName = "sterlang.jar"

let toUbuntuJob =
      λ(jarDir : Text) →
      λ(nativeName : Text) →
        let jarPath = jarDir ++ jarName

        in  Actions.Job::{
            , runs-on = Actions.RunsOn.Type.ubuntu-latest
            , steps =
                  prereqs
                # [ run
                      "Erlang version"
                      "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
                  , run
                      "test and coverage"
                      "cd sterlang; sbt clean coverage test coverageReport"
                  , run "test" "cd ./examples && make test"
                  , run "assembly sterlang.jar" "cd sterlang; sbt assembly"
                  , usesWith
                      "upload sterlang.jar"
                      "actions/upload-artifact@v2"
                      (toMap { name = jarName, path = jarPath })
                  , buildNativeImage
                      "Build sterlang native for Linux"
                      jarPath
                      nativeName
                  , usesWith
                      "upload artifacts"
                      "actions/upload-artifact@v2"
                      (toMap { name = nativeName, path = nativeName })
                  ]
            }

let toMacJob =
      λ(nativeName : Text) →
        Actions.Job::{
        , needs = Some [ "ubuntu" ]
        , runs-on = Actions.RunsOn.Type.macos-latest
        , steps =
              prereqs
            # [ usesWith
                  "Get JAR Artifact"
                  "actions/download-artifact@v2"
                  (toMap { name = jarName })
              , run "ls" "ls -lah"
              , buildNativeImage
                  "Build sterlang native for Mac"
                  jarName
                  nativeName
              , usesWith
                  "upload artifacts"
                  "actions/upload-artifact@v2"
                  (toMap { name = "sterlang-mac", path = "sterlang-mac" })
              ]
        }

let ubuntu = toUbuntuJob "sterlang/target/scala-2.13/" "sterlang-linux"

let mac = toMacJob "sterlang-mac"

in  Actions.Workflow::{
    , name = "StErlang CI"
    , on = Actions.On::{ push = Some Actions.Push::{=} }
    , jobs = toMap { ubuntu, mac }
    }

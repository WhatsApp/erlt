-- see ./README.md for dhall setup
let Util = ./shared/util.dhall

let Actions = Util.Actions

let usesWith = Util.usesWith

let Sterlang = ./shared/gen-sterlang-jobs.dhall

let upload =
      λ(nativeName : Text) →
        [ usesWith
            "upload artifacts"
            "actions/upload-artifact@v2"
            ( toMap
                { name = nativeName
                , path = nativeName
                , retention-period = Util.DEFAULT_RETENTION_PERIOD
                }
            )
        ]

in  Actions.Workflow::{
    , name = "Sterlang CI"
    , on = Actions.On::{
      , push = Some Actions.Push::{
        , paths = Some [ "sterlang/**", ".github/**" ]
        }
      }
    , jobs = toMap
        { test = Sterlang.test
        , buildJar = Sterlang.buildJar
        , macBinary =
            Sterlang.toNativeImageJob
              upload
              "sterlang-mac-draft"
              Actions.RunsOn.Type.macos-latest
        , linuxBinary =
            Sterlang.toNativeImageJob
              upload
              "sterlang-linux-draft"
              Actions.RunsOn.Type.ubuntu-latest
        }
    }

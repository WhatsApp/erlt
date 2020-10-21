-- see ./README.md for dhall setup
let Actions = (./shared/util.dhall).Actions

let usesWith = (./shared/util.dhall).usesWith

let Sterlang = ./shared/gen-sterlang-jobs.dhall

let upload =
      λ(nativeName : Text) →
        [ usesWith
            "upload artifacts"
            "actions/upload-artifact@v2"
            ( toMap
                { name = nativeName, path = nativeName, retention-period = "3" }
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

-- see ./README.md for dhall setup
let Util = ./shared/util.dhall

let Sterlang = ./shared/gen-sterlang-jobs.dhall

let Actions = Util.Actions

let checkout = Util.checkout

let run = Util.run

let usesWith = Util.usesWith

let env = Some (toMap { GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}" })

let release_url_step_id = "get_release_url"

let uploadToRelease =
      λ(binaryName : Text) →
        [ run "stat ${binaryName}" "stat ${binaryName}"
        , Actions.Step::{
          , name = Some "Get release url"
          , id = Some release_url_step_id
          , uses = Some "bruceadams/get-release@v1.2.2"
          , env
          }
        , Actions.Step::{
          , name = Some "upload release binary ${binaryName}"
          , uses = Some "actions/upload-release-asset@v1.0.2"
          , env
          , `with` = Some
              ( toMap
                  { upload_url =
                      "\${{ steps.${release_url_step_id}.outputs.upload_url }}"
                  , asset_path = binaryName
                  , asset_name = binaryName
                  , asset_content_type = "application/octet-stream"
                  }
              )
          }
        ]

let sterlangMacUpload =
      Sterlang.toNativeImageJob
        uploadToRelease
        "sterlang-mac"
        Actions.RunsOn.Type.macos-latest

let sterlangLinuxUpload =
      Sterlang.toNativeImageJob
        uploadToRelease
        "sterlang-linux"
        Actions.RunsOn.Type.ubuntu-latest

let uploadErltcBinary =
      let binaryName = "erltc"

      in  Actions.Job::{
          , runs-on = Actions.RunsOn.Type.ubuntu-latest
          , steps =
                [ Actions.Step::{
                  , name = Some "Checkout"
                  , uses = Some "actions/checkout@v2"
                  }
                , run
                    "Erlang version"
                    "erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell"
                , run "escriptize" "rebar3 escriptize"
                , run
                    "prevent conflict between '${binaryName}' file and directory"
                    "rm -rf '${binaryName}'"
                , run
                    "mv escript"
                    "mv ./_build/default/bin/erltc '${binaryName}'"
                ]
              # uploadToRelease binaryName
          }

let uploadVSCodeExtension =
      let binaryName = "erlt-ls.vsix"

      in  Actions.Job::{
          , runs-on = Actions.RunsOn.Type.ubuntu-latest
          , steps =
                [ usesWith
                    "Use Node.js"
                    "actions/setup-node@v1"
                    (toMap { node-version = "12.13.0" })
                , checkout
                , run
                    "build vsix"
                    (     "node --version"
                      ++  " && cd vscode"
                      ++  " && npm install"
                      ++  " && npm run compile"
                      ++  " && npx vsce package -o ${binaryName}"
                      ++  " && mv ${binaryName} .."
                    )
                ]
              # uploadToRelease binaryName
          }

in    Actions.Workflow::{
      , name = "Upload release binary"
      , on = Actions.On::{=}
      , jobs = toMap
          { sterlangMacUpload
          , sterlangLinuxUpload
          , uploadErltcBinary
          , uploadVSCodeExtension
            -- the sterlang mac and linux binary jobs require the jar to be built first
          , buildJar = Sterlang.buildJar
          }
      }
    -- we merge this part in using `//`
    -- because github-actions-dhall is missing types for { on : release }
    -- https://github.com/regadas/github-actions-dhall/issues/23
    ⫽ { on.release = Some { types = [ "created" ] } }

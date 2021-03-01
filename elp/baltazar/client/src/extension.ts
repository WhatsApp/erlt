import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    let serverPath = context.asAbsolutePath(
        path.join('..', 'target', 'debug', 'baltazar')
    )
    let logFile = context.asAbsolutePath(path.join('..', 'server.log'))
    let serverArgs = ["--log-file", logFile, "--no-log-buffering"]

    let serverOptions: ServerOptions = {
        command: serverPath,
        args: serverArgs,
        transport: TransportKind.stdio,
    };

    let clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'erlang' }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/rebar.config')
        }
    };

    client = new LanguageClient(
        'baltazar',
        'Baltazar',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

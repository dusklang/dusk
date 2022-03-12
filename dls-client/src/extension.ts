import * as path from 'path';
import { ExtensionContext } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const extension = process.platform == 'win32' ? '.exe' : '';
    // TODO: support release mode
    const serverPath = context.asAbsolutePath(
        path.join('..', 'target', 'debug', `dls${extension}`)
    );

    client = new LanguageClient(
        'Dusk Language Server',
        { command: serverPath },
        {
            documentSelector: [{ scheme: 'file', language: 'dusk'}],
        }
    );
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if(!client) return undefined;
    return client.stop();
}
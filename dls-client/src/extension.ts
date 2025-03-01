import * as path from 'path';
import { ExtensionContext } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import * as fs from 'fs';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const extension = process.platform == 'win32' ? '.exe' : '';
    const devServerPath = context.asAbsolutePath(
        path.join('..', 'target', 'debug', `dls${extension}`)
    );
    let serverPath = undefined;
    if(fs.existsSync(devServerPath)) {
        serverPath = devServerPath;
    } else {
        serverPath = context.asAbsolutePath(path.join('.', 'server', `dls${extension}`));
    }

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

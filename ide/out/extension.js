"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    console.log('Why3 activated!');
    const disposable = vscode.commands.registerCommand('extension.Why', () => {
        vscode.window.showInformationMessage('Proof: ok!');
    });
    context.subscriptions.push(disposable);
    // The server is implemented in node
    // const serverModule : string = context.asAbsolutePath(
    // 	path.join('server', 'out', 'server.js')
    // );
    const serverModule = "/Users/xavier/Code/whycode/_build/default/bin/main.exe";
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions = {
        run: {
            command: serverModule,
            // args: []
        },
        debug: {
            command: serverModule,
            // options: debugOptions
        }
    };
    // Options to control the language client
    const clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'plaintext' }],
        synchronize: {}
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map
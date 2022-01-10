"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
const vscode_1 = require("vscode");
const vscode = require("vscode");
const node_1 = require("vscode-languageclient/node");
const codeConverter_1 = require("vscode-languageclient/lib/common/codeConverter");
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
    var proofDocs = new Set();
    vscode_1.workspace.onDidChangeTextDocument((event) => {
        if (event.contentChanges.length === 0) {
            return;
        }
        vscode.window.showInformationMessage("Document Change!");
        // check if the document which changed is one we care about
        // if so, forward that to the LSP server.
        if (proofDocs.has(event.document.uri)) {
            // This seems to only be for 'incremental sync' 
            client.sendNotification(node_1.DidChangeTextDocumentNotification.type, (0, codeConverter_1.createConverter)().asChangeTextDocumentParams(event));
        }
    });
    vscode_1.workspace.onDidCloseTextDocument((e) => {
        vscode.window.showInformationMessage("Document Close!");
        // If the document is in our list, then remove it.
        proofDocs.delete(e.uri);
        // And notify the server
        client.sendNotification(node_1.DidCloseTextDocumentNotification.type, (0, codeConverter_1.createConverter)().asCloseTextDocumentParams(e));
    });
    const xx = vscode.commands.registerCommand('extension.Prove', () => {
        // Add a document URI to a list of things we care about
        if (vscode.window.activeTextEditor != undefined) {
            let doc = vscode.window.activeTextEditor.document;
            proofDocs.add(doc.uri);
            client.sendNotification(node_1.DidOpenTextDocumentNotification.type, (0, codeConverter_1.createConverter)().asOpenTextDocumentParams(doc));
        }
        vscode.window.showInformationMessage('File loaded for proof!');
    });
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map
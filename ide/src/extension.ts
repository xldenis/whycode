import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import * as vscode from 'vscode';

import {
	DidChangeTextDocumentNotification,
	DidCloseTextDocumentNotification,
	DidOpenTextDocumentNotification,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	URI
} from 'vscode-languageclient/node';
import { createConverter } from 'vscode-languageclient/lib/common/codeConverter';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	
	console.log('Why3 activated!');

	const disposable = vscode.commands.registerCommand('extension.Why', () => {
		vscode.window.showInformationMessage('Proof: ok!');
	});

	context.subscriptions.push(disposable);

	// The server is implemented in node
	// const serverModule : string = context.asAbsolutePath(
	// 	path.join('server', 'out', 'server.js')
	// );

	const serverModule = "/Users/xavier/Code/whycode/_build/default/bin/main.exe"
	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
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
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'plaintext' }],
		synchronize: {}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'languageServerExample',
		'Language Server Example',
		serverOptions,
		clientOptions
	);

	var proofDocs : Set<vscode.Uri> = new Set();

	workspace.onDidChangeTextDocument( (event) => {
		if (event.contentChanges.length === 0) {
            return;
        }
		vscode.window.showInformationMessage("Document Change!");

		// check if the document which changed is one we care about
		// if so, forward that to the LSP server.
		if (proofDocs.has(event.document.uri)) { 
			// This seems to only be for 'incremental sync' 
			client.sendNotification(DidChangeTextDocumentNotification.type, createConverter().asChangeTextDocumentParams(event));
		}
	});

	workspace.onDidCloseTextDocument((e) => {
		vscode.window.showInformationMessage("Document Close!");
		// If the document is in our list, then remove it.
		proofDocs.delete(e.uri);
		// And notify the server
		client.sendNotification(DidCloseTextDocumentNotification.type,  createConverter().asCloseTextDocumentParams(e));
	});

	const xx = vscode.commands.registerCommand('extension.Prove', () => {
		// Add a document URI to a list of things we care about
		if (vscode.window.activeTextEditor != undefined) {
			let doc = vscode.window.activeTextEditor.document;
			proofDocs.add(doc.uri);
			client.sendNotification(DidOpenTextDocumentNotification.type,  createConverter().asOpenTextDocumentParams(doc));

		}

		vscode.window.showInformationMessage('File loaded for proof!');
	});


	// Start the client. This will also launch the server
	client.start();
}
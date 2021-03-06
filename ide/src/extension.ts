import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import * as vscode from 'vscode';

import {
	DidChangeTextDocumentNotification,
	DidCloseTextDocumentNotification,
	DidOpenTextDocumentNotification,
	DocumentUri,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	URI
} from 'vscode-languageclient/node';
import { createConverter } from 'vscode-languageclient/lib/common/codeConverter';
import { countReset } from 'console';
import { GoalNode, TaskDataProvider, TaskNode, TaskTree } from './tree';

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
		'whycode',
		'Why3',
		serverOptions,
		clientOptions
	);

	var proofDocs : Set<vscode.Uri> = new Set();

	workspace.onDidChangeTextDocument( (event) => {
		if (event.contentChanges.length === 0) {
            return;
        }
		// vscode.window.showInformationMessage("Document Change!");

		// check if the document which changed is one we care about
		// if so, forward that to the LSP server.
		if (proofDocs.has(event.document.uri)) {
			// This seems to only be for 'incremental sync'
			client.sendNotification(DidChangeTextDocumentNotification.type, createConverter().asChangeTextDocumentParams(event));
		}
	});

	workspace.onDidCloseTextDocument((e) => {
		// vscode.window.showInformationMessage("Document Close!");
		// If the document is in our list, then remove it.
		proofDocs.delete(e.uri);
		// And notify the server
		client.sendNotification(DidCloseTextDocumentNotification.type,  createConverter().asCloseTextDocumentParams(e));
	});

	// Seems unnecessary?
	const prove = vscode.commands.registerCommand('extension.ResetSession', () => {
		let uri : DocumentUri = vscode.window.activeTextEditor?.document.uri?.toString()!;
		client.sendNotification('proof/resetSession', { uri: uri, dummy: true});

		vscode.window.showInformationMessage('Session Reset');
	});

	context.subscriptions.push(prove);

	const trans = vscode.commands.registerCommand('why3.runTransformation', (uri:DocumentUri, node : number, command : string) => {
		client.sendNotification('proof/runTransformation', { uri: uri, node: node, command: command });
	});

	context.subscriptions.push(trans);

	const tasks = new TaskTree();
	const treeDataProvider = new TaskDataProvider(tasks);

	const view: vscode.TreeView<TaskNode> = vscode.window.createTreeView('why3_tasks', { treeDataProvider });
	context.subscriptions.push(view);

	client.onNotification('proof/changeTreeNode', params => {
		let node = tasks.getChild(params.id);
		console.log("update node ", params);
		if (params.info[0] == 'Proved' && node != undefined) {
			node.proved = params.info[1];
		};
		treeDataProvider.refresh();
	});

	client.onNotification('proof/removeTreeNode', params => {
		console.log("remove node", params);
		tasks.remove(params.id);
		treeDataProvider.refresh();

	});

	client.onNotification('proof/addTreeNode', params => {
		let node = new GoalNode(params.id, params.parent_id, params.name, false, []);
		tasks.insertChild(params.parent_id, node);
		console.log("create node ", params);
		view.reveal(node);

		treeDataProvider.refresh();
	});

	// Start the client. This will also launch the server
	client.start();


}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
	  return undefined;
	}
	return client.stop();
  }
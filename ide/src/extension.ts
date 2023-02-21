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
	ProtocolNotificationType,
	ProtocolRequestType,
	RequestType,
	ServerOptions,
	TransportKind,
	URI
} from 'vscode-languageclient/node';
import { createConverter } from 'vscode-languageclient/lib/common/codeConverter';
import { countReset } from 'console';
import { GoalNode, TaskDataProvider, TaskNode, TaskTree } from './tree';

let client: LanguageClient;

interface ResolveSessionParams {
	uri: DocumentUri
}

type ResolveSessionResponse = ResolveSessionParams;

namespace ResolveSession {
    export const method: 'proof/resolveSession' = 'proof/resolveSession';
    export const type: RequestType<ResolveSessionParams, ResolveSessionResponse | null, {}> = new RequestType(method, undefined);
}

	
function sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
 

export function activate(context: ExtensionContext) {
	const disposable = vscode.commands.registerCommand('extension.Why', () => {
		vscode.window.showInformationMessage('Activated');
	});

	context.subscriptions.push(disposable);

	let trees : Map<string, TaskTree> = new Map();

	const treeDataProvider = new TaskDataProvider(new TaskTree());

	const view: vscode.TreeView<TaskNode> = vscode.window.createTreeView('taskTree', { treeDataProvider });
	context.subscriptions.push(view);

	vscode.commands.registerCommand('taskTree.runAuto0', (task: TaskNode) => {
		vscode.commands.executeCommand("why3.runTransformation", task.uri, task.id, "Auto_level_0");
	});

	vscode.commands.registerCommand('taskTree.splitVC', (task: TaskNode) => {
		vscode.commands.executeCommand("why3.runTransformation", task.uri, task.id, "Split_VC");
	});

	let serverSetting: string | undefined = vscode.workspace.getConfiguration('whycode').get('executablePath');
	if (serverSetting == undefined || serverSetting == "") {
		serverSetting = "whycode";
	}
	const serverModule: string = serverSetting!;
	const serverArgs: string[] = vscode.workspace.getConfiguration('whycode').get('extraArgs')!;

	console.log('Why3 activated!');

	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {
			command: serverModule,
			args: serverArgs,
		},
		debug: {
			command: serverModule,
			args: serverArgs,
			// options: debugOptions
		}
	};


	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'plaintext' }, { scheme: 'file', language: 'rust' }],
		synchronize: {}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'whycode',
		'Why3',
		serverOptions,
		clientOptions
	);

	var proofDocs: Set<vscode.Uri> = new Set();

	let focusOnTree = function(id : string) {
		treeDataProvider.tree = trees.get(id)!;
		treeDataProvider.refresh();
	};

	workspace.onDidChangeTextDocument((event) => {
		if (event.contentChanges.length === 0) {
			return;
		}

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
		client.sendNotification(DidCloseTextDocumentNotification.type, createConverter().asCloseTextDocumentParams(e));
	});



	vscode.window.onDidChangeActiveTextEditor(async (e) => {
		if (e != undefined) {
			let id = await client.sendRequest(ResolveSession.type, {uri: e.document.uri.toString() } );
			if (id != undefined && trees.get(id.uri) != undefined) {
				focusOnTree(id.uri);
			}
		}
	});

	setTimeout(async function () {
		let uri: DocumentUri = vscode.window.activeTextEditor?.document.uri?.toString()!;

		let id = await client.sendRequest(ResolveSession.type, {uri: uri } );
			if (id != undefined && trees.get(id.uri) != undefined) {
				focusOnTree(id.uri);
			}
	}, 250);


	// Seems unnecessary?
	const prove = vscode.commands.registerCommand('extension.ResetSession', () => {
		let uri: DocumentUri = vscode.window.activeTextEditor?.document.uri?.toString()!;
		client.sendNotification('proof/resetSession', { uri: uri, dummy: true });
		vscode.window.showInformationMessage('Session Reset');
	});
	context.subscriptions.push(prove);

	const reload = vscode.commands.registerCommand('extension.reloadSession', () => {
		let uri: DocumentUri = vscode.window.activeTextEditor?.document.uri?.toString()!;
		console.log(uri);
		client.sendNotification('proof/reloadSession', { uri: uri, dummy: true });

		vscode.window.showInformationMessage('Session Reloaded');
	});
	context.subscriptions.push(reload);

	const trans = vscode.commands.registerCommand('why3.runTransformation', (uri: DocumentUri, node: number, command: string) => {
		client.sendNotification('proof/runTransformation', { uri: uri, node: node, command: command });
	});

	context.subscriptions.push(trans);

	client.onNotification('proof/changeTreeNode', params => {
		let node = trees.get(params.uri)!.getChild(params.id);
		console.log("update node ", params);
		if (params.info[0] == 'Proved' && node != undefined) {
			node.proved = params.info[1];
		};
		treeDataProvider.refresh();
	});

	client.onNotification('proof/removeTreeNode', params => {
		console.log("remove node", params);
		if (!trees.has(params.uri)) {
			trees.set(params.uri, new TaskTree());
		};
		
		trees.get(params.uri)!.remove(params.id);
		treeDataProvider.refresh();

	});

	client.onNotification('proof/addTreeNode', params => {
		let node = new GoalNode(params.uri, params.id, params.parent_id, params.name, false, []);
		if (!trees.has(params.uri)) {
			trees.set(params.uri, new TaskTree());
		};

		trees.get(params.uri)!.insertChild(params.parent_id, node);
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
import { workspace, ExtensionContext, Range, Uri } from "vscode";
import * as vscode from "vscode";

import {
    DidChangeTextDocumentNotification,
    DidCloseTextDocumentNotification,
    DocumentUri,
    LanguageClient,
    LanguageClientOptions,
    NotificationType,
    RequestType,
    ServerOptions,
} from "vscode-languageclient/node";
import { createConverter } from "vscode-languageclient/lib/common/codeConverter";
import { TaskDataProvider, TaskNode, TaskTree } from "./tree";

let client: LanguageClient;

interface ResolveSessionParams {
  uri: DocumentUri;
}
interface Env {
  [name: string]: string;
}

type ResolveSessionResponse = ResolveSessionParams;

export const resolve = new RequestType<ResolveSessionParams, ResolveSessionResponse | null, unknown>(
    "proof/resolveSesion"
);
export const startProof = new NotificationType<{ uri: DocumentUri }>("proof/start");

const trees: Map<string, TaskTree> = new Map();
const proofDocs: Set<vscode.Uri> = new Set();

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function buildCommands(): [string, (...args: any[]) => any][] {
    return [
        [
            "whycode.task_tree.run_auto_0",
            (task: TaskNode) => {
                vscode.commands.executeCommand("why3.run_transformation", task.uri, task.id, "Auto_level_0");
            },
        ],
        [
            "whycode.task_tree.split_vc",
            (task: TaskNode) => {
                vscode.commands.executeCommand("why3.run_transformation", task.uri, task.id, "Split_VC");
            },
        ],
        [
            "whycode.reset_session",
            () => {
                const uri = vscode.window.activeTextEditor?.document.uri?.toString();
                if (uri != undefined) {
                    client.sendRequest("proof/resetSession", { uri: uri });
                    vscode.window.showInformationMessage("Session Reset");
                }
            },
        ],
        [
            "whycode.reload_session",
            () => {
                const uri = vscode.window.activeTextEditor?.document.uri?.toString();
                if (uri == undefined) {
                    return;
                }
                console.log(uri);
                client.sendNotification("proof/reload", {
                    uri: uri,
                });

                vscode.window.showInformationMessage("Session Reloaded");
            },
        ],
        [
            "whycode.replay_session",
            () => {
                const uri = vscode.window.activeTextEditor?.document.uri?.toString();
                if (uri == undefined) {
                    return;
                }
                console.log(uri);
                client.sendNotification("proof/replay", {
                    uri: uri,
                    dummy: true,
                });

                vscode.window.showInformationMessage("Session Reloaded");
            },
        ],
        [
            "whycode.run_transformation",
            (uri: DocumentUri, node: number | Range, command: string) => {
                let target;
                if (typeof node == "number") {
                    target = ["Node", node];
                } else {
                    target = ["Range", node];
                }

                client.sendRequest("proof/runTransformation", {
                    uri: uri,
                    target,
                    command: command,
                });
            },
        ],
        [
            "whycode.start",
            () => {
                const document = vscode.window.activeTextEditor?.document;
                if (document == undefined) {
                    return;
                }

                // proofDocs.add(document.uri);
                client.sendNotification(startProof, { uri: document.uri.toString() });
            },
        ],

        // ['proof/changeTreeNode', params => {
        // 	let node = trees.get(params.uri)!.getChild(params.id);
        // 	console.log("update node ", params);
        // 	if (params.info[0] == 'Proved' && node != undefined) {
        // 		node.proved = params.info[1];
        // 	};
        // 	treeDataProvider.refresh();
        // }],
        // ['proof/removeTreeNode', params => {
        // 	console.log("remove node", params);
        // 	if (!trees.has(params.uri)) {
        // 		trees.set(params.uri, new TaskTree());
        // 	};

        // 	trees.get(params.uri)!.remove(params.id);
        // 	treeDataProvider.refresh();

        // }],
        // ['proof/addTreeNode', params => {
        // 	let node = new GoalNode(params.uri, params.id, params.parent_id, params.name, false, []);
        // 	if (!trees.has(params.uri)) {
        // 		trees.set(params.uri, new TaskTree());
        // 	};

        // 	trees.get(params.uri)!.insertChild(params.parent_id, node);
        // 	console.log("create node ", params);
        // 	view.reveal(node);

    // 	treeDataProvider.refresh();
    // }],
    ];
}

async function startServer(context: ExtensionContext): Promise<LanguageClient> {
    let serverPath: string | undefined = vscode.workspace.getConfiguration("whycode").get("executablePath");
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion

    if (serverPath == undefined || serverPath == "") {
        serverPath = Uri.joinPath(context.extensionUri, "whycode").fsPath;
    }

    let libDir: string | undefined = vscode.workspace.getConfiguration("whycode").get("libPath");
    if (libDir == undefined || libDir == "") {
        libDir = Uri.joinPath(context.extensionUri, "why-lib").fsPath;
    }

    let dataDir: string | undefined = vscode.workspace.getConfiguration("whycode").get("dataPath");
    if (dataDir == undefined || dataDir == "") {
        dataDir = Uri.joinPath(context.extensionUri, "why-data").fsPath;
    }
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const serverArgs: string[] = vscode.workspace.getConfiguration("whycode").get("extraArgs")!;

    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    // const debugOptions = { execArgv: ["--nolazy", "--inspect=6009"] };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const outputChannel = vscode.window.createOutputChannel("WhyCode Server");
    const traceOutputChannel = vscode.window.createOutputChannel("Whycode Server Trace");
    const env: Env = {
        WHY3LIB: libDir,
        WHY3DATA: dataDir,
    };

    const run = {
        command: serverPath,
        args: serverArgs,
        options: {
            env,
        },
    };
    const serverOptions: ServerOptions = {
        run,
        debug: run,
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        outputChannel,
        traceOutputChannel,
        documentSelector: [
            { scheme: "file", language: "rust" },
            { scheme: "file", language: "mlcfg" },
            { scheme: "file", language: "why3" },
        ],
        synchronize: {},
    };

    // Create the language client and start the client.
    client = new LanguageClient("whycode", "Why3", serverOptions, clientOptions);

    await client.start();
    return client;
}

function setupServerEvents() {
    // workspace.onDidChangeTextDocument((event) => {
    //     if (event.contentChanges.length === 0) {
    //         return;
    //     }
    //     // check if the document which changed is one we care about
    //     // if so, forward that to the LSP server.
    //     if (proofDocs.has(event.document.uri)) {
    //         // This seems to only be for 'incremental sync'
    //         client.sendNotification(
    //             DidChangeTextDocumentNotification.type,
    //             createConverter().asChangeTextDocumentParams(event)
    //         );
    //     }
    // });
    // workspace.onDidCloseTextDocument((e) => {
    //     // vscode.window.showInformationMessage("Document Close!");
    //     // If the document is in our list, then remove it.
    //     proofDocs.delete(e.uri);
    //     // And notify the server
    //     client.sendNotification(DidCloseTextDocumentNotification.type, createConverter().asCloseTextDocumentParams(e));
    // });
}

function setupTaskTree(context: ExtensionContext) {
    const treeDataProvider = new TaskDataProvider(new TaskTree());

    const view: vscode.TreeView<TaskNode> = vscode.window.createTreeView("taskTree", {
        treeDataProvider,
    });
    context.subscriptions.push(view);

    const focusOnTree = function (id: string) {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        treeDataProvider.tree = trees.get(id)!;
        treeDataProvider.refresh();
    };

    setTimeout(async function () {
        const uri = vscode.window.activeTextEditor?.document.uri?.toString();
        if (uri == undefined) {
            return;
        }
    // const id = await client.sendRequest(resolve, { uri: uri });
    // if (id != undefined && trees.get(id.uri) != undefined) {
    //     focusOnTree(id.uri);
    // }
    }, 250);

    const disposable = vscode.window.onDidChangeActiveTextEditor(async (e) => {
        if (e != undefined) {
            // const id = await client.sendRequest(resolve, {
            //     uri: e.document.uri.toString(),
            // });
            // if (id != undefined && trees.get(id.uri) != undefined) {
            //     focusOnTree(id.uri);
            // }
        }
    });

    context.subscriptions.push(disposable);
}

export async function activate(context: ExtensionContext) {
    vscode.window.showInformationMessage("Whycode loaded");
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const client = await startServer(context);
    setupServerEvents();
    setupTaskTree(context);

    buildCommands().forEach(([name, command]) => {
        const disposable = vscode.commands.registerCommand(name, command);
        context.subscriptions.push(disposable);
    });
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

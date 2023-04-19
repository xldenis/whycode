import { workspace, ExtensionContext, Range, Uri, window } from "vscode";
import * as vscode from "vscode";
import * as os from "os";

import {
    DocumentUri,
    LanguageClient,
    LanguageClientOptions,
    NotificationType,
    RequestType,
    ServerOptions,
} from "vscode-languageclient/node";
import { TaskDataProvider, TaskNode, TaskTree, treeElem } from "./tree";
import { access, existsSync } from "fs";

let client: LanguageClient;

const languages = [
    { scheme: "file", language: "rust" },
    { scheme: "file", language: "mlcfg" },
    { scheme: "file", language: "why3" },
];

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

export const publishTree = new NotificationType<{ uri: DocumentUri; elems: treeElem[] }>("proof/publishTree");

// TODO: This is a bad hack that should be replaced
let activeTree: string | undefined = undefined;
const trees: Map<string, TaskTree> = new Map();

class TaskProvider implements vscode.TextDocumentContentProvider {
  static scheme = "tasks";

  constructor(private client: LanguageClient) {}
  async provideTextDocumentContent(uri: Uri): Promise<string> {
      const target = JSON.parse(uri.query);
      console.log(target, uri.query, Uri.file(uri.path));
      const resp: string = await client.sendRequest("proof/showTask", {
          uri: Uri.file(uri.path).toString(),
          target,
      });

      return resp;
  }
}

class Config {
  private extensionUri: Uri;

  constructor(context: ExtensionContext) {
      this.extensionUri = context.extensionUri;
  }

  public serverPath(): string {
      const serverPath: string | undefined = vscode.workspace.getConfiguration("whycode").get("executablePath");
      if (serverPath == undefined || serverPath == "") {
          return process.env.DEBUG_SERVER_PATH || Uri.joinPath(this.extensionUri, "whycode").fsPath;
      }
      return serverPath;
  }

  public env(): Env {
      const env: Env = {};

      const libDir: string | undefined = vscode.workspace.getConfiguration("whycode").get("libPath");
      if ((libDir == undefined || libDir == "") && !process.env.DEBUG_SERVER_PATH) {
          env.WHY3LIB = Uri.joinPath(this.extensionUri, "why-lib").fsPath;
      }

      const dataDir: string | undefined = vscode.workspace.getConfiguration("whycode").get("dataPath");
      if ((dataDir == undefined || dataDir == "") && !process.env.DEBUG_SERVER_PATH) {
          env.WHY3DATA = Uri.joinPath(this.extensionUri, "why-data").fsPath;
      }

      const configPath: string | undefined = vscode.workspace.getConfiguration("whycode").get("configPath");
      if (configPath == undefined || configPath == "") {
          env.WHY3CONFIG = os.homedir() + "/.why3.conf";
      }

      return env;
  }

  public serverArgs(): string[] {
      return vscode.workspace.getConfiguration("whycode").get("extraArgs") || [];
  }

  public autoStrategyStart(): string | undefined {
      return vscode.workspace.getConfiguration("whycode").get("auto.start");
  }

  public autoStrategyFinish(): string | undefined {
      return vscode.workspace.getConfiguration("whycode").get("auto.finish");
  }

  public doAuto(): string | undefined {
      return vscode.workspace.getConfiguration("whycode").get("auto.on");
  }
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function buildCommands(): [string, (...args: any[]) => any][] {
    return [
        [
            "whycode.taskTree.runAuto0",
            (e: treeElem) => {
                if (activeTree != undefined) {
                    vscode.commands.executeCommand("whycode.run_transformation", activeTree, e.id, "Auto_level_0");
                }
            },
        ],
        [
            "whycode.taskTree.runAuto1",
            (e: treeElem) => {
                if (activeTree != undefined) {
                    vscode.commands.executeCommand("whycode.run_transformation", activeTree, e.id, "Auto_level_1");
                }
            },
        ],
        [
            "whycode.taskTree.runAuto2",
            (e: treeElem) => {
                if (activeTree != undefined) {
                    vscode.commands.executeCommand("whycode.run_transformation", activeTree, e.id, "Auto_level_2");
                }
            },
        ],
        [
            "whycode.taskTree.runAuto3",
            (e: treeElem) => {
                if (activeTree != undefined) {
                    vscode.commands.executeCommand("whycode.run_transformation", activeTree, e.id, "Auto_level_3");
                }
            },
        ],
        [
            "whycode.taskTree.splitVC",
            (e: treeElem) => {
                if (activeTree != undefined) {
                    vscode.commands.executeCommand("whycode.run_transformation", activeTree, e.id, "Split_VC");
                }
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
                    uri,
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

                client.sendNotification("proof/replay", {
                    uri: uri,
                });

                vscode.window.showInformationMessage("Session Reloaded");
            },
        ],
        [
            "whycode.run_transformation",
            (uri: DocumentUri, node: number | Range, command: string) => {
                // console.log(uri, JSON.stringify(node), command);
                let target;
                if (typeof node == "number") {
                    target = ["Node", node];
                } else {
                    target = ["Range", { start: node.start, end: node.end }];
                }

                client.sendRequest("proof/runTransformation", {
                    uri,
                    target,
                    command,
                });
            },
        ],
        [
            "whycode.show_task",
            async (uri: DocumentUri, node: number | Range) => {
                let target;
                if (typeof node == "number") {
                    target = ["Node", node];
                } else {
                    target = ["Range", node];
                }
                const u = Uri.parse(uri);
                const uri2 = Uri.parse(`tasks:${u.path}?${JSON.stringify(target)}`);
                // await workspace.openTextDocument(Uri.parse("tasks:omgomg.mlw"));
                return workspace.openTextDocument(uri2).then((doc) => window.showTextDocument(doc));
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

async function startServer(config: Config, context: ExtensionContext): Promise<LanguageClient> {
    const serverPath = config.serverPath();
    const env: Env = config.env();
    const serverArgs = config.serverArgs();

    env.OCAMLRUNPARAM = "b";
    const outputChannel = vscode.window.createOutputChannel("WhyCode Server");
    const traceOutputChannel = vscode.window.createOutputChannel("Whycode Server Trace");

    if (!existsSync(serverPath)) {
        throw "WhyCode server not found";
    }
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
        documentSelector: languages,
        synchronize: {},
    };

    // Create the language client and start the client.
    client = new LanguageClient("whycode", "Why3", serverOptions, clientOptions);

    await client.start();
    return client;
}

function setupServerEvents(config: Config) {
    workspace.onDidSaveTextDocument((event) => {
        if (!config.doAuto()) {
            return;
        }

        const strat = config.autoStrategyFinish();

        if (strat == undefined || strat == "") {
            return;
        }

        if (vscode.languages.match(languages, event)) {
            const diagnostics = vscode.languages.getDiagnostics(event.uri);

            diagnostics.forEach((diag) => {
                vscode.commands.executeCommand("whycode.run_transformation", event.uri.toString(), diag.range, strat);
            });
        }
    });
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

function setupTaskTree(context: ExtensionContext, client: LanguageClient) {
    const treeDataProvider = new TaskDataProvider(new TaskTree());

    const view: vscode.TreeView<TaskNode> = vscode.window.createTreeView("taskTree", {
        treeDataProvider,
    });
    context.subscriptions.push(view);

    client.onNotification(publishTree, (notif) => {
        let tasks = trees.get(notif.uri);
        if (tasks == undefined) {
            tasks = new TaskTree();
            trees.set(notif.uri, tasks);
        }
        tasks.fromElems(notif.elems);
        activeTree = notif.uri;
        treeDataProvider.tree = tasks;
        treeDataProvider.refresh();
    });

    // const focusOnTree = function (id: string) {
    // // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    //     treeDataProvider.tree = trees.get(id)!;
    //     treeDataProvider.refresh();
    // };

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
    try {
        const config = new Config(context);
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        client = await startServer(config, context);

        setupServerEvents(config);
        setupTaskTree(context, client);

        const provider = new TaskProvider(client);

        workspace.registerTextDocumentContentProvider(TaskProvider.scheme, provider);

        buildCommands().forEach(([name, command]) => {
            const disposable = vscode.commands.registerCommand(name, command);
            context.subscriptions.push(disposable);
        });
        vscode.window.showInformationMessage("Whycode loaded");
    } catch (e) {
        let message: string;
        if (typeof e === "string") {
            message = e;
        } else if (e instanceof Error) {
            message = e.message;
        } else {
            message = "Unknown error occured";
        }
        vscode.window.showErrorMessage(message);
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

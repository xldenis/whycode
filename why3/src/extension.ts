// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

abstract class ProofStatus {
	abstract statusIcon(): vscode.ThemeIcon
}

class Unproved implements ProofStatus {
	statusIcon(): vscode.ThemeIcon {
		return new vscode.ThemeIcon('question', new vscode.ThemeColor('list.warningForeground'))
	}
}

abstract class TaskNode {
	constructor(
		readonly id: NodeId,
		readonly parentId: NodeId,
		public name: string,
		readonly detached: boolean,
		readonly children: TaskNode[]
	) { }

	get proved(): boolean {
		return this.children.every((child) => child.proved)
	}
}

type NewFileNode = NewNodeNotif<NodeType.NFile>
class FileNode extends TaskNode {
	static fromNotification(notif: NewFileNode) {
		return new FileNode(notif[1], notif[2], notif[4], notif[5], [])
	}
}

type NewTheoryNode = NewNodeNotif<NodeType.NTheory>
class TheoryNode extends TaskNode {
	static fromNotification(notif: NewTheoryNode) {
		return new TheoryNode(notif[1], notif[2], notif[4], notif[5], [])
	}
}

type NewGoalNode = NewNodeNotif<NodeType.NGoal>
class GoalNode extends TaskNode {
	static fromNotification(notif: NewGoalNode) {
		return new GoalNode(notif[1], notif[2], notif[4], notif[5], [])
	}

	get proved(): boolean {
		return this.children.some((child) => child.proved)
	}
}

type NewProofAttemptNode = NewNodeNotif<NodeType.NProofAttempt>
class ProofAttemptNode extends TaskNode {
	constructor(
		readonly id: NodeId,
		readonly parentId: NodeId,
		readonly name: string,
		readonly detached: boolean,
		public proofStatus?: AttemptStatus
	) { super(id, parentId, name, detached, []) }
	static fromNotification(notif: NewProofAttemptNode) {
		return new ProofAttemptNode(notif[1], notif[2], notif[4], notif[5], undefined)
	}

	get proved(): boolean {
		if (this.proofStatus == undefined) {
			return false;
		}

		if (this.proofStatus.type == "Done") {
			return true;
		}
		return true;
	}
}

type NewTransformationNode = NewNodeNotif<NodeType.NTransformation>
class TransformationNode extends TaskNode {
	static fromNotification(notif: NewTransformationNode) {
		return new TransformationNode(notif[1], notif[2], notif[4], notif[5], [])
	}
}

export class TaskTree {
	readonly roots: TaskNode[] = [];
	private nodeMap: Map<NodeId, TaskNode> = new Map();

	insertChild(parent: NodeId, child: TaskNode) {
		const parentNode = this.nodeMap.get(parent);
		this.nodeMap.set(child.id, child);
		if (parentNode != undefined) {
			parentNode.children.push(child);
		} else {
			this.roots.push(child);
		}
	}

	getChild(id: NodeId): TaskNode | undefined {
		return this.nodeMap.get(id)
	}

}

export class TaskDataProvider implements vscode.TreeDataProvider<TaskNode> {
	constructor(
		private tree: TaskTree,
	) { }


	// Refreshing the tree view
	private _onDidChangeTreeData: vscode.EventEmitter<TaskNode | undefined | void> = new vscode.EventEmitter<any>();
	readonly onDidChangeTreeData: vscode.Event<any> = this._onDidChangeTreeData.event;


	public refresh(): any {
		this._onDidChangeTreeData.fire();
	}

	public getTreeItem(element: TaskNode): vscode.TreeItem {
		let icon = element.proved ?
			new vscode.ThemeIcon('notebook-state-success', new vscode.ThemeColor('notebookStatusSuccessIcon.foreground')) :
			new vscode.ThemeIcon('question', new vscode.ThemeColor('list.warningForeground'))

		let state: vscode.TreeItemCollapsibleState;

		if (element.children.length > 0) {
			if (element.proved) {
				state = vscode.TreeItemCollapsibleState.Collapsed
			} else {
				state = vscode.TreeItemCollapsibleState.Expanded
			}
		} else {
			state = vscode.TreeItemCollapsibleState.None;
		}

		return {
			label: element.name,
			id: element.id.toString(),
			collapsibleState: state,
			iconPath: icon
		}
	}

	getChildren(element?: TaskNode): TaskNode[] {
		if (element == undefined) {
			return this.tree.roots
		} else {
			return element?.children;
		}
	}

	getParent(element: TaskNode): TaskNode {
		return this.tree.getChild(element.parentId)!
	}

}
type Prover = undefined
type ProverResult = undefined

type AttemptStatus =
	| { type: "Undone" }
	| { type: "Scheduled" }
	| { type: "Running" }
	| { type: "Done", result: ProverResult }
	| { type: "Interrupted" }
	| { type: "Detached" }
	// | {type: "InternalFailure" }
	| { type: "Uninstalled", prover: Prover }
	| { type: "UpgradeProver", prover: Prover }
	| { type: "Removed", prover: Prover }


type AttemptStatusMessage =
	| ["Undone"]
	| ["Scheduled"]
	| ["Running"]
	| ["Done", ProverResult]
	| ["Interrupted"]
	| ["Detached"]
	// | ["InternalFailure"]
	| ["Uninstalled", Prover]
	| ["UpgradeProver", Prover]
	| ["Removed", Prover]

function fromAttemptMessage(msg: AttemptStatusMessage): AttemptStatus {
	if (msg[0] == "Done") {
		return { type: msg[0], result: msg[1] }
	} else if (msg[0] == "Uninstalled") {
		return { type: msg[0], prover: msg[1] }
	} else if (msg[0] == "UpgradeProver") {
		return { type: msg[0], prover: msg[1] }
	} else if (msg[0] == "Removed") {
		return { type: msg[0], prover: msg[1] }
	} else {
		return { type: msg[0] }
	}
}

// Messages
const enum NodeType {
	NFile = "NFile",
	NTheory = "NTheory",
	NGoal = "NGoal",
	NProofAttempt = "NProofAttempt",
	NTransformation = "NTransformation"
}
type NodeId = number

type NewNodeNotif<NT> = ["New_node", NodeId, NodeId, [NT], string, boolean];

type NodeChangeNotif = [string, NodeId, UpdateInfo];
type UpdateInfo =
	| ["Proved", boolean]
	| ["Name_change", string]
	| ["Proof_status_change", AttemptStatusMessage, boolean, any[]]

type MessageNotif =
	| ["Information", string]
	| ["Error", string]
	| ["Query_Error",number, string]
	| ["File_Saved", string]
	| ["Task_Monitor", number, number, number]
	;

import * as child_process from 'child_process';
import * as ndjson from 'ndjson';
import { Duplex, PassThrough, Readable, Stream, Writable } from 'stream';
import { Server } from 'http';
import { MultiStepInput } from './multi_step';

class Why3ServerProcess {
	process?: child_process.ChildProcess;

	private outputStream: PassThrough = new PassThrough({ objectMode: true });
	private inputStream: PassThrough = new PassThrough({ objectMode: true });

	public start(sessionPath: string) {
		if (this.process != undefined) {
			throw "Server is already started"
		}

		this.process = child_process.spawn("/Users/xavier/Code/lri/whycode/_build/default/bin/main.exe", [sessionPath]);

		// Do some plumbing!
		this.process.stdout!.pipe(ndjson.parse()).pipe(this.outputStream);
		this.inputStream.pipe(ndjson.stringify()).pipe(this.process.stdin!);
	}

	// Returns an object mode stream of [Notifications]
	public notifications(): Readable {
		return this.outputStream;
	}

	//  Returns an object mode stream which takes [Requests]
	public requests(): Writable {
		return this.inputStream;
	}
}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	var server: Why3ServerProcess = new Why3ServerProcess();

	// TODO: Group this into a Why3Session type.
	var tasks: TaskTree = new TaskTree();

	// Commands we can use
	var commands: string[] = [];

	// Loaded Transformations
	type Transformation = { name: string, description: string }
	var transformations: Transformation[] = [];

	// Loaded Strategies 
	var strategies: { shortcut: string, name: string }[] = [];

	// Loaded Provers
	type ProverInfo = { name: string, shortcut: string, parseableName: string };
	var provers: ProverInfo[] = [];

	// Output Channel
	let out = vscode.window.createOutputChannel("Why3");
	context.subscriptions.push(out);

	// Task Monitor
	let taskMonitor = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 50);
	context.subscriptions.push(taskMonitor);
	taskMonitor.text = "0|0|0";
	taskMonitor.show();

	let auto0 = vscode.commands.registerCommand('taskList.autoLevel0', (task) => {
		server.requests()
			.write(["Command_req", task.id, "Auto_level_0"])
	});
	context.subscriptions.push(auto0);

	let auto1 = vscode.commands.registerCommand('taskList.autoLevel1', (task) => {
		server.requests()
			.write(["Command_req", task.id, "Auto_level_1"])
	});
	context.subscriptions.push(auto1);

	let auto2 = vscode.commands.registerCommand('taskList.autoLevel2', (task) => {
		server.requests()
			.write(["Command_req", task.id, "Auto_level_2"])
	});
	context.subscriptions.push(auto2);

	let splitVC = vscode.commands.registerCommand('taskList.splitVC', (task) => {
		server.requests()
			.write(["Command_req", task.id, "Split_VC"])
	});
	context.subscriptions.push(splitVC);

	let clean = vscode.commands.registerCommand('taskList.clean', (task) => {
		server.requests()
			.write(["Command_req", task.id, "clean"])
	});
	context.subscriptions.push(clean);

	let remove = vscode.commands.registerCommand('taskList.remove', (task) => {
		server.requests()
			.write(["Remove_subtree", task.id])
	});
	context.subscriptions.push(remove);
	
	let trans = vscode.commands.registerCommand('why.runTransform', () => {
		// MultiStepInput.run(input => {
		// 	const pick = await input.showQuickPick({
		// 		"Run Transofrmation...",
		// 		step: 1, 
		// 		totalSteps: 2,
		// 		items: transformations.map((trans) => {return { label: trans.name, description: trans.description }},
		// 		shouldResume: new Promise((_, _) => {})
		// 	});

		// 	return (input : MultiStepInput) => {

		// 	}
		// })
		// ).then((value) => {
		// 	console.log("QUICK PICK", value);
		// })
	});
	context.subscriptions.push(trans);

	// TreeView
	const treeDataProvider = new TaskDataProvider(tasks);
	const view: vscode.TreeView<TaskNode> = vscode.window.createTreeView('why3_tasks', { treeDataProvider });
	context.subscriptions.push(view);

	vscode.commands.registerCommand('why3.launchServer', () => {
		const file = "/Users/xavier/Code/lri/why3-scratch/drop_function_tests.mlw"
		server.start(file);
	
		server.requests()
			.write(["Get_global_infos"]);
	})

	// TODO: Pull this out 	
	server.notifications()
		.on('data', function (obj) {
			console.log(obj);
			// TODO: Extrac this into an actual function
			switch (obj[0]) {
				case "New_node": {
					let msg = obj as NewNodeNotif<keyof typeof NodeType>;

					let newNode: TaskNode;
					if (msg[3][0] == NodeType.NFile) {
						// Apparently TS can't narrow msg down to NewNodeNotif<NFile> after a switch :/ (#13995)
						newNode = FileNode.fromNotification(msg as NewFileNode);
					} else if (msg[3][0] == NodeType.NTheory) {
						newNode = TheoryNode.fromNotification(msg as NewTheoryNode);
					} else if (msg[3][0] == NodeType.NGoal) {
						newNode = GoalNode.fromNotification(msg as NewGoalNode);
					} else if (msg[3][0] == NodeType.NProofAttempt) {
						newNode = ProofAttemptNode.fromNotification(msg as NewProofAttemptNode);
					} else if (msg[3][0] == NodeType.NTransformation) {
						newNode = TransformationNode.fromNotification(msg as NewTransformationNode);
					} else {
						// Why is this needed? shouldn't it already be exhaustive
						throw "Unknown node type"
					}

					let parentId = msg[2];
					tasks.insertChild(parentId, newNode)
					treeDataProvider.refresh();
					break;
				}
				case "Node_change": {
					let msg = obj as NodeChangeNotif;
					const node = tasks.getChild(msg[1]);
					if (node == undefined) {
						console.error(`Node_change: Node ${msg[1]} does not exist in tree.`)
						return;
					}

					// This message doesn't actually determine the proof state of a node... 
					// If a downstream node has a Proof_status_change which is `Done` then even if we had a `Proved false` message it should
					// be marked as proved... 
					// I'm choosing to ignore this message for now... 
					if (msg[2][0] == "Proved") {
						break;
					} else if (msg[2][0] == "Name_change") {
						node.name = msg[2][1];
					} else if (msg[2][0] == "Proof_status_change") {
						if (node instanceof ProofAttemptNode) {
							node.proofStatus = fromAttemptMessage(msg[2][1])
						}
					}
					treeDataProvider.refresh();
					break;
				}
				case "Message": {
					let msg = obj[1] as MessageNotif;
					if (msg[0] == "Information") {
						out.appendLine(`INFO: ${msg[1]}`)
					} else if (msg[0] == "Error") {
						out.appendLine(`ERROR: ${msg[1]}`)
					} else if (msg[0] == "Task_Monitor") {
						taskMonitor.text = `${msg[1]}|${msg[2]}|${msg[3]}`
						taskMonitor.show();
					} else if (msg[0] == "Query_Error") {
						vscode.window.showErrorMessage(msg[2]);
					} else {
						throw "UNHANDLED!"
					}
					break;
				}
				case "Initialized": {
					type InitializationNotif = {
						provers: [string, string, string][],
						transformations: [string, string][],
						strategies: [string, string][],
						commands: string[]
					}
					let msg = obj[1] as InitializationNotif;
					provers = msg.provers.map(elem => { return { shortcut: elem[0], name: elem[1], parseableName: elem[2] } });
					transformations = msg.transformations.map(elem => { return { name: elem[0], description: elem[1] } });
					strategies = msg.strategies.map(elem => { return { shortcut: elem[0], name: elem[1] } });
					commands = msg.commands;
					break;
				}
				default: {
					// unhandled!
				}
			}
		});
}

// this method is called when your extension is deactivated
export function deactivate() { }

import * as vscode from "vscode";

abstract class ProofStatus {
  abstract statusIcon(): vscode.ThemeIcon;
}

class Unproved implements ProofStatus {
    statusIcon(): vscode.ThemeIcon {
        return new vscode.ThemeIcon("question", new vscode.ThemeColor("list.warningForeground"));
    }
}

// Provide access to document URI
export abstract class TaskNode {
  public proved = null;
  constructor(
    readonly uri: vscode.Uri,
    readonly id: NodeId,
    readonly parentId: NodeId,
    public name: string,
    readonly detached: boolean,
    readonly children: TaskNode[]
  ) {}
}

type NewFileNode = NewNodeNotif<NodeType.NFile>;
class FileNode extends TaskNode {}

type NewTheoryNode = NewNodeNotif<NodeType.NTheory>;
class TheoryNode extends TaskNode {}

type NewGoalNode = NewNodeNotif<NodeType.NGoal>;
export class GoalNode extends TaskNode {}

type NewProofAttemptNode = NewNodeNotif<NodeType.NProofAttempt>;
class ProofAttemptNode extends TaskNode {
    constructor(
    readonly uri: vscode.Uri,
    readonly id: NodeId,
    readonly parentId: NodeId,
    readonly name: string,
    readonly detached: boolean,
    public proofStatus?: AttemptStatus
    ) {
        super(uri, id, parentId, name, detached, []);
    }

    // get proved(): boolean {
    //   if (this.proofStatus == undefined) {
    //     return false;
    //   }

    //   if (this.proofStatus.type == "Done") {
    //     return true;
    //   }
    //   return true;
    // }
}

type NewTransformationNode = NewNodeNotif<NodeType.NTransformation>;
class TransformationNode extends TaskNode {}

export class TaskTree {
  public roots: TaskNode[] = [];
  private nodeMap: Map<NodeId, TaskNode> = new Map();

  insertChild(parent: NodeId, child: TaskNode) {
      const parentNode = this.nodeMap.get(parent);
      this.nodeMap.set(child.id, child);
      if (parent != 0) {
          const child_ix = parentNode!.children.findIndex((n) => n.id == child.id);
          if (child_ix == -1) {
        parentNode!.children.push(child);
          } else {
        // should probably just error here on the long term.
        parentNode!.children[child_ix] = child; // handle children of the old child
          }
      } else {
          const old_ix = this.roots.findIndex((n) => n.id == child.id);
          if (old_ix == -1) {
              this.roots.push(child);
          } else {
              this.roots[old_ix] = child;
          }
      }
  }

  remove(id: NodeId) {
      if (id == 0) {
          this.nodeMap.clear();
          this.roots = [];
          return;
      }
      const node = this.nodeMap.get(id);
      const parent = this.nodeMap.get(node!.parentId)!;

      const parent_ix = parent.children.findIndex((n) => n == n);
      parent.children.splice(parent_ix, 1);

      for (const child of node?.children || []) {
          this.remove(child.id);
      }

      this.nodeMap.delete(id);
  }

  getChild(id: NodeId): TaskNode | undefined {
      return this.nodeMap.get(id);
  }
}

export class TaskDataProvider implements vscode.TreeDataProvider<TaskNode> {
    constructor(public tree: TaskTree) {}

  // Refreshing the tree view
  private _onDidChangeTreeData: vscode.EventEmitter<TaskNode | undefined | void> = new vscode.EventEmitter<any>();
  readonly onDidChangeTreeData: vscode.Event<any> = this._onDidChangeTreeData.event;

  public refresh(): any {
      this._onDidChangeTreeData.fire();
  }

  public getTreeItem(element: TaskNode): vscode.TreeItem {
      let icon = new vscode.ThemeIcon("question", new vscode.ThemeColor("notebookStatusRunningIcon.foreground"));

      if (element.proved == true) {
          icon = new vscode.ThemeIcon(
              "notebook-state-success",
              new vscode.ThemeColor("notebookStatusSuccessIcon.foreground")
          );
      } else if (element.proved == false) {
          icon = new vscode.ThemeIcon("notebook-state-error", new vscode.ThemeColor("notebookStatusErrorIcon.foreground"));
      }

      let state: vscode.TreeItemCollapsibleState;

      if (element.children.length > 0) {
          if (element.proved) {
              state = vscode.TreeItemCollapsibleState.Collapsed;
          } else {
              state = vscode.TreeItemCollapsibleState.Expanded;
          }
      } else {
          state = vscode.TreeItemCollapsibleState.None;
      }

      return {
          label: element.name,
          id: element.id.toString(),
          collapsibleState: state,
          iconPath: icon,
      };
  }

  getChildren(element?: TaskNode): TaskNode[] {
      if (element == undefined) {
          return this.tree.roots;
      } else {
          return Array.from(element?.children);
      }
  }

  getParent(element: TaskNode): TaskNode {
      return this.tree.getChild(element.parentId)!;
  }
}
type Prover = undefined;
type ProverResult = undefined;

type AttemptStatus =
  | { type: "Undone" }
  | { type: "Scheduled" }
  | { type: "Running" }
  | { type: "Done"; result: ProverResult }
  | { type: "Interrupted" }
  | { type: "Detached" }
  // | {type: "InternalFailure" }
  | { type: "Uninstalled"; prover: Prover }
  | { type: "UpgradeProver"; prover: Prover }
  | { type: "Removed"; prover: Prover };

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
  | ["Removed", Prover];

function fromAttemptMessage(msg: AttemptStatusMessage): AttemptStatus {
    if (msg[0] == "Done") {
        return { type: msg[0], result: msg[1] };
    } else if (msg[0] == "Uninstalled") {
        return { type: msg[0], prover: msg[1] };
    } else if (msg[0] == "UpgradeProver") {
        return { type: msg[0], prover: msg[1] };
    } else if (msg[0] == "Removed") {
        return { type: msg[0], prover: msg[1] };
    } else {
        return { type: msg[0] };
    }
}

// Messages
const enum NodeType {
  NFile = "NFile",
  NTheory = "NTheory",
  NGoal = "NGoal",
  NProofAttempt = "NProofAttempt",
  NTransformation = "NTransformation",
}
type NodeId = number;

type NewNodeNotif<NT> = ["New_node", NodeId, NodeId, [NT], string, boolean];

type NodeChangeNotif = [string, NodeId, UpdateInfo];
type UpdateInfo =
  | ["Proved", boolean]
  | ["Name_change", string]
  | ["Proof_status_change", AttemptStatusMessage, boolean, any[]];

type MessageNotif =
  | ["Information", string]
  | ["Error", string]
  | ["Query_Error", number, string]
  | ["File_Saved", string]
  | ["Task_Monitor", number, number, number];

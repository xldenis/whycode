/* eslint-disable @typescript-eslint/no-non-null-assertion */
/* eslint-disable @typescript-eslint/no-unused-vars */
import * as vscode from "vscode";
import { integer } from "vscode-languageclient";

type Id = integer;

export type treeElem = { id: integer; proved: boolean; expl: string; parent: integer | null };
export type TaskNode = treeElem;

export class TaskTree {
  parent: Map<Id, Id> = new Map();
  children: Map<Id, Set<Id>> = new Map();
  data: Map<Id, TaskNode> = new Map();
  roots: Set<Id> = new Set();

  fromElems(elems: treeElem[]) {
      this.parent = new Map();
      this.children = new Map();
      this.data = new Map();

      elems.forEach((elem) => {
          this.data.set(elem.id, elem);
          if (elem.parent != undefined) {
              this.parent.set(elem.id, elem.parent);
              let children = this.children.get(elem.parent);

              if (children == undefined) {
                  children = new Set();
                  this.children.set(elem.parent, children);
              }

              children.add(elem.id);
          } else {
              this.roots.add(elem.id);
          }
      });
  }

  getChild(id: Id): TaskNode | undefined {
      return this.data.get(id);
  }

    // getChild(id: Id) :
}

export class TaskDataProvider implements vscode.TreeDataProvider<TaskNode> {
    constructor(public tree: TaskTree) {}

  // Refreshing the tree view
  private _onDidChangeTreeData: vscode.EventEmitter<TaskNode | undefined | void> = new vscode.EventEmitter<
    void | TaskNode | undefined
  >();
  readonly onDidChangeTreeData: vscode.Event<void | TaskNode | undefined> = this._onDidChangeTreeData.event;

  public refresh() {
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

      if ((this.tree.children.get(element.id) || new Set()).size > 0) {
          if (element.proved) {
              state = vscode.TreeItemCollapsibleState.Collapsed;
          } else {
              state = vscode.TreeItemCollapsibleState.Expanded;
          }
      } else {
          state = vscode.TreeItemCollapsibleState.None;
      }

      return {
          label: element.expl,
          id: element.id.toString(),
          collapsibleState: state,
          iconPath: icon,
      };
  }

  getChildren(element?: TaskNode): TaskNode[] {
      if (element == undefined) {
          return Array.from(this.tree.roots).map((e) => this.tree.data.get(e)!);
      } else {
          return Array.from(this.tree.children.get(element.id)!).map((e) => this.tree.data.get(e)!);
      }
  }

  getParent(element: TaskNode): TaskNode | undefined {
      if (element.parent == undefined) {
          return undefined;
      } else {
          return this.tree.getChild(element.parent);
      }
  }
}

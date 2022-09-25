# Notes and ideas for Why3 LSP server

The LSP model poses some issues for Why3, in particular, *one* instance of an a LSP server is created per workspace / instance of VSCode, in comparison Why3 creates one server *per session*.
Since Why3 projects will often involve multiple sessions, it is necessary that a Why3 LSP can handle multiple sessions simultaneously.
This involves jumping through some hoops, which this document attempts to lay out as well as potential solutions.

## Problem: Identifying a session

The fist problem is quite basic, a Why3 session can contain multiple files, so we must map the user file into a session. It seems like Why3 provides apis that do this. If the user stored their session in a non-standard location, then tough luck it won't work. A possible solution would be to have some comment header which tells the server where to look.

## Problem: Loading multiple Why3 servers at once

This can be solved using first class modules. It is just a matter of wiring up the inputs and outputs to a channel that can be controlled from outside the module.

## Problem: Communicating the task tree with VSCode

The task tree must be synchonized with the editor, and must be able to either switch between trees with the active file or contain multiple independent trees simultaneously.
Furthermore, there is a classic synchronization issue where we must ensure that the server and client don't fall out of synchronization.

## Problem: Mapping user actions to why3

Through Why3's support for spans, a session can provide proof objectives in source code totally unrelated to Why3 (ie: Rust). This is very cool for the user since they can see the proof objectives on files they understand and care about. However, if the user asks to solve an objective, the Why3 server needs to identify the session responsible for placing that span in the first place.

The solution could be that each server instance tracks the files it has emitted diagnostics for, and when a user action request comes in, we can filter the sessions which have provided some action for this.

# Notes on VSCode LSP client for Why3

The LSP client in VSCode is *mostly* standard, but must be extended with proof-specific commands and UI.
To start, the objective is to recreate the Why3 UI more-or-less faithfully, perhaps afterwards it will be possible to experiment what better UI is available given the VSCode tooling.

# Problem: Running transformations

In Why3, there is always a node 'focused' in the tree such that when a transformation is run, it will be applied to that node. It is unclear whether we can recreate this using the TreeView apis. Can we always have a 'focused' node?

# Problem: switching between sessions

Need to identify if we can easily swap out a Task Tree when the user switches tabs. This would require knowing when we are changing sessions.

# Problem: Task view

A key feature of Why3 is the task view which shows a traditional proof context.
We should be able to create a 'virtual file' in VSCode and display it for the user.

A future version could even probably allow the user to click on hypotheses / functions to apply transformations directly to them!


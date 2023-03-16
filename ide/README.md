# WhyCode

This extension adds support for [Why3](https://why3.lri.fr) proofs to VSCode, enabling users to do program verification directly in their code.
WhyCode uses an LSP server to provide Code Actions that perform verification strategies. **Note: The LSP server must currently be installed manually**
Usage will also require at least one backend to Why3 sucg as Z3, CVC5 or Alt-Ergo.

**This is pre-release software, please [report all bugs](https://github.com/xldenis/whycode)**

## Installing the server

0. Clone the repository at https://github.com/xldenis/whycode
1. Create a local switch and install the server into it: `ocaml switch create . --locked`
2. Ensure the resulting `whycode` binary is available on your `$PATH`, or set `Whycode: Executable Path` to the appropriate path

## Features

1. Load, reset and save Why3 proof sessions
2. Run Why3 strategies on goals, perform basic proof tasks
3. Show unproved goals in source code
4. View and perform Rust proofs using [Creusot](https://github.com/xldenis/creusot)

## Upcoming

1. View the Why3 task tree
2. Run individual _transformations_
3. View proof tasks
4. Display counter-examples inline
5. Configure Why3 backends from VSCode
6. Syntax highlighting

# WhyCode: A VsCode front-end for Why3

This repository contains an experiment in using VsCode as a front-end for the [Why3](http://why3.lri.fr) prover framework, rather than its hand-built GTK frontend.

# Building locally

## Building the Server

- Create a local switch and install the package: `opam switch create --locked .`
- This will build and install `whycode` into a local opam switch. 

## Building the extension

1. Turn on VSCode developer mode
2. Install JS dependencies by running `npm install`
3. Build and launch the extension using the Debug Menu in VSCode, "Launch Client"
4. Report all the (many) bugs
5. Do some proofs?

# Using the VSCode Extension

- Install the extension through the vscode marketplace.
- Install the server binary into your local switch or globally
- Launch VSCode and run the command `Start Why3`

# Proving Rust code with Creusot

- Setup the server and client like shown above
- Go to your VSCode settings
- In `Whycode: Extra Args` add `-L/path/to/creusot/prelude/`

# FAQ

- VSCode can't find the server binary!
  - Make sure you have the right `opam` switch loaded, or configure the extension to use an absolute path by changing `Whycode: Executable Path`.
- I can't see the task tree!
  - Unfortunately that is not yet supproted
- Something is seems like a bug!
  - It probably is, so don't hesitate to open an issue.

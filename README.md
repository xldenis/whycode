# WhyCode: A VsCode front-end for Why3

This repository contains an experiment in using VsCode as a front-end for the [Why3](http://why3.lri.fr) prover framework, rather than its hand-built GTK frontend.

# Building locally

## Building the Server

To build the server, I strongly recommend using a local `opam` switch which can be created using `opam switch create .`.
This should actually install all the required dependencies while it's at it. You can then build the project using `dune build`.

## Building the extension

1. Turn on VSCode developer mode
  How? I forgot how this is done
2. Install JS dependencies by running `npm install`
3. Build and launch the extension using the Debug Menu in VSCode, "Launch Client"
4. Report all the (many) bugs
5. Do some proofs?
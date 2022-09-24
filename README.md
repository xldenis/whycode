# WhyCode: A VsCode front-end for Why3

This repository contains an experiment in using VsCode as a front-end for the [Why3](http://why3.lri.fr) prover framework, rather than its hand-built GTK frontend.

# Building locally

## Building the Server

- Create a local opam switch: `opam switch create . 4.14.0`. Do not install the `whycode` package, it *will* fail.
- Pin `linol` and `linol-lwt`:
  ```
   opam pin linol git@github.com:c-cube/linol.git
   opam pin linol-lwt git@github.com:c-cube/linol.git
  ```
- Install the `whycode` package: `opam install .`
- Build the server: `dune build`


## Building the extension

1. Turn on VSCode developer mode
  How? I forgot how this is done
2. Install JS dependencies by running `npm install`
3. Build and launch the extension using the Debug Menu in VSCode, "Launch Client"
4. Report all the (many) bugs
5. Do some proofs?
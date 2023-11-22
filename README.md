# WhyCode: A VsCode front-end for Why3

This repository contains an experiment in using VsCode as a front-end for the [Why3](http://why3.lri.fr) prover framework, rather than its hand-built GTK frontend.

# Using the VSCode Extension

- Launch VSCode and run the command `Start Why3`

# Proving Rust code with Creusot

- Setup the server and client like shown above
- Go to your VSCode settings
- In `Whycode: Extra Args` add `-L/path/to/creusot/prelude/`

# FAQ

- I can't see the task tree!
  - Unfortunately that is not yet supproted
- Something seems like a bug!
  - It probably is, so don't hesitate to [open an issue](https://github.com/xldenis/whycode).

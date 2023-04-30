# Neovim extension for Why3

## Prerequisites

- [Latest stable version of Neovim](https://github.com/neovim/neovim/releases/tag/stable)
- The lsp neovim package: *neovim/nvim-lspconfig* (see [here](https://github.com/neovim/nvim-lspconfig))
- WhyCode binary: download the last artifact for you distribution from the
  [releases list](https://github.com/xldenis/whycode/actions)

## Installation
- packer
  ```lua
  use { "paulpatault/whycode" }
  ```

- lazy
  ```lua
  return { "paulpatault/whycode" }
  ```

## Configuration

Add the following to your LSP configuration:

```lua
local opts = {
    cmd = { "path/to/artifact/binary" },      -- path to the executable from the artifact
    lsp = require("lspconfig"),               -- idem
    lspconfig = require("lspconfig.configs"), -- from lspconfig package
    on_attach = ...
}
require("whycode").setup(opts)
```
## TODO

- [ ] documentation
- [ ] tasks view
- [ ] tree view

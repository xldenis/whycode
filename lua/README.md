# Neovim extension for Why3



## Prerequisites

- Neovim
- Neovim lsp extension: "neovim/nvim-lspconfig"

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

Add to your lsp configuration

```lua
local opts = {
    cmd = { "$DEV/whycode/extension/whycode" }, --path to executable
    lspconfig = require("lspconfig.configs"),
    lsp = require("lspconfig")
}
require("whycode").setup(opts)
```

## TODO

- [ ] documentation
- [ ] tasks view
- [ ] tree view

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
require"whycode".setup{
  lsp = {
    on_attach = on_attach,
    cmd = { "absolute/path/to/artifact/binary" },      -- path to the executable from the artifact
  }
}
```
where `on_attach` can be define as:
```lua
local function map(mod, key, f)
  vim.keymap.set(mod, key, f, { buffer = bufnr, remap = false })
end

local function on_attach(client, bufnr)
  map("n", "<leader>ca", function() vim.lsp.buf.code_action()  end)
  map("n", "<leader>rn", function() vim.lsp.buf.rename()       end)
  map("n", "gd",         function() vim.lsp.buf.definition()   end)
  map("n", "dc",         function() vim.lsp.buf.hover()        end)
  map("n", "dn",         function() vim.diagnostic.goto_next() end)
  map("n", "dN",         function() vim.diagnostic.goto_prev() end)
end
```

for example.

## State of the art

![alt: Screenshot of WIP](./assets/WIP.png "Screenshot")

## TODO

- [ ] documentation
- [ ] tasks view : WIP
- [ ] goal view

## Acknowledgment

Some ideas are taken from [coq lsp for neovim](https://github.com/tomtomjhj/coq-lsp.nvim).

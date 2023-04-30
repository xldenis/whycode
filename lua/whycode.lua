local whycode = {}

function whycode.setup(opts)

  local configs = opts.lspconfig 
  local lsp = opts.lsp
  local cmd = opts.cmd
  local on_attach = opts.on_attach

  if not configs["why3"] then
    configs["why3"] = {
      default_config = {
        autostart = true,
        cmd = cmd,
        filetypes = { "why3" },
        root_dir = function(fname)
          return lsp.util.find_git_ancestor(fname) or vim.loop.os_homedir()
        end;
        settings = {},
      },
    }
  end

  if not lsp["why3"].document_config.default_config.cmd and not cmd then
    print("You have not defined a server default cmd for a server that requires it please follow README instructions")
  end

  lsp["why3"].setup({
    cmd = cmd,
    on_attach = on_attach,
    filetypes = { "why3" },
  })

end

return whycode

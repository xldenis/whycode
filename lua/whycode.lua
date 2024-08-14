local whycode = {}

local the_client
local buf = require("whycode.buffers")

function whycode.stop()
  assert(the_client)
  the_client.stop(true)
  the_client = nil
  for bufnr, _ in pairs(buf.buffers) do
    buf.unregister(bufnr)
  end
  vim.api.nvim_clear_autocmds({ group = buf.au })
end

local function make_on_attach(user_on_attach, verb)
  return function(client, bufnr)
    if not the_client then
      the_client = client
    elseif the_client ~= client then
      error("why3 client must be unique")
    end
    if not buf.buffers[bufnr] then
      buf.register(bufnr)
      require("whycode.handlers").define_handlers(verb)
    end
    if user_on_attach then
      user_on_attach(client, bufnr)
    end
  end
end

function whycode.setup(opts)

  opts = opts or {}
  opts.lsp = opts.lsp or {}
  opts.lsp.cmd = opts.lsp.cmd or function() error("erreur cmd lsp") end
  opts.lsp.verbose = opts.lsp.verbose or false
  opts.lsp.capabilities = opts.lsp.capabilities or {}

  local lsp = require("lspconfig")
  local util = require("lspconfig.util")
  local configs = require("lspconfig.configs")

  local user_on_attach = opts.lsp.on_attach
  opts.lsp.on_attach = make_on_attach(user_on_attach, opts.lsp.verbose)

  if not configs["why3"] then
    configs["why3"] = {
      default_config = {
        autostart = true,
        cmd = opts.lsp.cmd,
        filetypes = { "why3" },
        root_dir = function(fname)
          return util.find_git_ancestor(fname)
        end;
        settings = {},
      },
    }
  end

  if not lsp["why3"].document_config.default_config.cmd then
    print("You have not defined a server default cmd for a server that requires it please follow README instructions")
  end

  lsp["why3"].setup({
    on_attach = opts.lsp.on_attach,
    filetypes = { "why3" },
    capabilities = opts.lsp.capabilities,
  })

  require("whycode.hi").init_highlights()
  require("whycode.hi").run_autocommands()

end

return whycode

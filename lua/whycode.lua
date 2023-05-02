local whycode = {}

--[[ local function request_async(client, bufnr, method, params, handler)
  local request_success, request_id = client.request(method, params, handler, bufnr)
  if request_success then
    return function()
      client.cancel_request(assert(request_id))
    end
  end
end ]]

local the_client

local buffers = {}

local whycode_ns = vim.api.nvim_create_namespace("whycode")

local function create_info_panel(bufnr, ft)
  ft = ft or "whyinfo"
  local info_bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_option(info_bufnr, "filetype", ft)
  buffers[bufnr].info_bufnr = info_bufnr
end

local function get_info_bufnr(bufnr)
  local info_bufnr = buffers[bufnr].info_bufnr
  if info_bufnr and vim.api.nvim_buf_is_valid(info_bufnr) then
    return info_bufnr
  end
  create_info_panel(bufnr)
  return buffers[bufnr].info_bufnr
end

local function open_info_panel(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local win = vim.api.nvim_get_current_win()
  local info_bufnr = get_info_bufnr(bufnr)

  vim.cmd.sbuffer {
    args = { info_bufnr },
    mods = { keepjumps = true, keepalt = true, vertical = true, split = "belowright"},
  }

  vim.cmd.clearjumps()

  -- todo: add more defaults (sidebar.nvim/lua/sidebar-nvim/view.lua)
  vim.api.nvim_buf_set_option(info_bufnr, "modifiable", false)
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false

  vim.api.nvim_set_current_win(win)
end

local function write_info_panel(bufnr, str)
    vim.api.nvim_buf_set_option(bufnr, "modifiable", true)
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, str)
    vim.api.nvim_buf_set_option(bufnr, "modifiable", false)
end

local function parse_nl(msg)
  local lines = {}
  local hi, lo = 0, 0
  while true do
    hi = string.find(msg, "\n", hi+1)
    if hi == nil then
      table.insert(lines, string.sub(msg, lo, string.len(msg)-1))
      break
    end
    table.insert(lines, string.sub(msg, lo, hi-1))
    lo = hi+1
  end
  if lo > 0 then
    return true, lines
  else
    return false, msg
  end
end

--[[ local function red(msg)
  local t = {}
  for k, v in pairs(msg) do
    table.insert(t, k, "\033[1;31m" .. v .. "\033[0m")
  end
  return t
end ]]

local icons = { why = "(?)", nok = "(⨯)", ok  = "(✓)", info = "(i)" }

local function render_diagnostics(diagnostics)
  local lines = {}

  lines[#lines+1] = ""
  lines[#lines+1] = " Verification Conditions"

  for _,v in pairs(diagnostics["diagnostics"]) do

    local err, msg = parse_nl(v["message"])

    if err then return msg end

    local start_line = v["range"]["start"]["line"] + 1
    local start_char = v["range"]["start"]["character"] + 1
    local end_line = v["range"]["end"]["line"] + 1
    local end_char = v["range"]["end"]["character"] + 1
    lines[#lines+1] = ""
    lines[#lines+1] = string.format(" %s goal: %s", icons.why, msg)
    lines[#lines+1] = string.format("        [from %d:%d to %d:%s]",
                                    start_line, start_char, end_line, end_char)
  end

  return lines
end

local function define_handlers()
                 -- textDocument/diagnostic ?
  vim.lsp.handlers["textDocument/publishDiagnostics"] = function(err, result, ctx, cconfig)
    -- local uri = result.uri
    -- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage

    print(vim.inspect(result))

    local content = render_diagnostics(result)

    local bufnr = vim.api.nvim_get_current_buf()
    local info_bufnr = get_info_bufnr(bufnr)

    write_info_panel(info_bufnr, content)

    return require("vim.lsp.diagnostic").on_publish_diagnostics(err, result, ctx, cconfig)
  end

end

local au = vim.api.nvim_create_augroup("whycode-lsp", { clear = true })

local function unregister(bufnr)
  assert(buffers[bufnr])
  vim.api.nvim_buf_clear_namespace(bufnr, whycode_ns, 0, -1)
  vim.api.nvim_clear_autocmds { group = au, buffer = bufnr }
  buffers[bufnr].debounce_timer:stop()
  buffers[bufnr].debounce_timer:close()
  if buffers[bufnr].info_bufnr then
    vim.api.nvim_buf_delete(buffers[bufnr].info_bufnr, { force = true })
  end
  buffers[bufnr] = nil
end

local function register(bufnr)
  assert(buffers[bufnr] == nil)
  buffers[bufnr] = {}
  create_info_panel(bufnr)
  open_info_panel(bufnr)
  buffers[bufnr].debounce_timer = assert(vim.loop.new_timer(), "Could not create timer")
  vim.api.nvim_create_autocmd({"BufDelete", "LspDetach"}, {
    group = au,
    buffer = bufnr,
    desc = "Unregister deleted/detached buffer",
    callback = function(ev) unregister(ev.buf) end,
  })
  -- goals_async(bufnr)
end

function whycode.stop()
  assert(the_client)
  the_client.stop(true)
  the_client = nil
  for bufnr, _ in pairs(buffers) do
    unregister(bufnr)
  end
  vim.api.nvim_clear_autocmds({ group = au })
end

local function make_on_attach(user_on_attach)
  return function(client, bufnr)
    if not the_client then
      the_client = client
    elseif the_client ~= client then
      error("why3 client must be unique")
    end
    if not buffers[bufnr] then
      register(bufnr)
      define_handlers()
    end
    if user_on_attach then
      user_on_attach(client, bufnr)
    end
  end
end

function whycode.setup(opts)

  -- if vim.bo.filetype ~= "why3" then return end

  opts = opts or {}
  opts.lsp = opts.lsp or {}
  opts.lsp.cmd = opts.lsp.cmd or function() error("erreur cmd lsp") end

  local lsp = require("lspconfig")
  local util = require("lspconfig.util")
  local configs = require("lspconfig.configs")

  local user_on_attach = opts.lsp.on_attach
  opts.lsp.on_attach = make_on_attach(user_on_attach)

  if not configs["why3"] then
    configs["why3"] = {
      default_config = {
        autostart = true,
        cmd = opts.lsp.cmd,
        filetypes = { "why3" },
        root_dir = function(fname)
          return util.find_git_ancestor(fname) or vim.loop.os_homedir()
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
  })

end

return whycode

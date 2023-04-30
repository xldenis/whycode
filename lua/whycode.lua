local whycode = {}

local function position_lsp_to_api(bufnr, position, offset_encoding)
  local idx = vim.lsp.util._get_line_byte_from_position(
    bufnr,
    { line = position.line, character = position.character },
    offset_encoding
  )
  return { position.line, idx }
end

local function make_position_params(bufnr, position, offset_encoding)
  local row, col = unpack(position)
  row = row - 1
  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, true)[1]
  if not line then
    return { line = 0, character = 0 }
  end

  col = vim.lsp.util._str_utfindex_enc(line, col, offset_encoding)

  return { line = row, character = col }
end

local function guess_position(bufnr)
  local win = vim.api.nvim_get_current_win()
  if vim.api.nvim_win_get_buf(win) ~= bufnr then
    error("can't guess position")
  end
  return vim.api.nvim_win_get_cursor(win)
end

local function request_async(client, bufnr, method, params, handler)
  local request_success, request_id = client.request(method, params, handler, bufnr)
  if request_success then
    return function()
      client.cancel_request(assert(request_id))
    end
  end
end

local the_client

local buffers = {}

local config = {
  show_goals_on = "cursor",
  goals_debounce = 150,
}

local progress_ns = vim.api.nvim_create_namespace("whycode")

---@param bufnr buffer
local function create_info_panel(bufnr, ft)
  ft = ft or "why"
  local info_bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_option(info_bufnr, "filetype", ft)
  buffers[bufnr].info_bufnr = info_bufnr
end

---@param bufnr buffer
local function get_info_bufnr(bufnr)
  local info_bufnr = buffers[bufnr].info_bufnr
  if info_bufnr and vim.api.nvim_buf_is_valid(info_bufnr) then
    return info_bufnr
  end
  create_info_panel(bufnr)
  return buffers[bufnr].info_bufnr
end

---@param bufnr? buffer
local function open_info_panel(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local win = vim.api.nvim_get_current_win()
  vim.cmd.sbuffer {
    args = { get_info_bufnr(bufnr) },
    mods = { keepjumps = true, keepalt = true, vertical = true, split = "belowright"},
  }
  vim.cmd.clearjumps()
  vim.api.nvim_set_current_win(win)
end

---@param i integer
---@param n integer
---@param goal Goal
---@return string
local function render_goal(i, n, goal)
  local lines = {}
  lines[#lines+1] = 'Goal ' .. i .. ' / ' .. n
  for _, hyp in ipairs(goal.hyps) do
    local line = table.concat(hyp.names, ', ') .. ' : ' .. hyp.ty
    if hyp.def then
      line = line .. ' := ' .. hyp.def
    end
    lines[#lines+1] = line
  end
  lines[#lines+1] = ''
  lines[#lines+1] = '========================================'
  lines[#lines+1] = ''
  lines[#lines+1] = goal.ty
  return table.concat(lines, '\n')
end

---@param answer GoalAnswer
---@param position MarkPosition Don't use answer.position because buffer content may have changed.
local function show_goals(answer, position)
  local bufnr = vim.uri_to_bufnr(answer.textDocument.uri)
  print("show G " .. vim.inspect(answer.textDocument.uri))
  local goal_config = answer.goals or {}
  local goals = goal_config.goals or {}
  local rendered = {}
  for i, goal in ipairs(goals) do
    rendered[#rendered+1] = render_goal(i, #goals, goal)
  end
  local lines = {}
  lines[#lines+1] = vim.fn.bufname(bufnr) .. ':' .. position[1] .. ':' .. (position[2] + 1)
  -- NOTE: each Pp can contain newline, which isn't allowed by nvim_buf_set_lines
  vim.list_extend(lines, vim.split(table.concat(rendered, '\n\n\n────────────────────────────────────────────────────────────\n'), '\n'))
  vim.api.nvim_buf_set_lines(get_info_bufnr(bufnr), 0, -1, false, lines)
end

local function goals_async(bufnr, position)
  assert(the_client)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  position = position or guess_position(bufnr)
  local cancel_old = buffers[bufnr].cancel_goals
  if cancel_old then
    buffers[bufnr].cancel_goals = nil
    cancel_old()
  end
  local params = {
    textDocument = vim.lsp.util.make_text_document_params(bufnr),
    position = make_position_params(bufnr, position, the_client.offset_encoding)
  }
  local cancel = request_async(the_client, bufnr, "whycode.show_task", params, function(err, result)
    --[[ local pres = ":res " .. vim.inspect(result)
    local perr = ":err " .. vim.inspect(err)
    local ppos = ":err " .. vim.inspect(position)
    print(pres .. " ".. perr .. " " .. ppos) ]]
    buffers[bufnr].cancel_goals = nil
    if err then return end
    show_goals(result, position)
  end)
  buffers[bufnr].cancel_goals = cancel
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = function(err, result, ctx, config)

  -- local uri = result.uri
  -- check
  -- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
  local str = "coucou" -- { vim.inspect(result) }
  local curr_buf = vim.api.nvim_get_current_buf()
  local info_bufnr = get_info_bufnr(curr_buf)

  vim.api.nvim_buf_set_lines(info_bufnr, 0, -1, false, str)

  return require("vim.lsp.diagnostic").on_publish_diagnostics(err, result, ctx, config)
end

---@param bufnr? buffer
---@param position? MarkPosition
local function goals_sync(bufnr, position)
  print("goals_sync")
  assert(the_client)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  position = position or guess_position(bufnr)
  local params = {
    textDocument = vim.lsp.util.make_text_document_params(bufnr),
    position = make_position_params(bufnr, position, the_client.offset_encoding)
  }
  local request_result, err = the_client.request_sync("whycode.show_task", params, 500, bufnr)
  if err then
    vim.notify('goals_sync() failed: ' .. err, vim.log.levels.ERROR)
    return
  end
  assert(request_result)
  if request_result.err then return end
  show_goals(request_result.result, position)
end

---@param bufnr? buffer
local function get_document(bufnr)
  assert(the_client)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local params = {
    textDocument = vim.lsp.util.make_text_document_params(bufnr),
  }
  local request_result, err = the_client.request_sync("whycode.show_task", params, 500, bufnr)
  if err then
    vim.notify('get_document() failed: ' .. err, vim.log.levels.ERROR)
    return
  end
  assert(request_result)
  if request_result.err then return end
  return request_result.result
end

local ag = vim.api.nvim_create_augroup("whycode-lsp", { clear = true })

---@param bufnr buffer
local function unregister(bufnr)
  assert(buffers[bufnr])
  vim.api.nvim_buf_clear_namespace(bufnr, progress_ns, 0, -1)
  vim.api.nvim_clear_autocmds { group = ag, buffer = bufnr }
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
    group = ag,
    buffer = bufnr,
    desc = "Unregister deleted/detached buffer",
    callback = function(ev) unregister(ev.buf) end,
  })
  goals_async(bufnr)
end

local function stop()
  assert(the_client)
  the_client.stop(true)
  the_client = nil
  for bufnr, _ in pairs(buffers) do
    unregister(bufnr)
  end
  vim.api.nvim_clear_autocmds({ group = ag })
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
    end
    if user_on_attach then
      user_on_attach(client, bufnr)
    end
  end
end

function whycode.setup(opts)

  opts = opts or {}
  opts.lsp = opts.lsp or require("lspconfig")
  opts.lspconfigs = opts.lspconfigs or require("lspconfig.configs")

  local user_on_attach = opts.on_attach
  opts.on_attach = make_on_attach(user_on_attach)

  if not opts.lspconfigs["why3"] then
    opts.lspconfigs["why3"] = {
      default_config = {
        autostart = true,
        cmd = opts.cmd,
        filetypes = { "why3" },
        root_dir = function(fname)
          return opts.lsp.util.find_git_ancestor(fname) or vim.loop.os_homedir()
        end;
        settings = {},
      },
    }
  end

  if not opts.lsp["why3"].document_config.default_config.cmd and not opts.cmd then
    print("You have not defined a server default cmd for a server that requires it please follow README instructions")
  end

  opts.lsp["why3"].setup({ on_attach = opts.on_attach })

end

return whycode

local M = {}

function M.create_info_panel(bufnr, ft)
  ft = ft or "whyinfo"
  local info_bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_option_value("filetype", ft, {buf=info_bufnr})
  require("whycode.buffers").buffers[bufnr].info_bufnr = info_bufnr
end

function M.run_autocommands()
  --[[ print("WIP doest not work")
  vim.api.nvim_create_autocmd({"CursorMoved"}, {
    group = vim.api.nvim_create_augroup("Why", {clear = true}),
    pattern = { "whyinfo" },
    callback = function() print("coucou") end
  }) ]]
  vim.api.nvim_create_autocmd({"BufEnter", "BufWinEnter"}, {
    pattern = { "*.whyinfo" },
    command = "echo 'Entering a C or C++ file'",
  })

end


function M.get_info_bufnr(bufnr)
  local info_bufnr = require("whycode.buffers").buffers[bufnr].info_bufnr
  if info_bufnr and vim.api.nvim_buf_is_valid(info_bufnr) then
    return info_bufnr
  end
  M.create_info_panel(bufnr)
  return require("whycode.buffers").buffers[bufnr].info_bufnr
end

function M.open_info_panel(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local win = vim.api.nvim_get_current_win()
  local info_bufnr = M.get_info_bufnr(bufnr)

  vim.cmd.sbuffer {
    args = { info_bufnr },
    mods = { keepjumps = true, keepalt = true, vertical = true, split = "belowright" },
  }

  vim.cmd.clearjumps()

  -- todo: add more defaults (sidebar.nvim/lua/sidebar-nvim/view.lua)
  vim.api.nvim_set_option_value("modifiable", false, {buf=info_bufnr})
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false
  vim.opt_local.buflisted = false

  vim.api.nvim_set_current_win(win)

  M.run_autocommands()
end

return M

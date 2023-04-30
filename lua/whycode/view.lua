local M = {}

function M.create_info_panel(bufnr, ft)
  ft = ft or "whyinfo"
  local info_bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_option(info_bufnr, "filetype", ft)
  require("whycode.buffers").buffers[bufnr].info_bufnr = info_bufnr
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
    mods = { keepjumps = true, keepalt = true, vertical = true, split = "belowright"},
  }

  vim.cmd.clearjumps()

  -- todo: add more defaults (sidebar.nvim/lua/sidebar-nvim/view.lua)
  vim.api.nvim_buf_set_option(info_bufnr, "modifiable", false)
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false

  vim.api.nvim_set_current_win(win)
end

return M

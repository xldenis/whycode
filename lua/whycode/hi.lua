local M = {}
local ns

function M.init_highlights()
   ns = vim.api.nvim_create_namespace("HiWhy")
   vim.api.nvim_set_hl(0, "WhyGoal", { bg = "#30423c" })
   vim.api.nvim_set_hl(0, "WhyHypo", { bg = "#574300" })
end

function M.run_autocommands()
  local aug = vim.api.nvim_create_augroup("HiWhy", {clear = true})
  vim.api.nvim_create_autocmd({"ColorScheme"}, {
    group = aug,
    pattern = "*",
    callback = function() M.init_highlights() end
  })
end

function M.highlight(goal)
  local win = vim.api.nvim_get_current_win()
  local buf = vim.api.nvim_get_current_buf()
  local pos = vim.api.nvim_win_get_cursor(win)
  local row = pos[1] - 1
  -- local col = pos[2]

  local current_line = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1]
  local end_col = string.len(current_line)

  local hl_group = "WhyHypo"
  if goal then
    hl_group = "WhyGoal"
  end

  vim.api.nvim_buf_set_extmark(buf, ns, row, 0, {end_row = row, end_col = end_col, hl_group=hl_group})
end

-- lua require'whycode.hi'.highlight()

return M

local M = {}

local view = require("whycode.view")
local whycode_ns = vim.api.nvim_create_namespace("whycode")

M.buffers = {}
M.au = vim.api.nvim_create_augroup("whycode-lsp", { clear = true })

function M.unregister(bufnr)
  assert(M.buffers[bufnr])
  vim.api.nvim_buf_clear_namespace(bufnr, whycode_ns, 0, -1)
  vim.api.nvim_clear_autocmds { group = M.au, buffer = bufnr }
  M.buffers[bufnr].debounce_timer:stop()
  M.buffers[bufnr].debounce_timer:close()
  if M.buffers[bufnr].info_bufnr then
    vim.api.nvim_buf_delete(M.buffers[bufnr].info_bufnr, { force = true })
  end
  M.buffers[bufnr] = nil
end

function M.register(bufnr)
  assert(M.buffers[bufnr] == nil)
  M.buffers[bufnr] = {}
  view.create_info_panel(bufnr)
  view.open_info_panel(bufnr)
  M.buffers[bufnr].debounce_timer = assert(vim.uv.new_timer(), "Could not create timer")
  vim.api.nvim_create_autocmd({"BufDelete", "LspDetach"}, {
    group = M.au,
    buffer = bufnr,
    desc = "Unregister deleted/detached buffer",
    callback = function(ev) M.unregister(ev.buf) end,
  })
end

return M

local M = {}

local function write_info_panel(bufnr, str)
    vim.api.nvim_buf_set_option(bufnr, "modifiable", true)
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, str)
    vim.api.nvim_buf_set_option(bufnr, "modifiable", false)
end

function M.define_handlers(verbose)

---@diagnostic disable-next-line: duplicate-set-field
  vim.lsp.handlers["workspace/executeCommand"] = function(err, result, ctx, cconfig)
    print(vim.inspect(err))
    print(vim.inspect(result))
    print(vim.inspect(ctx))
    print(vim.inspect(cconfig))
  end

---@diagnostic disable-next-line: duplicate-set-field
  vim.lsp.handlers["textDocument/publishDiagnostics"] = function(err, result, ctx, cconfig)

    -- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage
    if verbose then print(vim.inspect(result)) end

    local content = require"whycode.content".render_diagnostics(result)

    local bufnr = vim.api.nvim_get_current_buf()
    local info_bufnr = require"whycode.view".get_info_bufnr(bufnr)

    write_info_panel(info_bufnr, content)

    return require("vim.lsp.diagnostic").on_publish_diagnostics(err, result, ctx, cconfig)
  end
end

return M

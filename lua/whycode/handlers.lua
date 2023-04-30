local M = {}

local function write_info_panel(bufnr, str)
    vim.api.nvim_buf_set_option(bufnr, "modifiable", true)
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, str)
    vim.api.nvim_buf_set_option(bufnr, "modifiable", false)
end

function M.define_handlers()
                 -- textDocument/diagnostic ?
  vim.lsp.handlers["textDocument/publishDiagnostics"] = function(err, result, ctx, cconfig)
    -- local uri = result.uri
    -- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#responseMessage

    print(vim.inspect(result))

    local content = require"whycode.content".render_diagnostics(result)

    local bufnr = vim.api.nvim_get_current_buf()
    local info_bufnr = require"whycode.view".get_info_bufnr(bufnr)

    write_info_panel(info_bufnr, content)

    return require("vim.lsp.diagnostic").on_publish_diagnostics(err, result, ctx, cconfig)
  end
end

return M

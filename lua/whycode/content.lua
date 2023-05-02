local M = {}

local icons = { why = "(?)", nok = "(⨯)", ok  = "(✓)", info = "(i)" }

function M.render_diagnostics(diagnostics)
  local lines = {}

  lines[#lines+1] = ""
  lines[#lines+1] = " Verification Conditions"

  for _,v in pairs(diagnostics["diagnostics"]) do

    local err, msg = require("whycode.aux").parse_nl(v["message"])

    if err then return msg end

    local start_line = v["range"]["start"]["line"] + 1
    local start_char = v["range"]["start"]["character"] + 1
    local end_line = v["range"]["end"]["line"] + 1
    local end_char = v["range"]["end"]["character"] + 1
    lines[#lines+1] = ""
    lines[#lines+1] = string.format(" %s goal: %s", icons.why, msg)
    lines[#lines+1] = string.format("        [from %d:%d", start_line, start_char)
    lines[#lines+1] = string.format("           to %d:%d]", end_line, end_char)
  end

  return lines
end

return M

local M = {}

function M.diag_from_line(line)
  return line / 4
end

function M.parse_nl(msg)
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

return M

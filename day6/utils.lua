local function chunkgroups(lines)
  local groups = {}
  local g = {}

  for line in lines do
    if #line == 0 then
      -- end of current group
      groups[#groups + 1] = g
      g = nil
    else
      if g == nil then g = {} end
      -- append to current group
      g[#g + 1] = line
    end
  end

  -- make sure we grab last group
  if g then groups[#groups + 1] = g end

  return groups
end

local function countkeys(table, fn)
  local i = 0
  for k, v in pairs(table) do
    if fn == nil or fn(v) then i = i + 1 end
  end
  return i
end

return {
  chunkgroups = chunkgroups,
  countkeys   = countkeys
}

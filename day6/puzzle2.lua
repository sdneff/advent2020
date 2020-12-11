utils = require 'utils'

local function countunique(group)
  local qs = {}

  for i, person in ipairs(group) do
    for j = 1, #person do
      local c = string.sub(person, j, j)
      qs[c] = (qs[c] or 0) + 1
    end
  end

  local allyes = function (v)
    return v == #group
  end

  return utils.countkeys(qs, allyes)
end

local groups = utils.chunkgroups(io.lines("questionnaire.txt"))

local sum = 0
for i, group in ipairs(groups) do
  sum = sum + countunique(group)
end

print("total = " .. sum)

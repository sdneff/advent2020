utils = require 'utils'

local function sortbyid(t1, t2)
  return t1.id < t2.id
end

local tickets = {}

for line in io.lines("tickets.txt") do
  tickets[#tickets + 1] = utils.parseticket(line)
end

table.sort(tickets, sortbyid)

local emptyid = -1

for i,v in ipairs(tickets) do
  if v.id + 1 ~= tickets[i + 1].id then
    emptyid = v.id + 1
    break
  end
end

print("open seat=" .. emptyid)

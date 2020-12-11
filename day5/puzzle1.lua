utils = require 'utils'

maxid = -1

for line in io.lines("tickets.txt") do
  local ticket = utils.parseticket(line)
  if (maxid < ticket.id) then
    maxid = ticket.id
  end
end

print("max id=" .. maxid)

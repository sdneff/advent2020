local function charsub(s, map)
  return string.gsub(s, ".", function (c)
    return map[c]
  end)
end

local function decodebinary(s, map)
  return tonumber(charsub(s, map), 2)
end

local function parseticket(s)
  local mt = {
    __tostring = function(self)
      return "row=" .. self.row
        .. " column=" .. self.column
        .. " id=" .. self.id
    end
  }

  local row = decodebinary(string.sub(s, 1, 7), { F=0, B=1 })
  local col = decodebinary(string.sub(s, 8, 10), { L=0, R=1 })
  local id = row * 8 + col

  local ticket = {
    row    = row,
    column = col,
    id     = id
  }

  setmetatable(ticket, mt)

  return ticket
end

return {
  parseticket = parseticket,
  decodebinary = decodebinary
}

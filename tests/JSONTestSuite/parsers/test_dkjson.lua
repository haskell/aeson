local json = require ("dkjson") -- luarocks install dkjson

function readAll(file)
    local f = io.open(file, "rb")
    local content = f:read("*all")
    f:close()
    return content
end

local data = readAll(arg[1])

local obj, pos, err = json.decode(data)

-- print ("obj:", obj)
-- print ("pos:", pos)
-- print ("err:", err)

if err then
  print ("Error:", err)
  os.exit(1)
end

-- print ("--", obj)

os.exit(0)

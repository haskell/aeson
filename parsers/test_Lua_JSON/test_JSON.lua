-- http://regex.info/blog/lua/json
JSON = (loadfile "parsers/test_Lua_JSON/JSON.lua")() -- one-time load of the routines

function readAll(file)
    local f = io.open(file, "rb")
    local content = f:read("*all")
    f:close()
    return content
end

local data = readAll(arg[1])

local lua_value = JSON:decode(data)

if lua_value == nil then
  print ("Error:", err)
  os.exit(1)
end

print ("--", lua_value )

os.exit(0)

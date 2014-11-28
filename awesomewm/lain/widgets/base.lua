
--[[
                                                  
     Licensed under GNU General Public License v2 
      * (c) 2014, Luke Bonham                     
                                                  
--]]

local newtimer     = require("lain.helpers").newtimer
local wibox        = require("wibox")

local io           = { popen = io.popen }
local setmetatable = setmetatable

-- Basic template for custom widgets
-- lain.widgets.base

local function worker(args)
    local base     = {}
    local args     = args or {}
    local timeout  = args.timeout or 5
    local cmd      = args.cmd or ""
    local settings = args.settings or function() end

    base.widget = wibox.widget.textbox('')

    function base.update()
        local f = assert(io.popen(cmd))
        output = f:read("*a")
        f:close()
        widget = base.widget
        settings()
    end

    newtimer(cmd, timeout, base.update)

    return setmetatable(base, { __index = base.widget })
end

return setmetatable({}, { __call = function(_, ...) return worker(...) end })

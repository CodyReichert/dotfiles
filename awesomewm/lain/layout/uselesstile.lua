
--[[
                                                  
     Licensed under GNU General Public License v2 
      * (c) 2014       projektile                 
      * (c) 2013       Luke Bonham                
      * (c) 2009       Donald Ephraim Curtis      
      * (c) 2008       Julien Danjolu             
                                                  
--]]

local tag       = require("awful.tag")
local beautiful = require("beautiful")
local ipairs    = ipairs
local math      = { floor = math.floor,
                    max   = math.max,
                    min   = math.min }
local tonumber  = tonumber

local uselesstile = {}

local function tile_group(cls, wa, orientation, fact, group)
    -- A useless gap (like the dwm patch) can be defined with
    -- beautiful.useless_gap_width .
    local useless_gap = tonumber(beautiful.useless_gap_width) or 0
    if useless_gap < 0 then useless_gap = 0 end

    -- A global border can be defined with
    -- beautiful.global_border_width
    local global_border = tonumber(beautiful.global_border_width) or 0
    if global_border < 0 then global_border = 0 end

    -- Themes border width requires an offset
    local bw = tonumber(beautiful.border_width) or 0

    -- get our orientation right
    local height = "height"
    local width = "width"
    local x = "x"
    local y = "y"
    if orientation == "top" or orientation == "bottom" then
        height = "width"
        width = "height"
        x = "y"
        y = "x"
    end

    -- make this more generic (not just width)
    --if for top
    available = wa[width] - (group.coord - wa[x]) -- it's truly not here

    -- find our total values
    local total_fact = 0
    local min_fact = 1
    local size = group.size
    for c = group.first,group.last do
        -- determine the width/height based on the size_hint
        local i = c - group.first +1
        local size_hints = cls[c].size_hints
        local size_hint = size_hints["min_"..width] or size_hints["base_"..width] or 0
        size_hint = size_hint + cls[c].border_width*2
        size = math.max(size_hint, size)

        -- calculate the height
        if not fact[i] then
            fact[i] = min_fact
        else
            min_fact = math.min(fact[i],min_fact)
        end
        total_fact = total_fact + fact[i]
    end
    size = math.min(size, (available - global_border))
    local coord = wa[y]
    local geom = {}
    local used_size = 0
    local unused = wa[height] - (global_border * 2)
    local stat_coord = wa[x]
    --stat_coord = size
    for c = group.first,group.last do
        local i = c - group.first +1
        geom[width] = size - global_border - (bw * 2)
        geom[height] = math.floor(unused * fact[i] / total_fact) - (bw * 2)
        geom[x] = group.coord + global_border + (useless_gap / 2)
        geom[y] = coord + global_border + (useless_gap / 2)

        coord = coord + geom[height]
        unused = unused - geom[height]
        total_fact = total_fact - fact[i]
        used_size = math.max(used_size, geom[width])

        -- Useless gap
        if useless_gap > 0
        then
            -- Top and left clients are shrinked by two steps and
            -- get moved away from the border. Other clients just
            -- get shrinked in one direction.

            top = false
            left = false

            if geom[y] == wa[y] then
                top = true
            end

            if geom[x] == 0 or geom[x] == wa[x] then
                left = true
            end

            if top then
                geom[height] = geom[height] - (2 * useless_gap)
                geom[y] = geom[y] + useless_gap
            else
                geom[height] = geom[height] - useless_gap
            end

            if left then
                geom[width] = geom[width] - (2 * useless_gap)
                geom[x] = geom[x] + useless_gap
            else
                geom[width] = geom[width] - useless_gap
            end
        end
        -- End of useless gap.

        geom = cls[c]:geometry(geom)
    end

    return used_size
end

local function tile(param, orientation)
    local t = tag.selected(param.screen)
    orientation = orientation or "right"

    -- this handles are different orientations
    local height = "height"
    local width = "width"
    local x = "x"
    local y = "y"
    if orientation == "top" or orientation == "bottom" then
        height = "width"
        width = "height"
        x = "y"
        y = "x"
    end

    local cls = param.clients
    local nmaster = math.min(tag.getnmaster(t), #cls)
    local nother = math.max(#cls - nmaster,0)

    local mwfact = tag.getmwfact(t)
    local wa = param.workarea
    local ncol = tag.getncol(t)

    local data = tag.getdata(t).windowfact

    if not data then
        data = {}
        tag.getdata(t).windowfact = data
    end

    local coord = wa[x]
    local place_master = true
    if orientation == "left" or orientation == "top" then
        -- if we are on the left or top we need to render the other windows first
        place_master = false
    end

    -- this was easier than writing functions because there is a lot of data we need
    for d = 1,2 do
        if place_master and nmaster > 0 then
            local size = wa[width]
            if nother > 0 then
                size = math.min(wa[width] * mwfact, wa[width] - (coord - wa[x]))
            end
            if not data[0] then
                data[0] = {}
            end
            coord = coord + tile_group(cls, wa, orientation, data[0], {first=1, last=nmaster, coord = coord, size = size})
        end

        if not place_master and nother > 0 then
            local last = nmaster

            -- we have to modify the work area size to consider left and top views
            local wasize = wa[width]
            if nmaster > 0 and (orientation == "left" or orientation == "top") then
                wasize = wa[width] - wa[width]*mwfact
            end
            for i = 1,ncol do
                -- Try to get equal width among remaining columns
                local size = math.min((wasize - (coord - wa[x]))  / (ncol - i + 1)) --+ (global_border/(ncol))/(ncol+i^2)
                local first = last + 1
                last = last + math.floor((#cls - last)/(ncol - i + 1))
                -- tile the column and update our current x coordinate
                if not data[i] then
                    data[i] = {}
                end
                coord = coord + tile_group(cls, wa, orientation, data[i], { first = first, last = last, coord = coord, size = size })
            end
        end
        place_master = not place_master
    end

end

uselesstile.right = {}
uselesstile.right.name = "uselesstile"
uselesstile.right.arrange = tile

--- The main tile algo, on left.
-- @param screen The screen number to tile.
uselesstile.left = {}
uselesstile.left.name = "uselesstileleft"
function uselesstile.left.arrange(p)
    return tile(p, "left")
end

--- The main tile algo, on bottom.
-- @param screen The screen number to tile.
uselesstile.bottom = {}
uselesstile.bottom.name = "uselesstilebottom"
function uselesstile.bottom.arrange(p)
    return tile(p, "bottom")
end

--- The main tile algo, on top.
-- @param screen The screen number to tile.
uselesstile.top = {}
uselesstile.top.name = "uselesstiletop"
function uselesstile.top.arrange(p)
    return tile(p, "top")
end

uselesstile.arrange = uselesstile.right.arrange
uselesstile.name = uselesstile.right.name

return uselesstile


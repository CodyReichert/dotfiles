-- Calendar with Emacs org-mode agenda for Awesome WM
-- Inspired by and contributed from the org-awesome module, copyright of Damien Leone
-- Licensed under GPLv2
-- Version 1.1-awesome-git
-- @author Alexander Yakushev <yakushev.alex@gmail.com>
local awful = require("awful")
local util = awful.util
local format = string.format
local theme = require("beautiful")
local naughty = require("naughty")
local orglendar = { files = {},
                    char_width = nil,
                    text_color = theme.fg_normal or "#FFFFFF",
                    today_color = theme.fg_focus or "#00FF00",
                    event_color = theme.fg_urgent or "#FF0000",
                    font = theme.font or 'monospace 8',
                    parse_on_show = true,
                    limit_todo_length = nil,
                    date_format = "%d-%m-%Y" }
local freq_table =
   { d = { lapse = 86400,
           occur = 5,
           next = function(t, i)
              local date = os.date("*t", t)
              return os.time{ day = date.day + i, month = date.month,
                              year = date.year }
   end },
     w = { lapse = 604800,
           occur = 3,
           next = function(t, i)
              return t + 604800 * i
     end },
     y = { lapse = 220752000,
           occur = 1,
           next = function(t, i)
              local date = os.date("*t", t)
              return os.time{ day = date.day, month = date.month,
                              year = date.year + i }
     end },
     m = { lapse = 2592000,
           occur = 1,
           next = function(t, i)
              local date = os.date("*t", t)
              return os.time{ day = date.day, month = date.month + i,
                              year = date.year }
     end }
   }
local calendar = nil
local todo = nil
local offset = 0
local data = nil
local function pop_spaces(s1, s2, maxsize)
   local sps = ""
   for i = 1, maxsize - string.len(s1) - string.len(s2) do
      sps = sps .. " "
   end
   return s1 .. sps .. s2
end
local function strip_time(time_obj)
   local tbl = os.date("*t", time_obj)
   return os.time{day = tbl.day, month = tbl.month, year = tbl.year}
end
function orglendar.parse_agenda()
   local today = os.time()
   data = { tasks = {}, dates = {}, maxlen = 20 }
   local task_name
   for _, file in pairs(orglendar.files) do
      local fd = io.open(file, "r")
      if not fd then
         print("W: orglendar: cannot find " .. file)
      else
         for line in fd:lines() do
            local scheduled = string.find(line, "SCHEDULED:")
            local closed = string.find(line, "CLOSED:")
            local deadline = string.find(line, "DEADLINE:")
            if (scheduled and not closed) or (deadline and not closed) then
               local _, _, y, m, d, h, min, recur = string.find(line, "(%d%d%d%d)%-(%d%d)%-(%d%d) %w%w%w ?(%d*)%:?(%d*)[^%+]*%+?([^>]*)>")
               if h ~= "" then
                  h = tonumber(h)
               else
                  h = 23
               end
               if min ~= "" then
                  min = tonumber(min)
               else
                  min = 59
               end
               local task_date = os.time{day = tonumber(d), month = tonumber(m),
                                         year = tonumber(y), hour = h, min = min}
               if d and task_name and (task_date >= today or recur ~= "") then
                  local find_begin, task_start = string.find(task_name, "[A-Z]+%s+")
                  if task_start and find_begin == 1 then
                     task_name = string.sub(task_name, task_start + 1)
                  end
                  local task_end, _, task_tags = string.find(task_name,"%s+(:.+):")
                  if task_tags then
                     task_name = string.sub(task_name, 1, task_end - 1)
                  else
                     task_tags = " "
                  end
                  local len = string.len(task_name) + string.len(task_tags)
                  if (len > data.maxlen) and (task_date >= today) then
                     data.maxlen = len
                  end
                  if recur ~= "" then
                     local _, _, interval, freq = string.find(recur, "(%d)(%w)")
                     local now = os.time()
                     local curr
                     local event_time = task_date -- os.time({day = tonumber(d), month = tonumber(m), year = y})
                     if freq == "d" then
                        curr = math.max(now, event_time)
                     elseif freq == "w" then
                        local count = math.floor((now - event_time) / (freq_table.w.lapse * interval))
                        if count < 0 then count = 0 end
                        curr = event_time + count * (freq_table.w.lapse * interval)
                     else
                        curr = event_time
                     end
                     while curr < now do
                        curr = freq_table[freq].next(curr, interval)
                     end
                     for i = 1, freq_table[freq].occur do
                        local curr_date = os.date("*t", curr)
                        table.insert(data.tasks, { name = task_name,
                                                   tags = task_tags,
                                                   date = curr,
                                                   recur = recur})
                        data.dates[strip_time(curr)] = true
                        curr = freq_table[freq].next(curr, interval)
                     end
                  else
                     table.insert(data.tasks, { name = task_name,
                                                tags = task_tags,
                                                date = task_date,
                                                recur = recur})
                     data.dates[strip_time(task_date)] = true
                  end
               end
            end
            _, _, task_name = string.find(line, "%*+%s+(.+)")
         end
      end
   end
   table.sort(data.tasks, function (a, b) return a.date < b.date end)
end
local function create_calendar()
   offset = offset or 0
   local now = os.date("*t")
   local cal_month = now.month + offset
   local cal_year = now.year
   if cal_month > 12 then
      cal_month = (cal_month % 12)
      cal_year = cal_year + 1
   elseif cal_month < 1 then
      cal_month = (cal_month + 12)
      cal_year = cal_year - 1
   end
   local last_day = tonumber(os.date("%d", os.time({ day = 1, year = cal_year,
                                                     month = cal_month + 1}) - 86400))
   local first_day = os.time({ day = 1, month = cal_month, year = cal_year})
   local first_day_in_week =
      (os.date("%w", first_day) + 6) % 7
   local result = "Mo Tu We Th Fr Sa Su\n"
   for i = 1, first_day_in_week do
      result = result .. " "
   end
   local this_month = false
   for day = 1, last_day do
      local last_in_week = (day + first_day_in_week) % 7 == 0
      local day_str = pop_spaces("", day, 2) .. (last_in_week and "" or " ")
      if cal_month == now.month and cal_year == now.year and day == now.day then
         this_month = true
         result = result ..
            format('<span weight="bold" foreground = "%s">%s</span>',
                   orglendar.today_color, day_str)
      elseif data.dates[os.time{day = day, month = cal_month, year = cal_year}] then
         result = result ..
            format('<span weight="bold" foreground = "%s">%s</span>',
                   orglendar.event_color, day_str)
      else
         result = result .. day_str
      end
      if last_in_week and day ~= last_day then
         result = result .. "\n"
      end
   end
   local header
   if this_month then
      header = os.date("%a, %d %b %Y")
   else
      header = os.date("%B %Y", first_day)
   end
   return header, format('<span font="%s" foreground="%s">%s</span>',
                         orglendar.font, orglendar.text_color, result)
end
local function create_todo()
   local result = ""
   local maxlen = data.maxlen + 3
   if limit_todo_length and limit_todo_length < maxlen then
      maxlen = limit_todo_length
   end
   local prev_date, limit, tname
   for i, task in ipairs(data.tasks) do
      if strip_time(prev_date) ~= strip_time(task.date) then
         result = result ..
            format('<span weight = "bold" foreground = "%s">%s</span>\n',
                   orglendar.event_color,
                   pop_spaces("", os.date(orglendar.date_format, task.date), maxlen))
      end
      tname = task.name
      limit = maxlen - string.len(task.tags) - 3
      if limit < string.len(tname) then
         tname = string.sub(tname, 1, limit - 3) .. "..."
      end
      result = result .. pop_spaces(tname, task.tags, maxlen)
      if i ~= #data.tasks then
         result = result .. "\n"
      end
      prev_date = task.date
   end
   if result == "" then
      result = " "
   end
   return format('<span font="%s" foreground="%s">%s</span>',
                 orglendar.font, orglendar.text_color, result), data.maxlen + 3
end
function orglendar.get_calendar_and_todo_text(_offset)
   if not data or parse_on_show then
      orglendar.parse_agenda()
   end
   offset = _offset
   local header, cal = create_calendar()
   return format('<span font="%s" foreground="%s">%s</span>\n%s',
                 orglendar.font, orglendar.text_color, header, cal), create_todo()
end
local function calculate_char_width()
   return theme.get_font_height(font) * 0.555
end
function orglendar.hide()
   if calendar ~= nil then
      naughty.destroy(calendar)
      naughty.destroy(todo)
      calendar = nil
      offset = 0
   end
end
function orglendar.show(inc_offset)
   inc_offset = inc_offset or 0
   if not data or parse_on_show then
      orglendar.parse_agenda()
   end
   local save_offset = offset
   orglendar.hide()
   offset = save_offset + inc_offset
   local char_width = char_width or calculate_char_width()
   local header, cal_text = create_calendar()
   calendar = naughty.notify({ title = header,
                               text = cal_text,
                               timeout = 0, hover_timeout = 0.5,
                               screen = mouse.screen,
   })
   todo = naughty.notify({ title = "TO-DO list",
                           text = create_todo(),
                           timeout = 0, hover_timeout = 0.5,
                           screen = mouse.screen,
   })
end
function orglendar.register(widget)
   widget:connect_signal("mouse::enter", function() orglendar.show(0) end)
   widget:connect_signal("mouse::leave", orglendar.hide)
   widget:buttons(util.table.join( awful.button({ }, 3, function()
                                         orglendar.parse_agenda()
                                               end),
                                   awful.button({ }, 4, function()
                                         orglendar.show(-1)
                                   end),
                                   awful.button({ }, 5, function()
                                         orglendar.show(1)
   end)))
end
return orglendar

-- #######################################################################################
-- HYPRLAND CONFIG (LUA)
-- Hyprland 0.55+ Lua configuration
-- Refer to https://wiki.hypr.land/Configuring/ for more information.
-- #######################################################################################

-----------------------
----- PERMISSIONS -----
-----------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Permissions/
hl.permission("/usr/(bin|local/bin)/hyprpm", "plugin", "allow")

------------------
---- MONITORS ----
------------------

-- See https://wiki.hypr.land/Configuring/Monitors/
-- hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })

hl.monitor({
    output   = "HDMI-A-1",
    mode     = "3440x1440@119.96",
    position = "0x800",
    scale    = 1.25,
    bitdepth = 20,
})

hl.monitor({
    output    = "DP-1",
    mode      = "3840x2160@74.62",
    position  = "auto",
    scale     = 2,
    bitdepth  = 20,
    transform = 3,
})

-- Default workspaces per monitor
hl.workspace_rule({
    workspace = "name:hello",
    monitor   = "HDMI-A-1",
    default   = true,
})

hl.workspace_rule({
    workspace = "name:world",
    monitor   = "DP-1",
    default   = true,
})

---------------------
---- MY PROGRAMS ----
---------------------

-- See https://wiki.hypr.land/Configuring/Keywords/
local terminal = "kitty"
local editor   = "emacs"
local browser  = "google-chrome-unstable"
local menu     = "walker"

-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/
hl.on("hyprland.start", function()
    hl.exec_cmd("uwsm app -- " .. terminal)
    -- hl.exec_cmd("uwsm app -- nm-applet")
    hl.exec_cmd("uwsm app -- waybar")
    hl.exec_cmd("uwsm app -- ags run ~/.config/ags/app.tsx")
    hl.exec_cmd("uwsm app -- swaync -c ~/.config/swaync/config.json -s ~/.config/swaync/styles.css")

    -- KDE Connect
    hl.exec_cmd("uwsm app -- kdeconnect-indicator")

    -- Clipboard via cliphist
    hl.exec_cmd("uwsm app -- wl-paste --type text --watch cliphist store")
    hl.exec_cmd("uwsm app -- wl-paste --type image --watch cliphist store")
end)

-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/
hl.env("XCURSOR_SIZE", "20")
hl.env("HYPRCURSOR_SIZE", "20")
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "auto")

-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Variables/
hl.config({
    xwayland = {
        enabled = true,
    },

    general = {
        gaps_in  = 0,
        gaps_out = 0,

        border_size = 1,

        -- https://wiki.hypr.land/Configuring/Variables/#variable-types for info about colors
        col = {
            active_border   = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        -- Set to true to enable resizing windows by clicking and dragging on borders and gaps
        resize_on_border = true,

        -- Please see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/ before you turn this on
        allow_tearing = false,

        layout = "dwindle",
    },

    decoration = {
        rounding       = 0,
        rounding_power = 0,

        -- Change transparency of focused and unfocused windows
        active_opacity   = 1.0,
        inactive_opacity = 1.0,
    },

    animations = {
        enabled = true,
    },

    dwindle = {
        preserve_split = true, -- You probably want this
    },

    master = {
        new_status = "master",
    },

    misc = {
        force_default_wallpaper = 0,
        disable_hyprland_logo   = true,
        vrr                     = 2,
    },

    render = {
        -- explicit_sync = 2,
        -- explicit_sync_kms = 1,
        direct_scanout = false,
    },

    opengl = {
        nvidia_anti_flicker = true,
    },

    -- debug = {
    --     damage_tracking = 0,
    -- },

    input = {
        kb_layout = "us",

        follow_mouse                = 1,
        mouse_refocus               = true,
        focus_on_close              = true,
        float_switch_override_focus = 0,

        -- s/CapsLock/Ctrl
        kb_options = "ctrl:nocaps",

        -- Faster keyboard input
        repeat_delay = 250,
        repeat_rate  = 60,

        touchpad = {
            natural_scroll = false,
        },
    },
})

--------------------
---- ANIMATIONS ----
--------------------

-- Default curves, see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Animations/
hl.curve("easeOutQuint",   { type = "bezier", points = { {0.23, 1},    {0.32, 1}   } })
hl.curve("easeInOutCubic", { type = "bezier", points = { {0.65, 0.05}, {0.36, 1}   } })
hl.curve("linear",         { type = "bezier", points = { {0, 0},       {1, 1}      } })
hl.curve("almostLinear",   { type = "bezier", points = { {0.5, 0.5},   {0.75, 1.0} } })
hl.curve("quick",          { type = "bezier", points = { {0.15, 0},    {0.1, 1}    } })

hl.animation({ leaf = "global",        enabled = true, speed = 10,   bezier = "default" })
hl.animation({ leaf = "border",        enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows",       enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn",     enabled = true, speed = 4.1,  bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut",    enabled = true, speed = 1.49, bezier = "linear",       style = "popin 87%" })
hl.animation({ leaf = "fadeIn",        enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut",       enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade",          enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers",        enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn",      enabled = true, speed = 4,    bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut",     enabled = true, speed = 1.5,  bezier = "linear",       style = "fade" })
hl.animation({ leaf = "fadeLayersIn",  enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces",    enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn",  enabled = true, speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })

-- Ref https://wiki.hypr.land/Configuring/Workspace-Rules/
-- "Smart gaps" / "No gaps when only"
-- uncomment all if you wish to use that.
-- hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
-- hl.workspace_rule({ workspace = "f[1]", gaps_out = 0, gaps_in = 0 })
-- hl.window_rule({ match = { workspace = "w[tv1]", floating = false }, border_size = 0, rounding = 0 })
-- hl.window_rule({ match = { workspace = "f[1]", floating = false }, border_size = 0, rounding = 0 })

---------------------
---- KEYBINDINGS ----
---------------------

-- Modifiers
local mainMod  = "ALT"   -- Sets Alt as main modifier
local superMod = "SUPER" -- Use Super (Windows) key as modifier
local ctrlMod  = "CTRL"  -- Use Ctrl key as modifier

-- Custom bindings
hl.bind(superMod .. " + F", hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" }))
-- hl.bind(mainMod .. " + W", hl.dsp.focus({ monitor = "DP-4" }))
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "left" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "right" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "down" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "up" }))

-- Resize windows in a submap
hl.bind(mainMod .. " + SHIFT + R", hl.dsp.submap("resize"))
hl.define_submap("resize", function()
    -- Sets repeatable binds for resizing the active window
    hl.bind("right",  hl.dsp.window.resize({ x = 10, y = 0, relative = true }),  { repeating = true })
    hl.bind("left",   hl.dsp.window.resize({ x = -10, y = 0, relative = true }), { repeating = true })
    hl.bind("up",     hl.dsp.window.resize({ x = 0, y = -10, relative = true }), { repeating = true })
    hl.bind("down",   hl.dsp.window.resize({ x = 0, y = 10, relative = true }),  { repeating = true })
    -- Use reset to go back to the global submap
    hl.bind("escape", hl.dsp.submap("reset"))
end)

-- App launchers & window controls
hl.bind(mainMod .. " + Q",     hl.dsp.exec_cmd("uwsm app " .. browser))
hl.bind(mainMod .. " + W",     hl.dsp.exec_cmd("uwsm app " .. terminal))
hl.bind(mainMod .. " + E",     hl.dsp.exec_cmd("uwsm app " .. editor))
hl.bind(mainMod .. " + space", hl.dsp.exec_cmd("uwsm app " .. menu))
hl.bind(mainMod .. " + Y",     hl.dsp.window.float({ action = "toggle" }))

-- Screenshot utilities
local ss = "~/.config/hypr/scripts/screenshot-region"
hl.bind(mainMod .. " + SHIFT + V", hl.dsp.exec_cmd(ss))
hl.bind(mainMod .. " + SHIFT + B", hl.dsp.exec_cmd(ss .. " --delay 3"))

-- swaync
hl.bind(ctrlMod .. " + SHIFT + F", hl.dsp.exec_cmd("uwsm app -- swaync-client -t"))
hl.bind(ctrlMod .. " + SHIFT + D", hl.dsp.exec_cmd("uwsm app -- swaync-client -d"))

-- Layout
-- hl.bind(mainMod .. " + P", hl.dsp.layout("pseudo")) -- dwindle
hl.bind(mainMod .. " + N", hl.dsp.layout("togglesplit")) -- dwindle

hl.bind(mainMod .. " + C", hl.dsp.window.close())
hl.bind(mainMod .. " + M", hl.dsp.exit())

-- Move focus with mainMod + arrow or vim-style keys
hl.bind(mainMod .. " + left",  hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + H",     hl.dsp.focus({ direction = "left" }))

hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + L",     hl.dsp.focus({ direction = "right" }))

hl.bind(mainMod .. " + up",    hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + J",     hl.dsp.focus({ direction = "up" }))

hl.bind(mainMod .. " + down",  hl.dsp.focus({ direction = "down" }))
hl.bind(mainMod .. " + K",     hl.dsp.focus({ direction = "down" }))

-- Switch workspaces with mainMod + [0-9]
hl.bind(mainMod .. " + 0", hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + 9", hl.dsp.workspace.toggle_special("potion"))
hl.bind(mainMod .. " + 1", hl.dsp.focus({ workspace = "hello" }))
hl.bind(mainMod .. " + 2", hl.dsp.focus({ workspace = "world" }))

-- Move active window to a workspace with mainMod + SHIFT + [0-9]
hl.bind(mainMod .. " + SHIFT + 0", hl.dsp.window.move({ workspace = "special:magic" }))
hl.bind(mainMod .. " + SHIFT + 9", hl.dsp.window.move({ workspace = "special:potion" }))
hl.bind(mainMod .. " + SHIFT + 1", hl.dsp.window.move({ workspace = "hello" }))
hl.bind(mainMod .. " + SHIFT + 2", hl.dsp.window.move({ workspace = "world" }))

-- Scroll through existing workspaces with superMod + scroll
hl.bind(superMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(superMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Let AGS dismiss the visible popup on an outside click without consuming the click.
hl.bind("mouse:272", hl.dsp.exec_cmd("ags request -i desktop-shell outside-click"), { non_consuming = true, release = true })

-- Laptop multimedia keys for volume and LCD brightness (repeating + locked)
hl.bind("XF86AudioRaiseVolume",   hl.dsp.exec_cmd("uwsm app -- wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"), { repeating = true, locked = true })
hl.bind("XF86AudioLowerVolume",   hl.dsp.exec_cmd("uwsm app -- wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),       { repeating = true, locked = true })
hl.bind("XF86AudioMute",          hl.dsp.exec_cmd("uwsm app -- wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),      { repeating = true, locked = true })
hl.bind("XF86AudioMicMute",       hl.dsp.exec_cmd("uwsm app -- wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),    { repeating = true, locked = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("uwsm app -- brightnessctl s 10%+"),                           { repeating = true, locked = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("uwsm app -- brightnessctl s 10%-"),                           { repeating = true, locked = true })

-- Media player controls (locked)
hl.bind("XF86AudioNext",  hl.dsp.exec_cmd("uwsm app playerctl next"),       { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("uwsm app playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",  hl.dsp.exec_cmd("uwsm app playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev",  hl.dsp.exec_cmd("uwsm app playerctl previous"),   { locked = true })

----------------------------------
---- WINDOWS AND WORKSPACES ------
----------------------------------

-- Keyboard shortcuts to common apps
hl.bind(superMod .. " + E", hl.dsp.focus({ window = "class:^(" .. editor .. ")$" }))
hl.bind(superMod .. " + W", hl.dsp.focus({ window = "class:^(" .. terminal .. ")$" }))

-- See https://wiki.hypr.land/Configuring/Workspace-Rules/
-- See https://wiki.hypr.land/Configuring/Configuring-Hyprland/#window-rules

-- Example windowrule
-- hl.window_rule({ match = { class = "^(Google Chrome)$", title = "^(Google Chrome)$" }, float = true })

-- Ignore maximize requests from apps
-- hl.window_rule({ match = { class = ".*" }, suppressevent = "maximize" })

-- Fix some dragging issues with XWayland
-- hl.window_rule({ match = { class = "^$", title = "^$", xwayland = true, floating = true, fullscreen = false, pinned = false }, nofocus = true })

-- No border on swaync panel
-- hl.window_rule({ match = { class = "^(swaync)" }, float = true, border_size = 0 })

-- FreeCAD expression editor
hl.window_rule({
    match = { title = "^(.*Expression editor.*)$" },
    float = true,
    size  = "(monitor_w*0.5) (monitor_h*0.05)",
})

-- GPG pinentry dialogs
hl.window_rule({
    match        = { class = "^(pinentry-.*)$" },
    float        = true,
    center       = true,
    stay_focused = true,
})

-- hyprwhspr - Toggle mode (added by hyprwhspr setup)
-- Press once to start, press again to stop
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd("/usr/lib/hyprwhspr/config/hyprland/hyprwhspr-tray.sh record"), {
    description = "Speech-to-text",
})

#!/usr/bin/env sh
#
# KDE autostart script
#
# This script is run when KDE starts a new session.
# It should be placed in ~/.config/autostart-scripts/
#
# $ stow kde-autostart

# 1. Set mouse scroll method
xinput set-prop "Logitech M570" "libinput Scroll Method Enabled" 0, 0, 1
xinput set-prop "Logitech M570" "libinput Button Scrolling Button" 3

# 2. Set input device speed
xset r rate 250 60

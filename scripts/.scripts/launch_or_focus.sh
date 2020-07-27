#!/bin/bash
app_name=$1
workspace_number=`wmctrl -d | grep '\*' | cut -d' ' -f 1`
win_list=`wmctrl -lx | grep $app_name | grep " $workspace_number " | awk '{print $1}'`

active_win_id=`xprop -root | grep '^_NET_ACTIVE_W' | awk -F'# 0x' '{print $2}' | awk -F', ' '{print $1}'`
if [ "$active_win_id" == "0" ]; then
    active_win_id=""
fi

# get next window to focus on, removing id active
switch_to=`echo $win_list | sed s/.*$active_win_id// | awk '{print $1}'`
# if the current window is the last in the list ... take the first one
if [ "$switch_to" == "" ];then
    switch_to=`echo $win_list | awk '{print $1}'`
fi

if [[ -n "${switch_to}" ]]
    then
        (wmctrl -ia "$switch_to") &
    else
        if [[ -n "$2" ]]
            then
                ($2) &
        fi
fi

exit 0

#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bar1
# echo "---" | tee -a /tmp/polybar1.log
# polybar bar >>/tmp/polybar1.log 2>&1 &

# Multiple screens
# if type "xrandr"; then
#   for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
#     MONITOR=$m polybar --reload bar &
#   done
# else
#   polybar --reload bar &
# fi

for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar --reload bar &
done

echo "Bars launched..."
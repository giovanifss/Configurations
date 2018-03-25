#!/bin/bash

ICONS_PATH="<XMOBAR-DIR>/icons"
BATTINFO="$(acpi -b)"
CHARGE="$(echo $BATTINFO | cut -d ' ' -f4 | cut -d '%' -f1)"
REMAINING="$(echo $BATTINFO | cut -d ' ' -f5)"

# Colors
darkred="#cc0000"
orange="#ee9a00"
darkgreen="#008000"

# Final values
icon=""
color=""

# Define color and icon based on battery status
if echo "$BATTINFO" | grep -q "Discharging"; then
  if [ "$CHARGE" -lt 10 ]; then
    color="$darkred"
    icon="Empty_battery.xbm"
  elif [ "$CHARGE" -lt 35 ]; then
    color="$darkred"
    icon="Low_battery.xbm"
  elif [ "$CHARGE" -lt 75 ]; then
    color="$orange"
    icon="Medium_battery.xbm"
  else
    color="$darkgreen"
    icon="Full_battery.xbm"
  fi
else
  if [ "$CHARGE" -lt 10 ]; then
    color="$darkred"
    icon="Charging_battery.xbm"
  elif [ "$CHARGE" -lt 80 ]; then
    color="$orange"
    icon="Charging_battery.xbm"
  else
    color="$darkgreen"
    icon="Charging_battery.xbm"
  fi
fi

# Alert if battery is about to discharge
if [[ $(echo $BATTINFO | grep Discharging) && $REMAINING < 00:15:00 ]]; then
  /usr/bin/notify-send "Low Battery" "$(echo $REMAINING | cut -f 1,2 -d ':')  Remaining    [$CHARGE%]"
fi

# Return battery template for xmobar
echo "$CHARGE% <fc=$color><icon=$ICONS_PATH/$icon/></fc> ($(echo $REMAINING | cut -d ':' -f 1,2))"

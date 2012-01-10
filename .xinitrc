#!/bin/sh

xsetroot -cursor_name left_ptr
setxkbmap -option caps:none
xmodmap -e "keycode 66 = BackSpace"

exec /usr/bin/xmonad


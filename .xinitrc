#!/bin/sh

xsetroot -cursor_name left_ptr
wallp &
udiskie -s &
kbdd

jack_control start
aloop &

cd wiki && make &
cd

exec /usr/bin/xmonad

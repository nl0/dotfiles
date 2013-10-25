#!/bin/sh

xsetroot -cursor_name left_ptr
wallp &
udiskie -s &
kbdd

jack_control start
aloop &

redshift -v -l 55:61 -t 6000:4500 &

cd wiki && make &
cd

exec /usr/bin/xmonad

#!/bin/sh

xsetroot -cursor_name left_ptr
wallp &
udiskie -s &
jackd &
kbdd

cd wiki && make &
cd

exec /usr/bin/xmonad

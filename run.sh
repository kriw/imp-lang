#!/bin/sh
install="default"
executable="main.exe"
jbuilder build $executable $instsall
./_build/$install/$executable $@

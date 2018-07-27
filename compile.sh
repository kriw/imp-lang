#!/bin/sh
install="default"
executable="main.exe"
output="tmp"
asm="/tmp/$output.asm"
obj="/tmp/$output.o"
jbuilder build $executable $instsall
./_build/$install/$executable $@ > $asm
nasm -f elf64 -o $obj $asm
ld -s -o $output $obj

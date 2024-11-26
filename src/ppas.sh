#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/andreas/Projekte/Pascal/flexpacket/project1
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2     -L. -o /home/andreas/Projekte/Pascal/flexpacket/project1 -T /home/andreas/Projekte/Pascal/flexpacket/link1364529.res -e _start
if [ $? != 0 ]; then DoExitLink /home/andreas/Projekte/Pascal/flexpacket/project1; fi
IFS=$OFS
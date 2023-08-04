#!/bin/bash

echo ".o : subtract.c"
echo ""
arm-linux-gnueabi-gcc -c -o subtract.o testfs/subtract.c
arm-linux-gnueabi-objcopy -S -O binary subtract.o subtract.bin
hexdump -e '"%08x " "\n"' subtract.bin 
# hexdump -e '"%08x " "\n"' subtract.bin > subtract.hex
rm subtract.o subtract.bin

echo ""
echo ".o : subtract.c hf"
echo ""
arm-linux-gnueabihf-gcc -marm -c -o sub.o testfs/subtract.c
arm-linux-gnueabihf-objcopy -S -O binary sub.o sub.bin
hexdump -e '"%08x " "\n"' sub.bin 
# hexdump -e '"%08x " "\n"' sub.bin > subhf.hex
rm sub.o sub.bin

echo ""
echo ".o : prime.c"
echo ""
arm-linux-gnueabi-gcc -c -o prime.o testfs/prime.c
arm-linux-gnueabi-objcopy -S -O binary prime.o prime.bin
hexdump -e '"%08x " "\n"' prime.bin 
# hexdump -e '"%08x " "\n"' prime.bin > prime.hex
rm prime.o prime.bin

echo ""
echo ".o : fib.c"
echo ""
arm-linux-gnueabi-gcc -c -o fib.o testfs/fib.c
arm-linux-gnueabi-objcopy -S -O binary fib.o fib.bin
hexdump -e '"%08x " "\n"' fib.bin 
# hexdump -e '"%08x " "\n"' fib.bin > fib.hex
rm fib.o fib.bin

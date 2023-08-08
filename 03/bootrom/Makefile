boot: 
	arm-linux-gnueabi-as --warn --fatal-warnings -march=armv5t boot.S -o boot.o
	arm-linux-gnueabi-ld boot.o -Ttext=0 -T boot.lds -o boot.elf
	arm-linux-gnueabi-objdump -D boot.elf > boot.list
	qemu-system-arm -M versatilepb -m 128M -nographic -kernel boot.elf

clean:
	rm -rf boot.o boot.elf boot.bin boot.list

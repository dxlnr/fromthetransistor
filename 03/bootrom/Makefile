boot:
	@# Building
	@arm-linux-gnueabi-as -mcpu=arm926ej-s -g startup.s -o startup.o 
	@arm-linux-gnueabi-gcc -c -mcpu=arm926ej-s -g uart.c -o uart.o
	@# Linking
	@arm-linux-gnueabi-ld -T mem.lds uart.o startup.o -o startup.elf
	@arm-linux-gnueabi-objcopy -O binary startup.elf boot.bin
	@# Running
	@qemu-system-arm -M versatilepb -m 128M -audiodev id=none,driver=none -nographic -kernel boot.bin

link:
	@# Building
	@arm-linux-gnueabi-as -mcpu=arm926ej-s -g startup.s -o startup.o 
	@arm-linux-gnueabi-gcc -c -mcpu=arm926ej-s -g uart.c -o uart.o
	@# Linking
	@arm-linux-gnueabi-ld -T mem.lds uart.o startup.o -o startup.elf

clean:
	@rm -f *.o *.elf *.bin

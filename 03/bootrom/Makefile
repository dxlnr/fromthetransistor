all: boot.o boot.elf disk.img # kernel.o kernel.elf

boot.o: boot.S
	arm-linux-gnueabihf-as boot.S -o boot.o

# kernel.o: kernel.S
# 	arm-linux-gnueabihf-as kernel.S -o kernel.o

boot.elf: boot.lds boot.o
	arm-linux-gnueabihf-ld boot.o -T boot.lds -nostdlib --nmagic -o boot.elf
	arm-linux-gnueabihf-objcopy -O binary boot.elf boot.bin

# kernel.elf: kernel.lds kernel.o
# 	arm-linux-gnueabihf-ld kernel.o -T kernel.lds -nostdlib --nmagic -o kernel.elf
# 	arm-linux-gnueabihf-objcopy -O binary kernel.elf kernel.bin

disk.img: boot.bin # kernel.bin
	dd if=/dev/zero of=disk.img bs=512 count=2880
	dd if=boot.bin of=disk.img bs=512 conv=notrunc
	# dd if=kernel.bin of=disk.img bs=512 seek=1 conv=notrunc

qemu: disk.img
	qemu-system-arm -machine virt -cpu cortex-a7 -m 2096 -serial stdio -kernel ./disk.img

clean:
	rm -rf boot.o boot.elf boot.bin kernel.o kernel.elf kernel.bin disk.img

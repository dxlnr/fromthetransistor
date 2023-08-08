OUTPUT_FORMAT("elf32-littlearm", "elf32-littlearm", "elf32-littlearm")
OUTPUT_ARCH(arm)
ENTRY(_Reset)

RAM_OFFSET = 0x40010000;
ROM_OFFSET = 0x00000000;

SECTIONS
{
	bios ROM_OFFSET :
	{
		boot.o (.text)
		boot.o (.data)
		boot.o (.bss)

	}
	kernel_addr = RAM_OFFSET;
}

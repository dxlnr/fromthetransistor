OUTPUT_FORMAT("elf32-littlearm")
TARGET(binary)

ENTRY( _start )

RAM_OFFSET = 0x40000000;
ROM_OFFSET = 0x00000000;

SECTIONS
{
	boot ROM_OFFSET :
	{
		boot.o (.text)
		boot.o (.data)
		boot.o (.bss)

	}
	kernel_addr = RAM_OFFSET;
}

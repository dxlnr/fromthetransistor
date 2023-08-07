OUTPUT_FORMAT("elf32-littlearm")
TARGET(binary)

ENTRY( _start )

RAM_OFFSET = 0x40000000;

SECTIONS
{
	. = RAM_OFFSET;
	.text : { kernel.o (.text) }
	.data : { kernel.o (.data) }
	.bss : { kernel.o (.bss) }
}

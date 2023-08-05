OUTPUT_FORMAT("elf32-littlearm")
TARGET(binary)

ENTRY( _start )

ROM_OFFSET = 0x00000000;

SECTIONS
{
  .text : {} 
  .rodata : {}
  .data : {}
  .bss : {} 
}

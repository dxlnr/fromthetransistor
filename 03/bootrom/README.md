## Bootrom

BootROM (Boot Read-Only Memory) is a small piece of mask ROM or write-protected flash embedded inside the processor chip. This code is executed when the computer is powered on or reset, and it initializes the hardware and performs basic system checks before the operating system is loaded from the primary storage device, such as the hard drive or solid-state drive.

#### Object Files and Sections

[POSIX]() sections include:

- `.text`, where your code lives. It is usually a loadable, readonly, executable section.
- `.data` contains the initial values of global variables. It’s loadable.
- `.rodata` contains constants. It is loadable and readonly.
- `.bss` is an empty allocatable section. C specifies that uninitialized globals default to zero; this is a convenient way for avoiding storing a huge block of zeros in the executable!
--Debug sections that are not loaded or allocated; these are usually removed for release builds.

```
SECTIONS {
  /* Define an output section ".text". */
  .text : {
    /* Pull in all symbols in input sections named .text */
    *(.text)
    /* Do the same for sections starting with .text.,
       such as .text.foo */
    *(.text.*)
  }

  /* Do the same for ".bss", ".rodata", and ".data". */
  .bss : { *(.bss); *(.bss.*) }
  .data : { *(.data); *(.data.*) }
  .rodata : { *(.rodata); *(.rodata.*) }
}
```


### Setup 

Check out the [Qemu](https://wiki.qemu.org/Documentation/Platforms/ARM) Documentation for ARM.
```bash
# List of available CPUs
qemu-system-aarch64 -machine virt -cpu help
# start the emulator
qemu-system-aarch64 -machine -virt -cpu cortex-a7

qemu-system-riscv64 -machine virt -cpu cortex-a7 -smp 4 -m 128M  -serial mon:stdio -bios none -kernel kernel.elf
```

```
# Compilation 
arm-linux-gnueabihf-as boot.S -o boot.o
# Linking
arm-linux-gnueabihf-ld -T kernel.lds boot.o -o kernel.elf
```

[ARM mach-virt emulation](https://github.com/qemu/qemu/blob/master/hw/arm/virt.c). This provides the qemu memory layout implementation in a table.

### NotesThe Player Of Games: A Culture Novel
- Assembly: A [Calling Convention](https://en.wikipedia.org/wiki/Calling_convention) is an implementation-level (low-level) scheme for how subroutines or functions receive parameters from their caller and how they return a result.
This is architecture specific.

### Additionals

- [Tutorial](https://www.cs.ucr.edu/~csong/cs153/20f/lab0.html) on Memory
- [Turing Complete Game](https://turingcomplete.game/)

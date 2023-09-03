## Bootrom

[BootROM](https://en.wikipedia.org/wiki/Boot_ROM) (Boot Read-Only Memory) is a small piece of mask ROM or write-protected flash embedded inside the processor chip. This code is executed when the computer is powered on or reset, and it initializes the hardware and performs basic system checks before the operating system is loaded from the primary storage device, such as the hard drive or solid-state drive.

#### Object Files and Sections


### Linker Script

A linker script contains four things:
- **Memory** layout: what memory is available where
    ```c
    MEMORY
        {
            name [(attr)] : ORIGIN = origin, LENGTH = len
            …
        }
    ```
    - `name` is a name you want to use for this region. Names do not carry function.
    - `(attr)` are optional attributes for the region, like whether it is writable `(w)`, readable `(r)`, or executable `(x)`. Flash memory (ROM) is usually `(rx)`, while RAM is `(rwx)`. Marking a region as non-writable does not magically make it write protected: These attributes are meant to describe the properties of the memory, not set it.
    - `origin` is the start address of the memory region.
    - `len` is the size of the memory region, in bytes.

    Example: The memory map for the [SAMD21G18](https://cdn.sparkfun.com/datasheets/Dev/Arduino/Boards/Atmel-42181-SAM-D21_Datasheet.pdf):
    ```s
    MEMORY
        {
            ROM (rx)  : ORIGIN = 0x00000000, LENGTH = 0x00040000
            RAM (rwx) : ORIGIN = 0x20000000, LENGTH = 0x00008000
        }
    ```

    For investigating the addresses collected after the linking, run:
    ```bash
    make link && arm-linux-gnueabi-nm startup.elf
    ```

- **Section** definitions: what part of a program should go where

    Code and data are bucketed into sections, which are contiguous areas of memory. There are no hard rules about how many sections you should have.

    [POSIX](https://en.wikipedia.org/wiki/POSIX) sections include:

    - `.text`, where your code lives. It is usually a loadable, readonly, executable section.
    - `.data` contains the initial values of global variables. It’s loadable.
    - `.rodata` contains constants. It is loadable and readonly.
    - `.bss` is an empty allocatable section. C specifies that uninitialized globals default to zero; this is a convenient way for avoiding storing a huge block of zeros in the executable!
    - Debug sections that are not loaded or allocated; these are usually removed for release builds.

    ```c
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
    Find the sections in the object file:
    ```bash
    make link && arm-linux-gnueabi-objdump -t startup.elf
    ```
    
- **Options**: commands to specify architecture, entry point, …etc. if needed
- **Symbols**: variables to inject into the program at link time


### Setup 

Check out the [Qemu](https://wiki.qemu.org/Documentation/Platforms/ARM) Documentation for ARM and the [ARM mach-virt emulation](https://github.com/qemu/qemu/blob/master/hw/arm/virt.c). 
This provides the qemu memory layout implementation in a table.

```bash
# List of available CPUs
qemu-system-arm -machine virt -cpu help
# Compile and boot it in one go 
make boot
```



### Notes

- Assembly: A [Calling Convention](https://en.wikipedia.org/wiki/Calling_convention) is an implementation-level (low-level) scheme for how subroutines or functions receive parameters from their caller and how they return a result. This is architecture specific.
- ARM32 System Call: 
    - `swi 0`: Software interrupt : Upon this instruction, the kernel takes an action
    - `r7` determines what to do & `r0`-`r4` determines how. 
    - For information in the [syscall table](https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#arm-32_bit_EABI).

### Additionals

- [OSDev](https://wiki.osdev.org/Expanded_Main_Page)
- [Tutorial](https://www.cs.ucr.edu/~csong/cs153/20f/lab0.html) on Memory
- [Ebook](https://github.com/umanovskis/baremetal-arm/tree/master) about bare-metal programming for ARM
- [Turing Complete Game](https://turingcomplete.game/)
- [Blog Post](https://mcyoung.xyz/2021/06/01/linker-script/#memory-regions-and-section-allocation) by Miguel Young de la Sota.

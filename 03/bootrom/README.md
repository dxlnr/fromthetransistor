## Bootrom

BootROM (Boot Read-Only Memory) is a small piece of mask ROM or write-protected flash embedded inside the processor chip. This code is executed when the computer is powered on or reset, and it initializes the hardware and performs basic system checks before the operating system is loaded from the primary storage device, such as the hard drive or solid-state drive.

### Setup 

```bash
# List of available CPUs
qemu-system-aarch64 -machine virt -cpu help
# start the emulator
qemu-system-aarch64 -machine -virt -cpu cortex-a7

qemu-system-riscv64 -machine virt -cpu cortex-a7 -smp 4 -m 128M  -serial mon:stdio -bios none -kernel kernel.elf
```

[ARM mach-virt emulation](https://github.com/qemu/qemu/blob/master/hw/arm/virt.c). This provides the qemu memory layout implementation in a table.

### Notes
- Assembly: A [Calling Convention](https://en.wikipedia.org/wiki/Calling_convention) is an implementation-level (low-level) scheme for how subroutines or functions receive parameters from their caller and how they return a result.
This is architecture specific.

### Additionals

- [Tutorial](https://www.cs.ucr.edu/~csong/cs153/20f/lab0.html) on Memory
- [Turing Complete Game](https://turingcomplete.game/)

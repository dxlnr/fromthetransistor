## CPU

A 32-bit pipelined ARM processor is implemented in Verilog HDL.

```bash
fetch -> decode -> execute -> memory access -> write back
```

### Prerequisits

Running verilog depends on Icarus Verilog. Please follow the [installation guide](https://steveicarus.github.io/iverilog/usage/installation.html).

### Run

```bash
./run_arm32_cpu.sh test/subtract.hex
```

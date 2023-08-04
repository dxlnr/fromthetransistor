## Processor: What is a processor anyway?


### ARM

- [ARM Docs A32](https://developer.arm.com/documentation/ddi0597/2023-06/A32-Instructions-by-Encoding)
- ARM Instructions Layout
```
MNEMONIC{S}{condition} {Rd}, Operand1, Operand2

MNEMONIC     - Short name (mnemonic) of the instruction
{S}          - An optional suffix. If S is specified, the condition flags are updated on the result of the operation
{condition}  - Condition that is needed to be met in order for the instruction to be executed
{Rd}         - Register (destination) for storing the result of the instruction
Operand1     - First operand. Either a register or an immediate value 
Operand2     - Second (flexible) operand. Can be an immediate value (number) or a register with an optional shift
```

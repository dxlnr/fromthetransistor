## Assembler

### Prerequisits

**Cross-Compiler** (if you are not on a ARM architecture natively)
```bash
sudo apt install gcc gcc-arm-linux-gnueabi binutils-arm-linux-gnueabi gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu make
# python
pip install -r requirements.txt
```

### Test & Run

```bash
python arm_asm.py testfs/subtract.s
# Run automatic tests with
python -m unittest
```

Run the bash script to investigate the desired output of the assembler.
```bash
./run_arm32_tests.sh
```

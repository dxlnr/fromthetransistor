#!/bin/bash -e
python3 makehex.py $1 > /tmp/test.bin
iverilog -Wall -g2012 -o arm cpu_testbench.v cpu.v && vvp arm +firmware=test/subtract.hex
rm -rf arm 
rm -rf tmp/test.bin### Instructions

#!/bin/bash

ghc compiler.hs -o hc > /dev/null 2>&1
./hc test/t1.tc

make clean > /dev/null 2>&1
rm hc
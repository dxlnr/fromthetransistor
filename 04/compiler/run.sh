#!/bin/bash

ghc compiler.hs -o hc > /dev/null 2>&1
./hc test/t1.tc
echo ""
./hc test/t2.tc
# echo ""
# ./hc test/t3.tc

make clean > /dev/null 2>&1
rm hc

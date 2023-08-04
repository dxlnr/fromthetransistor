import sys
import os
import unittest
from pathlib import Path

sys.path.insert(0, os.path.abspath(Path(__file__).parent.parent))

from arm_asm import asm32, parser

TESTFS = ["testfs/arm32_subtract.s", "testfs/arm32_prime.s", "testfs/arm32_fib.s"]
TARGETFS = ["test/subtract.hex", "test/prime.hex", "test/fib.hex"]


class TestARMAssembler(unittest.TestCase):
    def test_arm_asm(self):
        ts, asmfs = [], []
        for x in TESTFS:
            with open(x, "r") as f:
                asmfs.append(asm32(parser(f.read())))

        for x in TARGETFS:
            with open(x, "rb") as f:
                ts.append(list(filter(None, f.read().replace(b" ", b"").split(b"\n"))))

        for i, t in enumerate(ts):
            for j, y in enumerate(t):
                try:
                    ts[i][j] = int(y, base=16)
                except ValueError:
                    ts[i][j] = str("*")

        for i, (t, asm) in enumerate(zip(ts, asmfs)):
            for j, (y, x) in enumerate(zip(t, asm)):
                if type(t[j]) == str:
                    continue
                self.assertEqual(
                    y,
                    x,
                    f"Test {i + 1} of {TESTFS[i]} failed for instruction {j+1}: target: {hex(t[j])} != actual: {hex(asm[j])}.",
                )


if __name__ == "__main__":
    unittest.main()

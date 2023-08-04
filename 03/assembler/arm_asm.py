"""ARM Assembler"""
import re
import sys
from enum import Enum


class REGISTERS(Enum):
    r0 = 0
    r1 = 1
    r2 = 2
    r3 = 3
    r4 = 4
    r5 = 5
    r6 = 6
    r7 = 7
    r8 = 8
    r9 = 9
    r10 = 10
    fp = 11
    r12 = 12
    sp = 13
    lr = 14
    pc = 15
    cpsr = 16

    @classmethod
    def as_strs(cls):
        return list(map(lambda c: c.name, cls))


class CONDITION(Enum):
    EQ = 0b0000  # Equal
    NE = 0b0001  # Not equal
    CS = 0b0010  # Carry set (identical to HS)
    HS = 0b0010  # Unsigned higher or same (identical to CS)
    CC = 0b0011  # Carry clear (identical to LO)
    LO = 0b0011  # Unsigned lower (identical to CC)
    MI = 0b0100  # Minus or negative result
    PL = 0b0101  # Positive or zero result
    VS = 0b0110  # Overflow
    VC = 0b0111  # No overflow
    HI = 0b1000  # Unsigned higher
    LS = 0b1001  # Unsigned lower or same
    GE = 0b1010  # Signed greater than or equal
    LT = 0b1011  # Signed less than
    GT = 0b1100  # Signed greater than
    LE = 0b1101  # Signed less than or equal
    AL = 0b1110  # Always (this is the default)

    @classmethod
    def as_strs(cls):
        return list(map(lambda c: c.name.lower(), cls))


class OPCODE(Enum):
    """ARM 32 bit opcodes."""

    ADD = "add"  # Addition
    ADDS = "adds"  # Addition signed
    SUB = "sub"  # Subtraction
    SUBS = "subs"  # Subtraction signed
    MUL = "mul"  # Multiplication
    MULS = "muls"  # Multiplication signed
    STR = "str"  # Store
    STM = "stm"  # Store Multiple
    LDR = "ldr"  # Load
    LDM = "ldm"  # Load Multiple
    MOV = "mov"  # Move data
    MOVS = "movs"  # Move data signed
    MVN = "mvn"  # Move data and negate
    MVNS = "mvns"  # Move data and negate signed
    POP = "pop"  # Pop off Stack
    PUSH = "push"  # Push on Stack
    B = "b"  # Branch
    BL = "bl"  # Branch with Link
    BX = "bx"  # Branch and eXchange
    CMP = "cmp"  # Compare
    EOR = "eor"  # Bitwise XOR
    EORS = "eors"  # Bitwise XOR signed
    LSL = "lsl"  # Logical Shift Left
    LSR = "lsr"  # Logical Shift Right
    ASR = "asr"  # Arithmetic Shift Right
    ROR = "ror"  # Rotate Right
    AND = "and"  # Bitwise AND
    ANDS = "and"  # Bitwise AND signed
    ORR = "orr"  # Bitwise OR
    ORRS = "orr"  # Bitwise OR signed
    SWI = "swi"  # System Call
    SVC = "svc"  # System Call


class Program(Enum):
    DIRECTIVE = 1
    LABEL = 2
    INSTRUCTION = 3
    COMMENT = 4


def sext(val: int, bits: int = 32):
    """Performs sign extension by number of bits given."""
    if val < 0:
        return (2**bits - 1) & val
    else:
        return val


def dump_to_hex(ins: list[int], fn: str) -> None:
    """Writes the output to hex file."""
    with open(f"{fn}", "w") as fp:
        for i in ins:
            fp.write("%08x\n" % i)


def split_words(str_in: str, sl: list[str]) -> list[str]:
    """Reads in a string and splits it into words based on a list of symbols."""
    inswords = ["movs"]
    for w in sl:
        if str_in not in inswords:
            m = list(filter(None, re.split(f"({str(re.escape(w))})", str_in)))
            if len(m) > 1:
                return m
    return [str_in]


def check_const(insb: list[str]) -> bool:
    """Checks if the instruction contains a #<const>."""
    return list(filter(lambda x: x.startswith("#"), insb))


def check_label(insb: list[str]) -> bool:
    """Checks if the instruction contains a <label>."""
    return list(filter(lambda x: x.startswith("."), insb))


def check_pc(insb: list[str]) -> bool:
    """Checks if the instruction contains a <label>."""
    return list(filter(lambda x: x == "pc", insb))


def prepare_labels(tokens: list[tuple]):
    """."""
    res = []
    labels = list(filter(lambda c: c[0] == Program.LABEL, tokens))
    label_names = [lab[1][0].replace(":", "") for lab in labels]

    for li, label in enumerate(labels):
        idx = tokens.index(label)
        for i in range(idx + 1, len(tokens)):
            if i == idx + 1:
                res.append([label_names[li], label[2], label_names[li]])
            if tokens[i][0] == Program.INSTRUCTION:
                break
            if tokens[i][0] == Program.LABEL:
                break
            if tokens[i][0] == Program.DIRECTIVE:
                for wi, w in enumerate(tokens[i][1]):
                    if w == ".word":
                        if tokens[i][1][wi + 1] in label_names:
                            res[-1][2] = tokens[i][1][wi + 1]
                            break
    return res


def get_off_start(labels, label, count) -> int:
    """Returns the offset of the label from the start of the program."""
    label = label if not type(label) == list else label[0]
    tl = next(x for x in labels if label == x[0])
    u = 1 if tl[1] > count else 0
    off = [tl[1]]
    if tl[0] != tl[2]:
        for n in labels:
            if n[0] == tl[2]:
                off.append(n[1])
                break
    return off, u


def parser(opdc: str):
    """Reads the assmebly code and returns list of tokenized symbols.

    :param opdc: The assembly code to be parsed in string format.
    :return: A list of tuples containing the type of the program and its symbols.
    """
    ts, pc = [], 0
    for line in opdc.split("\n"):
        sl = list(filter(None, re.split(r"\t+|,| |\(|\)|(\[)|(\]+)|(\{)|(\}+)", line)))
        if sl:
            if re.match("\.(\w+):", sl[0]):
                ts.append((Program.LABEL, sl, pc))
            elif re.match("\.+", sl[0]):
                ts.append((Program.DIRECTIVE, sl))
            elif re.match("\w+(?: \w+)*:", sl[0]):
                ts.append((Program.LABEL, sl, pc))
            elif re.match("\@+", sl[0]):
                ts.append((Program.COMMENT, sl))
            elif re.match("\//+", sl[0]):
                ts.append((Program.COMMENT, sl))
            else:
                ts.append((Program.INSTRUCTION, sl, pc))
                pc += 1
    return ts


def asm32(tokens) -> list[int]:
    """ARM 32 bit assembler.

    :param tokens: List of a tuple holding the type of the program and its symbols.
    :returns: A list of 32 bit machine code.
    :raises: RuntimeError if the instruction is not supported.
    """
    regs, conds = frozenset(REGISTERS.as_strs()), frozenset(CONDITION.as_strs())
    labels = prepare_labels(tokens)

    ins = []
    for idx, t in enumerate(tokens):
        print(t)
        if t[0] == Program.INSTRUCTION:
            inn = split_words(t[1].pop(0), conds)
            cond = (
                CONDITION[inn[1].upper()].value if len(inn) > 1 else CONDITION.AL.value
            )
            insb = t[1]
            print(inn)
            if wf := any(filter(lambda x: x in ["[", "]"], insb)):
                if insb.index("]") - insb.index("[") <= 2:
                    wf = False
                insb = list(filter(lambda x: x not in ["[", "]"], insb))
            if bbwf := any(filter(lambda x: x in ["{", "}"], insb)):
                insb = list(filter(lambda x: x not in ["{", "}"], insb))
            if ef := any(filter(lambda x: x[-1] == "!", insb)):
                insb = list(filter(lambda x: x not in ["[", "]", "{", "}"], insb))
            if rs := [s for s in insb if s in regs]:
                insb = list(filter(lambda x: x not in rs, insb))
            if len(insb) > 0:
                if const := list(
                    filter(None, [re.findall(r"#(-?\d+)", s) for s in insb])
                ):
                    insb = list(filter(lambda x: not x.startswith("#"), insb))
                    const = const[0][0]

                # TODO: Distinguish between <shift> and <label>
                label = insb[0] if len(insb) else None

            if inn[0] == OPCODE.ADD.value or inn[0] == OPCODE.ADDS.value:
                s_bit = 1 if inn[0] == OPCODE.ADDS.value else 0
                rn = 0xF if rs[1] == "pc" else REGISTERS[rs[1]].value
                if len(rs) == 2:
                    op = 2
                    imm = abs(int(const))
                elif len(rs) == 3:
                    op = 0
                    imm = REGISTERS[rs[2]].value
                elif len(rs) == 4:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (rn << 16)
                    + (s_bit << 20)
                    + (0b100 << 21)
                    + (op << 24)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.SUB.value or inn[0] == OPCODE.SUBS.value:
                s_bit = 1 if inn[0] == OPCODE.SUBS.value else 0
                rn = 0xF if rs[1] == "pc" else REGISTERS[rs[1]].value
                if len(rs) == 2:
                    op = 2
                    imm12 = abs(int(const))
                elif len(rs) == 3:
                    op = 0
                    imm12 = REGISTERS[rs[2]].value
                elif len(rs) == 4:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm12
                    + (REGISTERS[rs[0]].value << 12)
                    + (rn << 16)
                    + (s_bit << 20)
                    + (0b010 << 21)
                    + (op << 24)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.STR.value:
                if len(rs) == 2:
                    u_bit = 0 if int(const) < 0 else 1
                    if not wf:
                        o2wo1, p_bit = 2, 0
                    elif wf and ef:
                        o2wo1, p_bit = 2, 1
                    else:
                        o2wo1, p_bit = 0, 1
                    ins.append(
                        abs(int(const))
                        + (REGISTERS[rs[0]].value << 12)
                        + (REGISTERS[rs[1]].value << 16)
                        + (o2wo1 << 20)
                        + (u_bit << 23)
                        + (p_bit << 24)
                        + (0b010 << 25)
                        + (cond << 28)
                    )
                elif len(rs) == 3:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")

            elif inn[0] == OPCODE.LDR.value:
                op = 2
                if len(rs) == 1:
                    off, u_bit = get_off_start(labels, label, t[2])
                    imm = (
                        off[0] - t[2] - 2
                        if len(off) == 1
                        else sum(abs(o - t[2]) for o in off) + 1
                    )
                    rn = 0xF
                elif len(rs) == 2:
                    u_bit = 0 if int(const) < 0 else 1
                    rn = REGISTERS[rs[1]].value
                    imm = abs(int(const))
                    if not wf:
                        o2wo1, p_bit = 1, 0
                    elif wf and ef:
                        o2wo1, p_bit = 3, 1
                    else:
                        o2wo1, p_bit = 1, 1
                elif len(rs) == 3:
                    op = 3
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (rn << 16)
                    + (o2wo1 << 20)
                    + (u_bit << 23)
                    + (p_bit << 24)
                    + (op << 25)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.MOV.value or inn[0] == OPCODE.MOVS.value:
                s_bit = 1 if inn[0] == OPCODE.MOVS.value else 0
                if len(rs) == 1:
                    imm = abs(int(const))
                    op = 7
                elif len(rs) == 2:
                    op = 3
                    imm = REGISTERS[rs[1]].value
                elif len(rs) == 3:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (0 << 16)
                    + (s_bit << 20)
                    + (1 << 21)
                    + (op << 23)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.PUSH.value:
                registers_list = 0
                for i in reversed(REGISTERS.as_strs()):
                    if i in rs:
                        registers_list += 1 << REGISTERS[i].value
                ins.append(registers_list + (0x92D << 16) + (cond << 28))
            elif inn[0] == OPCODE.POP.value:
                registers_list = 0
                for i in reversed(REGISTERS.as_strs()):
                    if i in rs:
                        registers_list += 1 << REGISTERS[i].value
                ins.append(registers_list + (0x8BD << 16) + (cond << 28))
            elif inn[0] == OPCODE.CMP.value:
                if len(rs) == 1:
                    imm = abs(int(const))
                    op = 0x35
                elif len(rs) == 2:
                    op = 0x15
                    imm = REGISTERS[rs[1]].value
                elif len(rs) == 3:
                    op = 0x15
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (0 << 12)
                    + (REGISTERS[rs[0]].value << 16)
                    + (op << 20)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.B.value or inn[0] == OPCODE.BL.value:
                if label := check_label(insb):
                    off, u_bit = get_off_start(labels, label, t[2])
                    imm = (
                        off[0] - t[2] - 2
                        if len(off) == 1
                        else sum(abs(o - t[2]) for o in off) + 1
                    )
                    off = sext(imm, 24)
                else:
                    off = 0xFFFFFE
                op = 0xA if inn[0] == OPCODE.B.value else 0xB
                ins.append(off + (op << 24) + (cond << 28))
            elif inn[0] == OPCODE.BX.value:
                ins.append(
                    REGISTERS[rs[0]].value
                    + (1 << 4)
                    + (0xFFF << 8)
                    + (18 << 20)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.MUL.value or inn[0] == OPCODE.MULS.value:
                s_bit = 1 if inn[0] == OPCODE.MULS.value else 0
                ins.append(
                    REGISTERS[rs[1]].value
                    + (9 << 4)
                    + (REGISTERS[rs[2]].value << 8)
                    + (0 << 12)
                    + (REGISTERS[rs[0]].value << 16)
                    + (s_bit << 20)
                    + (0 << 21)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.MVN.value or inn[0] == OPCODE.MVNS.value:
                s_bit = 1 if inn[0] == OPCODE.MVNS.value else 0
                if len(rs) == 1:
                    imm = abs(int(const))
                    op = 0x1F
                elif len(rs) == 2:
                    imm = REGISTERS[rs[1]].value
                    op = 0xF
                elif len(rs) == 3:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (0 << 16)
                    + (s_bit << 20)
                    + (op << 21)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.EOR.value or inn[0] == OPCODE.EORS.value:
                s_bit = 1 if inn[0] == OPCODE.EORS.value else 0
                if len(rs) == 2:
                    imm = abs(int(const))
                    op = 0x11
                elif len(rs) == 3:
                    imm = REGISTERS[rs[2]].value
                    op = 1
                elif len(rs) == 4:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (REGISTERS[rs[1]].value << 16)
                    + (s_bit << 20)
                    + (op << 21)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.AND.value or inn[0] == OPCODE.ANDS.value:
                s_bit = 1 if inn[0] == OPCODE.ANDS.value else 0
                if len(rs) == 2:
                    imm = abs(int(const))
                    op = 0x10
                elif len(rs) == 3:
                    imm = REGISTERS[rs[2]].value
                    op = 0
                elif len(rs) == 4:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (REGISTERS[rs[1]].value << 16)
                    + (s_bit << 20)
                    + (op << 21)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.ORR.value or inn[0] == OPCODE.ORRS.value:
                s_bit = 1 if inn[0] == OPCODE.ORRS.value else 0
                if len(rs) == 2:
                    imm = abs(int(const))
                    op = 0x1C
                elif len(rs) == 3:
                    imm = REGISTERS[rs[2]].value
                    op = 0xC
                elif len(rs) == 4:
                    pass
                else:
                    raise RuntimeError(f"Invalid number of registers in {t[0][0]}.")
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (REGISTERS[rs[1]].value << 16)
                    + (s_bit << 20)
                    + (op << 21)
                    + (cond << 28)
                )
            elif inn[0] == OPCODE.ASR.value:
                op = 0xD
                if len(rs) == 2:
                    imm = REGISTERS[rs[2]].value + (5 << 4) + abs(int(const))
                elif len(rs) == 3:
                    imm = REGISTERS[rs[2]].value
                ins.append(
                    imm
                    + (REGISTERS[rs[0]].value << 12)
                    + (0 << 16)
                    + (s_bit << 20)
                    + (op << 21)
                    + (cond << 28)
                )
            # elif inn[0] == OPCODE.STM.value:
            #     pass
            # elif inn[0] == OPCODE.LDM.value:
            #     pass
            # elif inn[0] == OPCODE.LSL.value:
            #     pass
            # elif inn[0] == OPCODE.LSR.value:
            #     pass
            else:
                raise RuntimeError(f"OPCODE '{inn[0]}' not supported.")
    return ins


if __name__ == "__main__":
    try:
        x = sys.argv[1]
    except ValueError:
        raise ValueError("No input file provided.")

    print(f"Read : {x}\n")
    with open(x, "r") as f:
        ts = parser(f.read())
        ins = asm32(ts)

        [print(f"{idx + 1} %08x " % i) for (idx, i) in enumerate(ins)]

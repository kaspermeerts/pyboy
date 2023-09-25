B, C, D, E, H, L, mem_HL, A = range(8)
IRQ_VBLANK = 0b00000001
IRQ_STAT   = 0b00000010
IRQ_TIMER  = 0b00000100
IRQ_SERIAL = 0b00001000
IRQ_JOYPAD = 0b00010000

IF = 0x0F

class CPU:
    def __init__(self, mmu):
        self.CLOCKSPEED = 4 * 1024 * 1024
        self.cycles = 0

        self.mmu = mmu

        self.zram = bytearray(0x7F)

        self._regs = bytearray(8)
        class Flags:
            Z = 0
            N = 0
            H = 0
            C = 0

        self.F = Flags()
        self.PC = 0
        self.SP = 0

        self.IME = 0 # Interrupt Master Enable flag
        self.halted = False
        self.IF = 0
        self.IE = 0

    def __getitem__(self, item):
        return self._regs[item]

    def __setitem__(self, item, value):
        value &= 0xFF
        self._regs[item] = value

    @property
    def AF(self):
        F = 0
        if self.F.Z: F |= 0b10000000
        if self.F.N: F |= 0b01000000
        if self.F.H: F |= 0b00100000
        if self.F.C: F |= 0b00010000
        return (self[A] << 8) | F

    @AF.setter
    def AF(self, value):
        self[A] = value >> 8
        self.F.Z = value & 0b10000000 == 0b10000000
        self.F.N = value & 0b01000000 == 0b01000000
        self.F.H = value & 0b00100000 == 0b00100000
        self.F.C = value & 0b00010000 == 0b00010000

    @property
    def BC(self):
        return (self[B] << 8) | self[C]

    @BC.setter
    def BC(self, value):
        self[B] = (value & 0xFF00) >> 8
        self[C] =  value & 0x00FF

    @property
    def DE(self):
        return (self[D] << 8) | self[E]

    @DE.setter
    def DE(self, value):
        self[D] = (value & 0xFF00) >> 8
        self[E] =  value & 0x00FF

    @property
    def HL(self):
        return (self[H] << 8) | self[L]

    @HL.setter
    def HL(self, value):
        self[H] = (value & 0xFF00) >> 8
        self[L] =  value & 0x00FF

    def __str__(self):
        flags = ""
        flags += "Z" if self.F.Z else "-"
        flags += "N" if self.F.N else "-"
        flags += "H" if self.F.H else "-"
        flags += "C" if self.F.C else "-"
        out = ""
        out += "PC: {:04x} {:s}\n".format(self.PC, "HALTED" if self.halted else "")
        out += "SP: {:04x}\n".format(self.SP)
        out += "AF: {:02x} {:s}\n".format(self[A], flags)
        out += "BC: {:02x} {:02x}\n".format(self[B], self[C])
        out += "DE: {:02x} {:02x}\n".format(self[D], self[E])
        out += "HL: {:02x} {:02x}\n".format(self[H], self[L])

        return out

    def read_pc(self):
        val = self.mmu.read(self.PC)
        self.PC = (self.PC + 1) & 0xFFFF
        return val

    def read_pc_word(self):
        val = self.mmu.read_word(self.PC)
        self.PC = (self.PC + 2) & 0xFFFF
        return val

    def pop(self):
        val = self.mmu.read_word(self.SP)
        self.SP = (self.SP + 2) & 0xFFFF
        return val

    def push(self, value):
        self.SP = (self.SP - 2) & 0xFFFF
        self.mmu.write_word(self.SP, value)

    def tick(self):
        # TODO Interrupts are processed after the fetch. Maybe do that
        # to reproduce the HALT bug?
        cycles_before = self.cycles

        if not self.halted:
            opcode = self.read_pc()
            opcodes[opcode](self, opcode)
            self.cycles += opcode_cycles[opcode]
        else:
            self.cycles += 4

        if self.IE & self.IF:
            self.handle_interrupt()

        return self.cycles - cycles_before

    def handle_interrupt(self):
        assert self.IE & self.IF

        if self.halted:
            self.halted = False
            self.cycles += 4

        if not self.IME:
            return

        self.IME = 0
        self.push(self.PC)

        if self.IE & self.IF & IRQ_VBLANK:
            self.IF &= ~IRQ_VBLANK
            self.PC = 0x40
        elif self.IE & self.IF & IRQ_STAT:
            self.IF &= ~IRQ_STAT
            self.PC = 0x48
        elif self.IE & self.IF & IRQ_TIMER:
            self.IF &= ~IRQ_TIMER
            self.PC = 0x50
        elif self.IE & self.IF & IRQ_SERIAL:
            self.IF &= ~IRQ_SERIAL
            self.PC = 0x58
        elif self.IE & self.IF & IRQ_JOYPAD:
            self.IF &= ~IRQ_JOYPAD
            self.PC = 0x60

        # According to TCAGBD, an interrupt dispatch takes 20 cycles
        # I'd've expected 16, maybe it's because interrupts interrupt
        # the execution of the current instruction
        self.cycles += 20

    def interrupt_request(self, irq):
        assert irq != 0 and irq & (irq - 1) == 0 and irq & 0b00011111 != 0

        self.IF |= irq
B, C, D, E, H, L, mem_HL, A = range(8)

def OP_NOP(self, opcode):
    pass

def OP_LDrrnn(self, opcode):
    val = self.read_pc_word()
    dst = (opcode & 0b00110000) >> 4
    if dst == 0b00: self.BC = val
    if dst == 0b01: self.DE = val
    if dst == 0b10: self.HL = val
    if dst == 0b11: self.SP = val

def OP_LDBCA(self, opcode):
    self.mmu.write(self.BC, self[A])

def OP_INCrr(self, opcode):
    src = (opcode & 0b00110000) >> 4
    if src == 0b00: self.BC += 1
    if src == 0b01: self.DE += 1
    if src == 0b10: self.HL += 1
    if src == 0b11: self.SP = (self.SP + 1) & 0xFFFF

def OP_INCr(self, opcode):
    src = (opcode & 0b00111000) >> 3
    val = self[src] + 1
    self[src] = val

    self.F.Z = val & 0xFF == 0
    self.F.N = 0
    self.F.H = (val & 0xF) == 0
    # Don't touch the Carry flag

def OP_DECr(self, opcode):
    src = (opcode & 0b00111000) >> 3
    val = self[src] - 1
    self[src] = val

    self.F.Z = val & 0xFF == 0
    self.F.N = 1
    self.F.H = (val & 0xF) == 0xF
    # Don't touch the Carry flag

def OP_LDrn(self, opcode):
    val = self.read_pc()
    dst = (opcode & 0b00111000) >> 3

    self[dst] = val

def OP_RLCA(self, opcode):
    self.F.C = self[A] & 0x80 == 0x80
    self[A] = (self[A] << 1) | (self.F.C)
    self.F.Z = 0
    self.F.N = 0
    self.F.H = 0

def OP_LDnnSP(self, opcode):
    val = self.read_pc_word()
    self.mmu.write_word(val, self.SP)

def OP_ADDHLrr(self, opcode):
    src = (opcode & 0b00110000) >> 4
    if src == 0b00: val = self.BC
    if src == 0b01: val = self.DE
    if src == 0b10: val = self.HL
    if src == 0b11: val = self.SP

    self.F.N = 0
    self.F.H = (self.HL & 0x0FFF) + (val & 0x0FFF) > 0x0FFF
    self.F.C = self.HL + val > 0xFFFF

    self.HL += val

def OP_LDABC(self, opcode):
    self[A] = self.mmu.read(self.BC)

def OP_DECrr(self, opcode):
    src = (opcode & 0b00110000) >> 4
    if src == 0b00: self.BC -= 1
    if src == 0b01: self.DE -= 1
    if src == 0b10: self.HL -= 1
    if src == 0b11: self.SP = (self.SP - 1) & 0xFFFF

def OP_RRCA(self, opcode):
    self.F.C = self[A] & 0x1 == 0x1
    self[A] = (self[A] >> 1) | (self.F.C << 7)
    self.F.Z = 0
    self.F.N = 0
    self.F.H = 0

def OP_STOP(self, opcode):
    pass # TODO

def OP_LDDEA(self, opcode):
    self.mmu.write(self.DE, self[A])

def OP_RLA(self, opcode):
    carry = self.F.C
    self.F.C = (self[A] & 0x80) == 0x80
    self[A] = (self[A] << 1) & 0xFF | carry
    self.F.Z = 0
    self.F.N = 0
    self.F.H = 0

def OP_JR(self, opcode):
    e = self.read_pc()
    offset = (e & 0x7F) - (e & 0x80)
    self.PC = (self.PC + offset) & 0xFFFF

def OP_LDADE(self, opcode):
    self[A] = self.mmu.read(self.DE)

def OP_RRA(self, opcode):
    carry = self.F.C
    self.F.C = (self[A] & 0x1) == 0x1
    self[A] = (self[A] >> 1) | (carry << 7)
    self.F.Z = 0
    self.F.N = 0
    self.F.H = 0

def OP_JRcc(self, opcode):
    e = self.read_pc()
    flag = (opcode & 0b00011000) >> 3
    if ( (flag == 0b00 and not self.F.Z) or
         (flag == 0b01 and     self.F.Z) or
         (flag == 0b10 and not self.F.C) or
         (flag == 0b11 and     self.F.C) ):
        offset = (e & 0x7F) - (e & 0x80)
        self.PC = (self.PC + offset) & 0xFFFF
        self.cycles += 4

def OP_LDHLIA(self, opcode):
    self.mmu.write(self.HL, self[A])
    self.HL += 1

def OP_DAA(self, opcode):
    new_a = self[A]
    correction = 0
    if self.F.H: correction |= 0x06
    if self.F.C: correction |= 0x60
    if not self.F.N:
        if self[A] & 0x0F > 0x09: correction |= 0x06
        if self[A]        > 0x99: correction |= 0x60

        new_a += correction
    else:
        new_a -= correction

    self[A] = new_a
    self.F.Z = self[A] == 0
    self.F.H = 0
    self.F.C = self.F.C or new_a >= 0x100

def OP_LDAHLI(self, opcode):
    self[A] = self.mmu.read(self.HL)
    self.HL += 1

def OP_CPL(self, opcode):
    self[A] = self[A] ^ 0xFF

    self.F.N = 1
    self.F.H = 1

def OP_LDHLDA(self, opcode):
    self.mmu.write(self.HL, self[A])
    self.HL -= 1

def OP_INCHL(self, opcode):
    val = (self.mmu.read(self.HL) + 1) & 0xFF
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = (val & 0xF) == 0
    # Don't touch the Carry flag

def OP_DECHL(self, opcode):
    val = (self.mmu.read(self.HL) - 1) & 0xFF
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 1
    self.F.H = (val & 0xF) == 0xF
    # Don't touch the Carry flag

def OP_LDHLn(self, opcode):
    val = self.read_pc()
    self.mmu.write(self.HL, val)

def OP_SCF(self, opcode):
    self.F.N = 0
    self.F.H = 0
    self.F.C = 1

def OP_LDAHLD(self, opcode):
    self[A] = self.mmu.read(self.HL)
    self.HL -= 1

def OP_CCF(self, opcode):
    self.F.N = 0
    self.F.H = 0
    self.F.C = not self.F.C

def OP_LDrr(self, opcode):
    src =  opcode & 0b00000111
    dst = (opcode & 0b00111000) >> 3

    self[dst] = self[src]

def OP_LDrHL(self, opcode):
    dst = (opcode & 0b111000) >> 3

    self[dst] = self.mmu.read(self.HL)

def OP_LDHLr(self, opcode):
    src = opcode & 0b111

    self.mmu.write(self.HL, self[src])

def OP_HALT(self, opcode):
    assert self.IE & 0x1F, "Infinite HALT"
    self.halted = 1

def OP_ADDr(self, opcode):
    src = opcode & 0b00000111
    val = self[src]

    self.F.N = 0
    self.F.H = (self[A] & 0xF) + (val & 0xF) > 0xF
    self.F.C = self[A] + val > 0xFF

    self[A] += val
    self.F.Z = self[A] == 0

def OP_ADDHL(self, opcode):
    val = self.mmu.read(self.HL)

    self.F.N = 0
    self.F.H = (self[A] & 0xF) + (val & 0xF) > 0xF
    self.F.C = self[A] + val > 0xFF

    self[A] += val
    self.F.Z = self[A] == 0

def OP_ADCr(self, opcode):
    src = opcode & 0b00000111
    val = self[src]

    carry = self.F.C
    self.F.N = 0
    self.F.H = (self[A] & 0xF) + (val & 0xF) + carry > 0xF
    self.F.C = self[A] + val + carry > 0xFF

    self[A] += val + carry
    self.F.Z = self[A] == 0

def OP_ADCHL(self, opcode):
    val = self.mmu.read(self.HL)

    carry = self.F.C
    self.F.N = 0
    self.F.H = (self[A] & 0xF) + (val & 0xF) + carry > 0xF
    self.F.C = self[A] + val + carry > 0xFF

    self[A] += val + carry
    self.F.Z = self[A] == 0

def OP_SUBr(self, opcode):
    src = opcode & 0b00000111

    val = self[src]
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF)
    self.F.C = self[A] < val

    self[A] -= val
    self.F.Z = self[A] == 0

def OP_SUBHL(self, opcode):
    val = self.mmu.read(self.HL)

    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF)
    self.F.C = self[A] < val

    self[A] -= val
    self.F.Z = self[A] == 0

def OP_SBCr(self, opcode):
    src = opcode & 0b00000111
    val = self[src]

    carry = self.F.C
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF) + carry
    self.F.C = self[A] < val + carry

    self[A] -= val + carry
    self.F.Z = self[A] == 0

def OP_SBCHL(self, opcode):
    val = self.mmu.read(self.HL)

    carry = self.F.C
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF) + carry
    self.F.C = self[A] < val + carry

    self[A] -= val + carry
    self.F.Z = self[A] == 0

def OP_ANDr(self, opcode):
    src = opcode & 0b00000111

    val = self[src]
    self.F.Z = self[A] & val == 0
    self.F.N = 0
    self.F.H = 1
    self.F.C = 0

    self[A] &= val

def OP_ANDHL(self, opcode):
    val = self.mmu.read(self.HL)

    self.F.Z = self[A] & val == 0
    self.F.N = 0
    self.F.H = 1
    self.F.C = 0

    self[A] &= val

def OP_XORr(self, opcode):
    src = opcode & 0b00000111
    val = self[src]

    self.F.Z = self[A] ^ val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0

    self[A] ^= val

def OP_XORHL(self, opcode):
    val = self.mmu.read(self.HL)

    self.F.Z = self[A] ^ val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0

    self[A] ^= val

def OP_ORr(self, opcode):
    src = opcode & 0b00000111
    val = self[src]

    self.F.Z = self[A] | val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0

    self[A] |= val

def OP_ORHL(self, opcode):
    val = self.mmu.read(self.HL)

    self.F.Z = self[A] | val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0

    self[A] |= val

def OP_CPr(self, opcode):
    src = opcode & 0b00000111
    val = self[src]

    self.F.Z = (self[A] - val) & 0xFF == 0
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF)
    self.F.C = self[A] < val

def OP_CPHL(self, opcode):
    val = self.mmu.read(self.HL)

    self.F.Z = (self[A] - val) & 0xFF == 0
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF)
    self.F.C = self[A] < val

def OP_RETcc(self, opcode):
    flag = (opcode & 0b00011000) >> 3
    if ( (flag == 0b00 and not self.F.Z) or
         (flag == 0b01 and     self.F.Z) or
         (flag == 0b10 and not self.F.C) or
         (flag == 0b11 and     self.F.C) ):
        self.PC = self.pop()
        self.cycles += 12

def OP_POPrr(self, opcode):
    dst = (opcode & 0b00110000) >> 4
    if dst == 0b00: self.BC = self.pop()
    if dst == 0b01: self.DE = self.pop()
    if dst == 0b10: self.HL = self.pop()
    if dst == 0b11: self.AF = self.pop()

def OP_JPccnn(self, opcode):
    dst = self.read_pc_word()
    flag = (opcode & 0b00011000) >> 3
    if ( (flag == 0b00 and not self.F.Z) or
         (flag == 0b01 and     self.F.Z) or
         (flag == 0b10 and not self.F.C) or
         (flag == 0b11 and     self.F.C) ):
        self.PC = dst
        self.cycles += 4

def OP_JPnn(self, opcode):
    dst = self.read_pc_word()
    self.PC = dst

def OP_CALLccnn(self, opcode):
    dst = self.read_pc_word()
    flag = (opcode & 0b00011000) >> 3
    if ( (flag == 0b00 and not self.F.Z) or
         (flag == 0b01 and     self.F.Z) or
         (flag == 0b10 and not self.F.C) or
         (flag == 0b11 and     self.F.C) ):
        self.push(self.PC)
        self.PC = dst
        self.cycles += 12

def OP_PUSHrr(self, opcode):
    src = (opcode & 0b00110000) >> 4
    if src == 0b00: self.push(self.BC)
    if src == 0b01: self.push(self.DE)
    if src == 0b10: self.push(self.HL)
    if src == 0b11: self.push(self.AF)

def OP_ADDn(self, opcode):
    val = self.read_pc()
    self.F.N = 0
    self.F.H = (self[A] & 0xF) + (val & 0xF) > 0xF
    self.F.C = self[A] + val > 0xFF

    self[A] += val
    self.F.Z = self[A] == 0

def OP_RST(self, opcode):
    t = (opcode & 0b00111000) >> 3
    dst = t * 0x8
    self.push(self.PC)
    self.PC = dst

def OP_RET(self, opcode):
    self.PC = self.pop()

def OP_CALLnn(self, opcode):
    dst = self.read_pc_word()
    self.push(self.PC)
    self.PC = dst

def OP_ADCn(self, opcode):
    val = self.read_pc()

    carry = self.F.C
    self.F.N = 0
    self.F.H = (self[A] & 0xF) + (val & 0xF) + carry > 0xF
    self.F.C = self[A] + val + carry > 0xFF

    self[A] += val + carry
    self.F.Z = self[A] == 0

def OP_SUBn(self, opcode):
    val = self.read_pc()

    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF)
    self.F.C = self[A] < val

    self[A] -= val
    self.F.Z = self[A] == 0

def OP_RETI(self, opcode):
    self.PC = self.pop()
    self.IME = 1

def OP_SBCn(self, opcode):
    val = self.read_pc()

    carry = self.F.C
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF) + carry
    self.F.C = self[A] < val + carry

    self[A] -= val + carry
    self.F.Z = self[A] == 0

def OP_LDHnA(self, opcode):
    dst = self.read_pc()
    if dst < 0x80:
        self.mmu.write_io(dst, self[A])
    elif dst < 0xFF:
        self.zram[dst - 0x80] = self[A]
    else:
        self.IE = self[A]

def OP_LDHCA(self, opcode):
    dst = self[C]
    if dst < 0x80:
        self.mmu.write_io(dst, self[A])
    elif dst < 0xFF:
        self.zram[dst - 0x80] = self[A]
    else:
        self.IE = self[A]

def OP_ANDn(self, opcode):
    val = self.read_pc()

    self.F.Z = self[A] & val == 0
    self.F.N = 0
    self.F.H = 1
    self.F.C = 0

    self[A] &= val

def OP_ADDSPe(self, opcode):
    e = self.read_pc()
    s = (e & 0x7f) - (e & 0x80) # Convert two's complement

    # Flags are set according to UNSIGNED addition!
    # http://forums.nesdev.com/viewtopic.php?p=42138
    self.F.Z = 0
    self.F.N = 0
    self.F.H = (self.SP & 0xF) + (e & 0xF) > 0xF
    self.F.C = (self.SP & 0xFF) + e > 0xFF

    self.SP = (self.SP + s) & 0xFFFF

def OP_JPHL(self, opcode):
    self.PC = self.HL

def OP_LDnnA(self, opcode):
    dst = self.read_pc_word()
    self.mmu.write(dst, self[A])

def OP_XORn(self, opcode):
    val = self.read_pc()

    self.F.Z = self[A] ^ val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0

    self[A] ^= val

def OP_LDHAn(self, opcode):
    src = self.read_pc()
    if src < 0x80:
        self[A] = self.mmu.read_io(src)
    elif src < 0xFF:
        self[A] = self.zram[src - 0x80]
    else:
        self[A] = self.IE

def OP_LDHAC(self, opcode):
    src = self[C]
    if src < 0x80:
        self[A] = self.mmu.read_io(src)
    elif src < 0xFF:
        self[A] = self.zram[src - 0x80]
    else:
        self[A] = self.IE

def OP_ORn(self, opcode):
    val = self.read_pc()
    self.F.Z = self[A] | val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0

    self[A] |= val

def OP_DI(self, opcode):
    self.IME = 0

def OP_LDHLSPe(self, opcode):
    e = self.read_pc()
    s = (e & 0x7f) - (e & 0x80) # Convert two's complement
    self.HL = self.SP + s
    # Flags are set according to UNSIGNED addition
    # http://forums.nesdev.com/viewtopic.php?p=42138
    self.F.Z = 0
    self.F.N = 0
    self.F.H = (self.SP & 0xF) + (e & 0xF) > 0xF
    self.F.C = (self.SP & 0xFF) + e > 0xFF

def OP_LDSPHL(self, opcode):
    self.SP = self.HL

def OP_LDAnn(self, opcode):
    src = self.read_pc_word()
    self[A] = self.mmu.read(src)

def OP_EI(self, opcode):
    self.IME = 1

def OP_CPn(self, opcode):
    val = self.read_pc()

    self.F.Z = self[A] - val == 0
    self.F.N = 1
    self.F.H = (self[A] & 0xF) < (val & 0xF)
    self.F.C = self[A] < val

def OP_ILL(self, opcode):
    print("Illegal opcode {:02x}".format(opcode))
    print(self)

def OP_EXTENDED(self, opcode):
    opcode2 = self.read_pc()
    extended_opcodes[opcode2](self, opcode2)
    self.cycles += extended_opcode_cycles[opcode2]

opcode_cycles = [
     4, 12,  8,  8,  4,  4,  8,  4, 20,  8,  8,  8,  4,  4,  8,  4,
     4, 12,  8,  8,  4,  4,  8,  4, 12,  8,  8,  8,  4,  4,  8,  4,
     8, 12,  8,  8,  4,  4,  8,  4,  8,  8,  8,  8,  4,  4,  8,  4,
     8, 12,  8,  8, 12, 12, 12,  4,  8,  8,  8,  8,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     8,  8,  8,  8,  8,  8,  4,  8,  4,  4,  4,  4,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     4,  4,  4,  4,  4,  4,  8,  4,  4,  4,  4,  4,  4,  4,  8,  4,
     8, 12, 12, 16, 12, 16,  8, 16,  8, 16, 12,  0, 12, 24,  8, 16,
     8, 12, 12,  0, 12, 16,  8, 16,  8, 16, 12,  0, 12,  0,  8, 16,
    12, 12,  8,  0,  0, 16,  8, 16, 16,  4, 16,  0,  0,  0,  8, 16,
    12, 12,  8,  4,  0, 16,  8, 16, 12,  8, 16,  4,  0,  0,  8, 16
]
assert len(opcode_cycles) == 256

opcodes = [
    # 0X
    OP_NOP,    OP_LDrrnn,  OP_LDBCA, OP_INCrr, OP_INCr, OP_DECr, OP_LDrn, OP_RLCA,
    OP_LDnnSP, OP_ADDHLrr, OP_LDABC, OP_DECrr, OP_INCr, OP_DECr, OP_LDrn, OP_RRCA,
    # 1X
    OP_STOP, OP_LDrrnn, OP_LDDEA, OP_INCrr, OP_INCr, OP_DECr, OP_LDrn, OP_RLA,
    OP_JR,   OP_ADDHLrr, OP_LDADE, OP_DECrr, OP_INCr, OP_DECr, OP_LDrn, OP_RRA,
    # 2X
    OP_JRcc, OP_LDrrnn,  OP_LDHLIA, OP_INCrr, OP_INCr, OP_DECr, OP_LDrn, OP_DAA,
    OP_JRcc, OP_ADDHLrr, OP_LDAHLI, OP_DECrr, OP_INCr, OP_DECr, OP_LDrn, OP_CPL,
    # 3X
    OP_JRcc, OP_LDrrnn,  OP_LDHLDA, OP_INCrr, OP_INCHL, OP_DECHL, OP_LDHLn, OP_SCF,
    OP_JRcc, OP_ADDHLrr, OP_LDAHLD, OP_DECrr, OP_INCr,  OP_DECr,  OP_LDrn, OP_CCF,
    # 4X 5X 6X 7X
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    OP_LDHLr, OP_LDHLr, OP_LDHLr, OP_LDHLr, OP_LDHLr, OP_LDHLr, OP_HALT,  OP_LDHLr,
    OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrr,  OP_LDrHL, OP_LDrr,
    # 8X 9X AX BX
    OP_ADDr, OP_ADDr, OP_ADDr, OP_ADDr, OP_ADDr, OP_ADDr, OP_ADDHL, OP_ADDr,
    OP_ADCr, OP_ADCr, OP_ADCr, OP_ADCr, OP_ADCr, OP_ADCr, OP_ADCHL, OP_ADCr,
    OP_SUBr, OP_SUBr, OP_SUBr, OP_SUBr, OP_SUBr, OP_SUBr, OP_SUBHL, OP_SUBr,
    OP_SBCr, OP_SBCr, OP_SBCr, OP_SBCr, OP_SBCr, OP_SBCr, OP_SBCHL, OP_SBCr,
    OP_ANDr, OP_ANDr, OP_ANDr, OP_ANDr, OP_ANDr, OP_ANDr, OP_ANDHL, OP_ANDr,
    OP_XORr, OP_XORr, OP_XORr, OP_XORr, OP_XORr, OP_XORr, OP_XORHL, OP_XORr,
    OP_ORr,  OP_ORr,  OP_ORr,  OP_ORr,  OP_ORr,  OP_ORr,  OP_ORHL,  OP_ORr,
    OP_CPr,  OP_CPr,  OP_CPr,  OP_CPr,  OP_CPr,  OP_CPr,  OP_CPHL,  OP_CPr,
    # Cx
    OP_RETcc,   OP_POPrr,  OP_JPccnn, OP_JPnn,      OP_CALLccnn, OP_PUSHrr, OP_ADDn, OP_RST,
    OP_RETcc,   OP_RET,    OP_JPccnn, OP_EXTENDED,  OP_CALLccnn, OP_CALLnn, OP_ADCn, OP_RST,
    # Dx
    OP_RETcc,   OP_POPrr,  OP_JPccnn, OP_ILL,  OP_CALLccnn, OP_PUSHrr, OP_SUBn, OP_RST,
    OP_RETcc,   OP_RETI,   OP_JPccnn, OP_ILL,  OP_CALLccnn, OP_ILL,    OP_SBCn, OP_RST,
    # Ex
    OP_LDHnA,   OP_POPrr,  OP_LDHCA,  OP_ILL,  OP_ILL,      OP_PUSHrr, OP_ANDn, OP_RST,
    OP_ADDSPe,  OP_JPHL,   OP_LDnnA,  OP_ILL,  OP_ILL,      OP_ILL,    OP_XORn, OP_RST,
    # Fx
    OP_LDHAn,   OP_POPrr,  OP_LDHAC,  OP_DI,   OP_ILL,      OP_PUSHrr, OP_ORn,  OP_RST,
    OP_LDHLSPe, OP_LDSPHL, OP_LDAnn,  OP_EI,   OP_ILL,      OP_ILL,    OP_CPn,  OP_RST]
assert len(opcodes) == 256

def OP_RLCr(self, opcode):
    reg = opcode & 0b111

    self.F.C = self[reg] & 0x80 == 0x80
    val = (self[reg] << 1) | (self.F.C)
    self[reg] = val

    self.F.Z = self[reg] == 0
    self.F.N = 0
    self.F.H = 0
def OP_RLCHL(self, opcode):
    val = self.mmu.read(self.HL)
    self.F.C = val & 0x80 == 0x80
    val = (val << 1) | (self.F.C)
    val &= 0xFF
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_RRCr(self, opcode):
    reg = opcode & 0b111

    self.F.C = self[reg] & 0x1 == 0x1
    val = (self[reg] >> 1) | (self.F.C << 7)
    self[reg] = val

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_RRCHL(self, opcode):
    val = self.mmu.read(self.HL)
    self.F.C = val & 0x1 == 0x1
    val = (val >> 1) | (self.F.C << 7)
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_RLr(self, opcode):
    reg = opcode & 0b111
    carry = self.F.C
    self.F.C = self[reg] & 0x80 == 0x80
    val = (self[reg] << 1) | (carry)
    self[reg] = val

    self.F.Z = self[reg] == 0
    self.F.N = 0
    self.F.H = 0
def OP_RLHL(self, opcode):
    val = self.mmu.read(self.HL)
    carry = self.F.C
    self.F.C = val & 0x80 == 0x80
    val = (val << 1) | (carry)
    val &= 0xFF
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_RRr(self, opcode):
    reg = opcode & 0b111
    carry = self.F.C
    self.F.C = self[reg] & 0x1
    val = (self[reg] >> 1) | (carry << 7)
    self[reg] = val

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_RRHL(self, opcode):
    val = self.mmu.read(self.HL)
    carry = self.F.C
    self.F.C = val & 0x1
    val = (val >> 1) | (carry << 7)
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_SLAr(self, opcode):
    reg = opcode & 0b111
    self.F.C = self[reg] & 0x80 == 0x80
    val = self[reg] << 1
    self[reg] = val

    self.F.Z = self[reg] == 0
    self.F.N = 0
    self.F.H = 0
def OP_SLAHL(self, opcode):
    val = self.mmu.read(self.HL)
    self.F.C = val & 0x80 == 0x80
    val = val << 1 & 0xFF
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_SRAr(self, opcode):
    # Because Python treats our registers as unsigned values
    # we need to manually update the leftmost bit
    reg = opcode & 0b111
    self.F.C = self[reg] & 0x1
    sign = self[reg] & 0x80
    val = (self[reg] >> 1) | sign
    self[reg] = val

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_SRAHL(self, opcode):
    # Because Python treats our registers as unsigned values
    # we need to manually update the leftmost bit
    val = self.mmu.read(self.HL)
    self.F.C = val & 0x1
    sign = val & 0x80
    val = (val >> 1) | sign
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_SRLr(self, opcode):
    reg = opcode & 0b111
    self.F.C = self[reg] & 0x1
    val = self[reg] >> 1
    self[reg] = val

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_SRLHL(self, opcode):
    val = self.mmu.read(self.HL)
    self.F.C = val & 0x1
    val = val >> 1
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
def OP_SWAPr(self, opcode):
    reg = opcode & 0b111
    hi, lo = self[reg] & 0xF0, self[reg] & 0x0F
    val = (lo << 4) | (hi >> 4 & 0xFF)
    self[reg] = val

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0
def OP_SWAPHL(self, opcode):
    val = self.mmu.read(self.HL)
    hi, lo = val & 0xF0, val & 0x0F
    val = (lo << 4) | (hi >> 4 & 0xFF)
    self.mmu.write(self.HL, val)

    self.F.Z = val == 0
    self.F.N = 0
    self.F.H = 0
    self.F.C = 0
def OP_BITr(self, opcode):
    reg = opcode & 0b111
    num = (opcode & 0b00111000) >> 3
    bit = 1 << num

    self.F.Z = self[reg] & bit != bit
    self.F.N = 0
    self.F.H = 1
def OP_BITHL(self, opcode):
    num = (opcode & 0b00111000) >> 3
    bit = 1 << num

    val = self.mmu.read(self.HL)

    self.F.Z = val & bit != bit
    self.F.N = 0
    self.F.H = 1
def OP_RESr(self, opcode):
    reg = opcode & 0b111
    num = (opcode & 0b00111000) >> 3
    bit = 1 << num
    mask = ~bit & 0xFF

    self[reg] = self[reg] & mask
def OP_RESHL(self, opcode):
    num = (opcode & 0b00111000) >> 3
    bit = 1 << num
    mask = ~bit & 0xFF

    val = self.mmu.read(self.HL)
    val = val & mask
    self.mmu.write(self.HL, val)
def OP_SETr(self, opcode):
    reg = opcode & 0b111
    num = (opcode & 0b00111000) >> 3
    bit = 1 << num
    mask = ~bit & 0xFF

    self[reg] = (self[reg] & mask) | bit
def OP_SETHL(self, opcode):
    num = (opcode & 0b00111000) >> 3
    bit = 1 << num
    mask = ~bit & 0xFF

    val = self.mmu.read(self.HL)
    val = (val & mask) | bit
    self.mmu.write(self.HL, val)

extended_opcodes = [
    OP_RLCr,  OP_RLCr,  OP_RLCr,  OP_RLCr,  OP_RLCr,  OP_RLCr,  OP_RLCHL,  OP_RLCr,
    OP_RRCr,  OP_RRCr,  OP_RRCr,  OP_RRCr,  OP_RRCr,  OP_RRCr,  OP_RRCHL,  OP_RRCr,
    OP_RLr,   OP_RLr,   OP_RLr,   OP_RLr,   OP_RLr,   OP_RLr,   OP_RLHL,   OP_RLr,
    OP_RRr,   OP_RRr,   OP_RRr,   OP_RRr,   OP_RRr,   OP_RRr,   OP_RRHL,   OP_RRr,
    OP_SLAr,  OP_SLAr,  OP_SLAr,  OP_SLAr,  OP_SLAr,  OP_SLAr,  OP_SLAHL,  OP_SLAr,
    OP_SRAr,  OP_SRAr,  OP_SRAr,  OP_SRAr,  OP_SRAr,  OP_SRAr,  OP_SRAHL,  OP_SRAr,
    OP_SWAPr, OP_SWAPr, OP_SWAPr, OP_SWAPr, OP_SWAPr, OP_SWAPr, OP_SWAPHL, OP_SWAPr,
    OP_SRLr,  OP_SRLr,  OP_SRLr,  OP_SRLr,  OP_SRLr,  OP_SRLr,  OP_SRLHL,  OP_SRLr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITr,  OP_BITHL,  OP_BITr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESr,  OP_RESHL,  OP_RESr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr,
    OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETr,  OP_SETHL,  OP_SETr]
assert len(extended_opcodes) == 256

extended_opcode_cycles = [
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 12,  8,  8,  8,  8,  8,  8,  8, 12,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
    8,  8,  8,  8,  8,  8, 16,  8,  8,  8,  8,  8,  8,  8, 16,  8,
]
assert len(extended_opcode_cycles) == 256


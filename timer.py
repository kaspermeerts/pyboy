# TODO Put these interrupts and constants somewhere?
import cpu

DIV, TIMA, TMA, TAC = range(0x04, 0x08)
TIMER_DIVIDER_BITS = [9, 3, 5, 7]

class Timer:
    def __init__(self, cpu):
        self.cpu = cpu

        self.DIV = 0
        self.TMA = 0
        self.TIMA = 0
        self.TAC = 0

        self.timer_enabled = False
        self.div_bit = TIMER_DIVIDER_BITS[0]

    def read_io(self, address):
        assert DIV <= address <= TAC

        if address == DIV:
            return self.DIV >> 8
        elif address == TIMA:
            return self.TIMA
        elif address == TMA:
            return self.TMA
        elif address == TAC:
            return self.TAC

    def write_io(self, address, value):
        if address == DIV:
            self.DIV = 0
        elif address == TIMA:
            self.TIMA = value
        elif address == TMA:
            self.TMA = value
        elif address == TAC:
            self.TAC = (self.TAC & ~0b111) | (value & 0b111)
            self.timer_enabled = value & 0b100 == 0b100
            self.div_bit = TIMER_DIVIDER_BITS[value & 0b11]

    def step(self, cycles):
        # The TIMA register is incremented whenever there is a falling edge
        # in the nth bit of the DIV register, determined by the TAC register.
        # As a shortcut to calculate it, we consider that a falling edge in
        # the nth place means a 1 is added to the number consisting of the
        # bits above it
        if self.timer_enabled:
            new_div = self.DIV + cycles
            num_falling_edges = (new_div >> (self.div_bit + 1)) - (self.DIV >> (self.div_bit + 1))
            self.TIMA += num_falling_edges
            if self.TIMA > 0xFF:
                self.TIMA = self.TMA
                self.cpu.interrupt_request(cpu.IRQ_TIMER)

        self.DIV = (self.DIV + cycles) & 0xFFFF

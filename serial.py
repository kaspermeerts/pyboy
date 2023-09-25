SB, SC = 0x01, 0x02

# TODO Needs to be clocked. This is necessary to get the interrupt to work

class Serial:
    def __init__(self):
        self.SB = 0
        self.SC = 0

    def read_io(self, address):
        if address == SB:
            return self.SB
        elif address == SC:
            return self.SC & 0b01111110

    def write_io(self, address, value):
        if address == SB:
            self.SB = value
        elif address == SC:
            if value & 0b10000000:
                if 0x20 <= self.SB <= 0x80:
                    print(chr(self.SB), end='') # for blargg test ROMs <3
                self.SB = 0xFF
            self.SC = value & 0b01111111


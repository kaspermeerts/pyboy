# TODO joypad interrupt
# Hard to test, as no game seems to use them

P1 = 0x00

class Joypad:
    def __init__(self):
        self.P15 = 0b00100000
        self.P14 = 0b00010000
        self.input_down = False
        self.input_up = False
        self.input_left = False
        self.input_right = False
        self.input_start = False
        self.input_select = False
        self.input_a = False
        self.input_b = False

    def get_P1(self):
        P15, P14 = self.P15, self.P14
        P13, P12, P11, P10 = 0b1000, 0b0100, 0b0010, 0b0001
        if not self.P15 & 0b00100000:
            if self.input_start:  P13 = 0
            if self.input_select: P12 = 0
            if self.input_a:      P11 = 0
            if self.input_b:      P10 = 0
        if not self.P14 & 0b00010000:
            if self.input_down:   P13 = 0
            if self.input_up:     P12 = 0
            if self.input_left:   P11 = 0
            if self.input_right:  P10 = 0

        return 0b11000000 | P15 | P14 | P13 | P12 | P11 | P10

    def set_P1(self, P1):
        self.P15 = P1 & 0b0100000
        self.P14 = P1 & 0b0010000

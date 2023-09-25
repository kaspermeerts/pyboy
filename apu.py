NR10,  NR11, NR12, NR13, NR14, \
_NR20, NR21, NR22, NR23, NR24, \
NR30,  NR31, NR32, NR33, NR34, \
_NR40, NR41, NR42, NR43, NR44, \
NR50,  NR51, NR52 = range(0x10, 0x27)

READ_MASK = [0x80, 0x3F, 0x00, 0xFF, 0xBF, # Square 1
             0xFF, 0x3F, 0x00, 0xFF, 0xBF, # Square 2
             0x7F, 0xFF, 0x9F, 0xFF, 0xBF, # Wave
             0xFF, 0xFF, 0x00, 0x00, 0xBF, # Noise
             0x00, 0x00, 0x70,             # Control/Status
             0xFF, 0xFF, 0xFF, 0xFF, 0xFF, # Unused
             0xFF, 0xFF, 0xFF, 0xFF,
             0x00, 0x00, 0x00, 0x00, 0x00, # Wave Table
             0x00, 0x00, 0x00, 0x00, 0x00,
             0x00, 0x00, 0x00, 0x00, 0x00,
             0x00]

assert len(READ_MASK) == 0x30

class APU:
    def __init__(self):
        self.sample_buffer = bytearray(96000*10)
        self.sample_count = 0
        self.subsample_count = 0

        self.left_volume = 0
        self.right_volume = 0
        self.left_enables = [False]*4
        self.right_enables = [False]*4
        self.power_control = False

        # A copy of the sound registers, the unused registers and the wave table
        self._io = bytearray(0x40)

        # 1048576 Hz / 256 Hz
        # TODO I think this is clocked by the DIV register?
        self.frame_timer = 4096
        self.frame_count = 0

        self.square1 = Square1()
        self.square2 = Square2()
        self.wave = Wave()
        self.noise = Noise()

    def read_io(self, address):
        assert 0x10 <= address < 0x40

        if address == NR52:
            out = 0
            if self.square1.enabled: out |= 0b0001
            if self.square2.enabled: out |= 0b0010
            if self.wave.enabled:    out |= 0b0100
            if self.noise.enabled:   out |= 0b1000
            if self.power_control:   out |= 0b10000000
            return out | READ_MASK[NR52 - 0x10]
        else:
            return self._io[address] | READ_MASK[address - 0x10]

    def write_io(self, address, value):
        assert 0x10 <= address < 0x40

        # Store a copy for reading
        self._io[address] = value

        if 0x30 <= address < 0x40:
            self.wave.write_wave(address, value)
        elif address == NR50:
            self.right_volume = (value >> 4) & 0b111
            self.left_volume = value & 0b111
        elif address == NR51:
            self.right_enables[3] = value & 0b10000000 == 0b10000000
            self.right_enables[2] = value & 0b01000000 == 0b01000000
            self.right_enables[1] = value & 0b00100000 == 0b00100000
            self.right_enables[0] = value & 0b00010000 == 0b00010000
            self.left_enables[3]  = value & 0b00001000 == 0b00001000
            self.left_enables[2]  = value & 0b00000100 == 0b00000100
            self.left_enables[1]  = value & 0b00000010 == 0b00000010
            self.left_enables[0]  = value & 0b00000001 == 0b00000001
        elif address == NR52:
            if value & 0b10000000 != 0b10000000:
                # APU turned off, zero all registers
                for i in range(NR10, NR52):
                    self.write_io(i, 0)
                    self.power_control = False
            elif not self.power_control:
                self.power_control = True
                self.frame_count = 0
        else:

            # Writes to the remaining registers are ignored when the APU is
            # powered down
            if not self.power_control:
                self._io[address] = 0
                return

            if NR10 <= address <= NR14:
                self.square1.write(address, value)
            elif _NR20 <= address <= NR24:
                self.square2.write(address, value)
            elif NR30 <= address <= NR34:
                self.wave.write(address, value)
            elif _NR40 <= address <= NR44:
                self.noise.write(address, value)
            else:
                print("Write to unused sound register {:02x}".format(address))

    def step(self, cycles):
        # XXX The actual APU is synchronized with the CPU and thus ticks at 
        # 4MiHz. Because all instructions take a number of cycles divisible by
        # four, I'm going to clock this at 1MiHz. Most things divide neatly
        # by four, but the wave channel loses one bit of precision in its 
        # frequency. Ah well...
        assert cycles % 4 == 0
        ticks = cycles // 4

        if not self.power_control:
            return

        for _ in range(ticks):
            self.tick()

    def tick(self):
        if self.square1.enabled:
            channel_tick(self.square1)
        if self.square2.enabled:
            channel_tick(self.square2)
        if self.wave.enabled:
            channel_tick(self.wave)
        if self.noise.enabled:
            channel_tick(self.noise)

        self.subsample_count += 1
        if self.subsample_count == 22: # ~ 1 MiHz / 48 kHz
            self.subsample_count = 0
            self.store_sample()

        # Frame sequencer
        self.frame_timer -= 1
        if self.frame_timer == 0:
            # Every frame, so 256 Hz
            length_clock(self.square1)
            length_clock(self.square2)
            length_clock(self.wave)
            length_clock(self.noise)

            # Every 4 frames, so 64 Hz
            if self.frame_count == 3:
                volume_clock(self.square1)
                volume_clock(self.square2)
                volume_clock(self.noise)

            # Every 2 frames, so 128 Hz
            #if self.frame_count == 1 or self.frame_count == 3:
            #    self.square1.sweep_clock()

            self.frame_count += 1
            if self.frame_count == 4:
                self.frame_count = 0

            self.frame_timer = 4096

    def store_sample(self):
        out_left, out_right = 0, 0
        if self.square1.enabled and self.left_enables[0]:
            out_left += self.square1.output_volume
        if self.square2.enabled and self.left_enables[1]:
            out_left += self.square2.output_volume
        if self.wave.enabled and self.left_enables[2]:
            out_left += self.wave.output_volume
        if self.noise.enabled and self.left_enables[3]:
            pass #out_left += self.noise.output_volume

        if self.square1.enabled and self.right_enables[0]:
            out_right += self.square1.output_volume
        if self.square2.enabled and self.right_enables[1]:
            out_right += self.square2.output_volume
        if self.wave.enabled and self.right_enables[2]:
            out_right += self.wave.output_volume
        if self.noise.enabled and self.right_enables[3]:
            pass #out_right += self.noise.output_volume

        out_left *= 17
        if out_left > 0xFF: out_left = 0xFF
        out_right *= 17
        if out_right > 0xFF: out_right = 0xFF

        self.sample_buffer[self.sample_count] = out_left
        self.sample_buffer[self.sample_count + 1] = out_right
        self.sample_count += 0

    def play_XXX(self):
        import sounddevice as sd
        ostr = sd.RawOutputStream(channels=2, samplerate=48000, dtype="uint8")
        ostr.start()
        ostr.write(self.sample_buffer)
        ostr.stop()

def channel_tick(channel):
    channel.period_timer -= 1
    if channel.period_timer > 0:
        return
    channel.period_timer = 2048 - channel.period_timer_start

    channel.clock()

def volume_clock(channel):
    channel.volume_timer -= 1
    if channel.volume_timer > 0:
        return
    channel.volume_timer = channel.volume_timer_start or 8

    if channel.volume_enabled and channel.volume_timer_start != 0:
        if channel.volume_increase:
            if channel.volume < 15:
                channel.volume += 1
            else:
                channel.volume_enabled = False
        else:
            if channel.volume > 0:
                channel.volume -= 1
            else:
                channel.volume_enabled = False

def length_clock(channel):
    if not channel.length_enabled:
        return

    channel.length -= 1
    if channel.length == 0:
        channel.enabled = False

# TODO Use actual duties. Not worth it?
# SQUARE_DUTIES = [[0,0,0,0,0,0,0,1],[1,0,0,0,0,0,0,1],[1,0,0,0,0,1,1,1],[0,1,1,1,1,1,1,0]]
SQUARE_DUTIES = [1, 2, 4, 6]

class Square1:
    def __init__(self):
        self.enabled = False

        # TODO How does this work?
        # Does it count down from 2048 to start, or count up from start to 2048
        # (or equivalent like down from 2048 - start to 0)
        # Might give slightly different behaviors when the freq changes
        # For consistency I'm making all my timers count down
        self.period_timer = 0
        self.period_timer_start = 0
        self.duty = 4
        self.phase = 0

        self.length = 0
        self.length_enabled = False

        self.sweep_enabled = False
        self.sweep_shadow = 0
        self.sweep_timer_start = 0
        self.sweep_timer = 0
        self.sweep_increase = True
        self.sweep_shift = 0

        self.volume = 0
        self.volume_enabled = True
        self.volume_start = 0
        self.volume_increase = True
        self.volume_timer_start = 0
        self.volume_timer = 0

        self.output_volume = 0


    def write(self, address, value):
        assert NR10 <= address <= NR14

        if address == NR10:
            self.sweep_timer_start = value >> 4 or 8
            self.sweep_increase = (value & 0b1000) == 0
            self.sweep_shift = value & 0b111
        elif address == NR11:
            self.duty = SQUARE_DUTIES[value >> 6]
            self.length = 64 - (value & 0b111111)
        elif address == NR12:
            self.volume_start = value >> 4
            self.volume_increase = (value & 0b1000) == 0b1000
            self.volume_timer_start = value & 0b111 or 8
        elif address == NR13:
            self.period_timer_start = self.period_timer_start & 0b11100000000 | value
        elif address == NR14:
            self.period_timer_start = self.period_timer_start & 0b00011111111 | ((value & 0b111) << 8)
            self.length_enabled = value & 0b01000000
            if value & 0b10000000:
                self.trigger()

    def trigger(self):
        self.enabled = True

        self.length = self.length or 64
        self.period_timer = 2048 - self.period_timer_start
        self.volume_timer = self.volume_timer_start
        self.volume = self.volume_start
        self.volume_enabled = True

        self.sweep_shadow = self.period_timer_start
        self.sweep_timer = self.sweep_timer_start
        self.sweep_enabled = self.sweep_timer != 0 or self.sweep_shift != 0
        if self.sweep_shift != 0:
            self.sweep_calculate()


    def sweep_clock(self):
        if not self.sweep_enabled:
            return

        self.sweep_timer -= 1
        if self.sweep_timer != 0:
            return
        self.sweep_timer = self.sweep_timer_start

        new_period = self.sweep_calculate()
        if new_period < 2048 and self.sweep_shift != 0:
            self.sweep_shadow = new_period
            self.period_timer_start = new_period
        self.sweep_calculate()


    def sweep_calculate(self):
        period_offset = self.sweep_shadow >> self.sweep_shift
        if self.sweep_increase:
            new_period = self.sweep_shadow + period_offset
        else:
            new_period = self.sweep_shadow - period_offset

        new_period &= 0xFFFF

        if new_period >= 2048:
            self.enabled = False

        return new_period

    def clock(self):
        self.phase = (self.phase + 1) & 0b111

        if self.phase < self.duty:
            self.output_volume = self.volume
        else:
            self.output_volume = 0

class Square2:
    def __init__(self):
        self.enabled = False

        self.period_timer = 0
        self.period_timer_start = 0
        self.duty = 4
        self.phase = 0

        self.length = 0
        self.length_enabled = False

        self.volume = 0
        self.volume_enabled = True
        self.volume_start = 0
        self.volume_increase = True
        self.volume_timer_start = 0
        self.volume_timer = 0

        self.output_volume = 0

    def write(self, address, value):
        assert _NR20 <= address <= NR24

        if address == _NR20:
            pass
        elif address == NR21:
            self.duty = SQUARE_DUTIES[value >> 6]
            self.length = 64 - (value & 0b111111)
        elif address == NR22:
            self.volume_start = value >> 4
            self.volume_increase = (value & 0b1000) == 0b1000
            self.volume_timer_start = value & 0b111
        elif address == NR23:
            self.period_timer_start = self.period_timer_start & 0b11100000000 | value
        elif address == NR24:
            self.period_timer_start = self.period_timer_start & 0b00011111111 | ((value & 0b111) << 8)
            self.length_enabled = value & 0b01000000
            if value & 0b10000000:
                self.trigger()

    def trigger(self):
        self.enabled = True

        self.length = self.length or 64
        self.period_timer = 2048 - self.period_timer_start
        self.volume_timer = self.volume_timer_start
        self.volume = self.volume_start
        self.volume_enabled = True

    def clock(self):
        self.phase = (self.phase + 1) & 0b111

        if self.phase < self.duty:
            self.output_volume = self.volume
        else:
            self.output_volume = 0

VOLUME_SHIFTS = [4, 0, 1, 2]
class Wave:
    def __init__(self):
        self.enabled = False

        self.period_timer = 0
        self.period_timer_start = 0
        self.period_timer_real = 0
        # XXX The wave channel ticks twice as fast as the other. However, I
        # can't afford to tick the APU more often :/ So I just divide its 
        # period by two

        self.length = 0
        self.length_enabled = False

        self.volume = 0
        self.volume_shift = 4

        self.output_volume = 0

        self.wave_ram = bytearray(32)
        self.wave_position = 0

    def write(self, address, value):
        assert NR30 <= address <= NR34

        if address == NR30:
            self.enabled = value & 0b10000000
        elif address == NR31:
            self.length = 256 - value
        elif address == NR32:
            self.volume_shift = VOLUME_SHIFTS[(value >> 5) & 0b11]
        elif address == NR33:
            self.period_timer_real = self.period_timer_real & 0b11100000000 | value
            self.period_timer_start = 1024 + self.period_timer_real // 2
        elif address == NR34:
            self.period_timer_real = self.period_timer_real & 0b00011111111 | ((value & 0b111) << 8)
            self.period_timer_start = 1024 + self.period_timer_real // 2
            self.length_enabled = value & 0b01000000
            if value & 0b10000000:
                self.trigger()

    def write_wave(self, address, value):
        assert 0x30 <= address < 0x40

        sample_num = (address - 0x30) * 2
        self.wave_ram[sample_num    ] = value >> 4
        self.wave_ram[sample_num + 1] = value & 0b1111

    def trigger(self):
        self.enabled = True

        self.length = self.length or 256
        self.period_timer = 2048 - self.period_timer_start

    def clock(self):
        self.wave_position = (self.wave_position + 1) & 0x20
        self.output_volume = self.wave_ram[self.wave_position] >> self.volume_shift


DIVISORS = [2, 4, 8, 12, 16, 20, 24, 28]

class Noise:
    def __init__(self):
        self.enabled = False
        self.period_timer = 0
        self.period_timer_start = 0
        self.divisor = 2
        self.clock_shift = 0

        self.length = 0
        self.length_enabled = False

        self.volume = 0
        self.volume_enabled = True
        self.volume_start = 0
        self.volume_increase = True
        self.volume_timer_start = 0
        self.volume_timer = 0

        self.lfsr = 0x7F
        self.width_mode_7 = False
        self.output_volume = 0


    def write(self, address, value):
        assert _NR40 <= address <= NR44

        if address == _NR40:
            pass
        elif address == NR41:
            self.length = 64 - (value & 0b111111)
        elif address == NR42:
            self.volume_start = value >> 4
            self.volume_increase = (value & 0b1000) == 0b1000
            self.volume_timer_start = value & 0b111
        elif address == NR43:
            self.clock_shift = value >> 4
            self.width_mode_7 = value & 0b1000
            self.divisor = DIVISORS[value & 0b111]
            self.period_timer_start = self.divisor << self.clock_shift
        elif address == NR44:
            self.length_enabled = value & 0b01000000
            if value & 0b10000000:
                self.trigger()

    def trigger(self):
        self.enabled = True

        self.length = self.length or 64
        self.period_timer = 2048 - self.period_timer_start
        self.volume_timer = self.volume_timer_start
        self.volume = self.volume_start
        self.volume_enabled = True

        self.lfsr = 0x7F

    def clock(self):
        tap0 = self.lfsr & 0b1
        tap1 = (self.lfsr >> 1) & 0b1
        self.lfsr = (self.lfsr >> 1) | ((tap0 ^ tap1) << 14)
        if self.width_mode_7:
            self.lfsr |= (tap0 ^ tap1) << 6

        if not tap1:
            self.output_volume = self.volume
        else:
            self.output_volume = 0



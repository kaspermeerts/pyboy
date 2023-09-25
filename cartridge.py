import abc
import os.path

try:
    with open("DMG_ROM.bin", "rb") as f:
        BOOTROM = f.read()
    if len(BOOTROM) != 0x100:
        raise ValueError("Bootrom in file DMG_ROM.bin has wrong length")
except (OSError, ValueError):
    print("Couldn't load bootrom, using replacement")
    BOOTROM = bytearray(0x100)
    BOOTROM[0x00] = 0x31 # LD SP, $fffe
    BOOTROM[0x01] = 0xfe
    BOOTROM[0x02] = 0xff
    BOOTROM[0x03] = 0x3e # LD A, $91
    BOOTROM[0x04] = 0x91
    BOOTROM[0x05] = 0xe0 # LDH ($40), A
    BOOTROM[0x06] = 0x40
    BOOTROM[0xFC] = 0x3e # LD A, $01
    BOOTROM[0xFD] = 0x01
    BOOTROM[0xFE] = 0xe0 # LDH ($50), A
    BOOTROM[0xFF] = 0x50

assert len(BOOTROM) == 256

class Cartridge:
    def __init__(self):
        self.name = None
        self.mbc = None

        self.rom_size = 0  # In 16 kB banks
        self.rom_banks = None
        self.rom_bank0 = None  # The lower bank is always bank 0
        self.rom_bankx = None  # The upper bank can be switched

        self.ram_size = 0  # In 8 kB bank
        self.ram_banks = None
        self.ram_bank = None

        self.ram = False
        self.rtc = False
        self.battery = False
        self.savefile = None

    def __str__(self):
        if self.name is None:
            return "No ROM loaded\n"
        out  = "Name: {}\n".format(self.name)
        out += "ROM banks: {}\n".format(self.rom_size)
        out += "RAM banks: {}\n".format(self.ram_size)
        out += "RAM: " + ("Yes" if self.ram else "No") + "\n"
        out += "RTC: " + ("Yes" if self.rtc else "No") + "\n"
        out += "Battery: " + ("Yes" if self.battery else "No") + "\n"

        return out

    def load_rom(self, filename):
        try:
            with open(filename, "rb") as rom_file:
                contents = rom_file.read()
        except OSError:
            raise ValueError("Could not open file {}".format(filename))

        if len(contents) < 0x4000:
            raise ValueError("Truncated ROM file")

        self.name = contents[0x134:0x143].decode('ascii').rstrip('\x00')

        cgb_only = contents[0x143] == 0xC0
        if cgb_only:
            raise NotImplementedError("GBC not supported")

        header_checksum = contents[0x14D]
        header_sum = -sum([byte + 1 for byte in contents[0x134:0x14D]]) & 0xFF
        if header_checksum != header_sum:
            raise ValueError("Header checksum doesn't match")

        # Uncomment to enable check?
        # global_checksum = (contents[0x14E] << 8) + contents[0x14F]
        # global_sum = sum([byte for byte in contents])
        # global_sum = (global_sum - contents[0x14E] - contents[0x14F]) & 0xFFFF
        # if global_checksum != global_sum:
        #     raise ValueError("Global checksum doesn't match")

        cart_type = contents[0x147]
        if cart_type in (0x00, 0x08, 0x09):
            self.mbc = MBCNone()
        elif cart_type in (0x01, 0x02, 0x03):
            self.mbc = MBC1()
        elif cart_type in (0x05, 0x06):
             self.mbc = MBC2()
        elif cart_type in (0x0F, 0x10, 0x11, 0x12, 0x13):
            self.mbc = MBC3()
        #elif cart_type in (0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E):
        #    self.mbc = MBC3()
        #elif cart_type == 0x20:
        #    self.mbc = MBC6()
        #elif cart_type == 0x22:
        #    self.mbc = MBC7()
        else:
            raise ValueError("Unknown cartridge type: 0x{:x}".format(cart_type))

        if cart_type in (0x02, 0x03, 0x05, 0x06, 0x08, 0x09, 0x0C, 0x0D, 0x10, 0x12, 0x13, 0x1A, 0x1B, 0x1D, 0x1E, 0x20, 0x22):
            self.ram = True
        if cart_type in (0x03, 0x06, 0x09, 0x0D, 0x0F, 0x10, 0x13, 0x1B, 0x1E):
            self.battery = True
        if cart_type in (0x0F, 0x10):
            self.rtc = True

        # ROM
        rom_size = contents[0x148]
        if rom_size <= 8:
            self.rom_size = 2 << rom_size
        else:
            raise ValueError("Invalid ROM size: {}".format(rom_size))

        if len(contents) != self.rom_size * 0x4000:
            raise ValueError("ROM size doesn't match header")

        self.rom_banks = [None] * self.rom_size
        for i in range(self.rom_size):
            self.rom_banks[i] = contents[0x4000*i: 0x4000*(i+1)]

        # RAM
        ram_size = contents[0x149]
        if ram_size == 0x00:
            self.ram_size = 0
        elif ram_size == 0x01:
            self.ram_size = 1 # Actually just 2 kB, quarter of a bank :/
        elif ram_size == 0x02:
            self.ram_size = 1
        elif ram_size == 0x03:
            self.ram_size = 4
        elif ram_size == 0x04:
            self.ram_size = 16
        elif ram_size == 0x05:
            self.ram_size = 8
        else:
            raise ValueError("Unknown RAM size")

        # MBC2 cartridges advertise no RAM in the header, but they do have
        # 512 nibbles of internal RAM. This isn't properly emulated
        if cart_type in (0x05, 0x06):
            self.ram_size = 1

        assert self.ram == (self.ram_size != 0)

        self.ram_banks = [None] * self.ram_size
        for i in range(self.ram_size):
            self.ram_banks[i] = bytearray(8192)

        if self.ram_size != 0:
            self.ram_bank = self.ram_banks[0]

        self.rom_bank0 = self.rom_banks[0]
        self.switch_banks()

        # Temporarily overwrite the first 100 bytes with the boot ROM
        # It's a bit of a hack, but I want to avoid an extra branch on every
        # single read
        self.rom_bank0 = bytearray(self.rom_bank0)
        self.rom_bank0[0x0000:0x0100] = BOOTROM

        root, ext = os.path.splitext(filename)
        if ext == ".gb":
            self.savefile = root + ".sav"

        self.load_sram()

    def load_sram(self):
        if not self.battery or not self.savefile:
            return

        try:
            with open(self.savefile, "rb") as fd:
                save = fd.read()
        except OSError:
            print("Could not load SRAM from {}".format(self.savefile))
            return

        if len(save) != self.ram_size * 8192:
            print("Savefile {} has an invalid size. Possibly corrupted".format(self.savefile))
            return

        for i in range(self.ram_size):
            # Copy into, don't replace
            self.ram_banks[i][:] = save[i*8192:(i+1)*8192]

    def save_sram(self):
        if not self.battery or not self.savefile:
            return

        try:
            with open(self.savefile, "wb") as fd:
                for i in range(self.ram_size):
                    fd.write(self.ram_banks[i])
        except OSError:
            print("Could not save SRAM to {}".format(self.savefile))

    def disable_bootrom(self):
        self.rom_bank0 = self.rom_banks[0]

    def write_rom(self, address, value):
        self.mbc.write(address, value)
        self.switch_banks()

    def switch_banks(self):
        if self.mbc.active_rom_bank < self.rom_size:
            self.rom_bankx = self.rom_banks[self.mbc.active_rom_bank]
        else:
            print("Tried to map unavailable ROM bank 0x{:x}".format(self.mbc.active_rom_bank))
        if self.mbc.ram_enable:
            if self.mbc.active_ram_bank < self.ram_size:
                self.ram_bank = self.ram_banks[self.mbc.active_ram_bank]
            else:
                print("Tried to map unavailable RAM bank 0x{:x}".format(self.mbc.active_ram_bank))
        else:
            self.ram_bank = None

    def read_ram(self, address):
        assert address < 0x2000

        if self.ram_bank:
            return self.ram_bank[address]
        else:
            return 0xFF

    def write_ram(self, address, value):
        assert address < 0x2000

        if self.ram_bank:
            self.ram_bank[address] = value
        else:
            # Many games (like Super Mario Land) write to the RAM even when it's
            # disabled or no RAM is even present on the cartridge. Might be a
            # bug, or a remnant from development
            pass

class MBC(metaclass=abc.ABCMeta):
    @property
    @abc.abstractmethod
    def active_rom_bank(self):
        pass

    @property
    @abc.abstractmethod
    def active_ram_bank(self):
        pass

    @property
    @abc.abstractmethod
    def ram_enable(self):
        pass

    @abc.abstractmethod
    def write(self, address, value):
        pass

class MBCNone(MBC):
    @property
    def active_rom_bank(self):
        return 1

    @property
    def active_ram_bank(self):
        return 0

    @property
    def ram_enable(self):
        return False

    def write(self, address, value):
        pass

class MBC1(MBC):
    def __init__(self):
        self._ram_enable = False
        self._reg_lo = 0b00001
        self._reg_hi = 0b00
        self._romram_mode = 0b0

    @property
    def active_rom_bank(self):
        if self._romram_mode:
            return (self._reg_hi << 5) | self._reg_lo
        else:
            return self._reg_lo

    @property
    def active_ram_bank(self):
        if self._romram_mode:
            return 0
        else:
            return self._reg_hi

    @property
    def ram_enable(self):
        return self._ram_enable

    def write(self, address, value):
        # The lower 13 bits of the address bus aren't even connected
        address >>= 13

        if   address == 0: # 0x0000 - 0x2000 RAM enable
            self._ram_enable = (value & 0x0F) == 0x0A
        elif address == 1: # 0x2000 - 0x4000 ROM bank select lo
            value &= 0b00011111
            if value == 0:
                value = 1
            self._reg_lo = value
        elif address == 2: # 0x4000 - 0x6000 RAM/ROM bank select hi
            self._reg_hi = value & 0b00000011
        elif address == 3: # 0x6000 - 0x8000 ROM/RAM mode
            self._romram_mode = value & 0b1
        else:
            raise ValueError

class MBC2(MBC):
    def __init__(self):
        self._ram_enable = False
        self._rom_bank = 1
        self._ram_bank = 0

    @property
    def active_rom_bank(self):
        return self._rom_bank

    @property
    def active_ram_bank(self):
        return self._ram_bank

    @property
    def ram_enable(self):
        return self._ram_enable

    def write(self, address, value):

        if   0x0000 <= address < 0x2000: # 0x0000 - 0x2000 RAM enable
            if (address & 0x0100) == 0x0000:
                self._ram_enable = (value & 0x0F) == 0x0A
                print(self._ram_enable)
        elif 0x2000 <= address < 0x4000: # 0x2000 - 0x4000 ROM bank select
            if address & 0x0100 == 0x0100:
                value &= 0x0F
                if value == 0:
                    value = 1
                self._rom_bank = value

class MBC3(MBC):
    def __init__(self):
        self._ram_enable = False
        self._rom_bank = 1
        self._ram_bank = 0

    @property
    def active_rom_bank(self):
        return self._rom_bank

    @property
    def active_ram_bank(self):
        return self._ram_bank

    @property
    def ram_enable(self):
        return self._ram_enable

    def write(self, address, value):

        address >>= 13
        if   address == 0: # 0x0000 - 0x2000 RAM enable
            self._ram_enable = (value & 0x0F) == 0x0A
        elif address == 1: # 0x2000 - 0x4000 ROM bank select
            value &= 0b01111111
            if value == 0:
                value = 1
            self._rom_bank = value
        elif address == 2: # 0x4000 - 0x6000 RAM/RTC bank select
            if 0x0 <= value < 0x04:
                self._ram_bank = value
            elif 0x08 <= value < 0x0D:
                pass # TODO RTC
        elif address == 3: # 0x6000 - 0x8000 RTC latch
            pass # TODO Implement :p
        else:
            raise ValueError

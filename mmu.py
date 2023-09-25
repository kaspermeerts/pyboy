import joypad
import timer
import cpu
import serial

DMA = 0x46

class MMU:
    def __init__(self):
        self.cpu = None
        self.gpu = None
        self.apu = None
        self.cartridge = None
        self.joypad = None
        self.timer = None
        self.serial = None

        # The MMU keeps track of the non-cartridge RAM
        self.ram = bytearray(0x2000)

        self.dma_reg = 0x00

    def read(self, address):
        assert 0 <= address < 0x10000, "Address out of range: 0x%x" % address

        # Documentation is incomplete as usual, but the lowest 256 bytes 
        # containing the interrupt vectors are also in cartridge ROM
        if   0x0000 <= address < 0x4000: # ROM
            return self.cartridge.rom_bank0[address]
        elif 0x4000 <= address < 0x8000:
            return self.cartridge.rom_bankx[address - 0x4000]
        elif 0x8000 <= address < 0xA000: # VRAM
            return self.gpu.vram[address - 0x8000]
        elif 0xA000 <= address < 0xC000: # Cartridge RAM
            return self.cartridge.read_ram(address - 0xA000)
        elif 0xC000 <= address < 0xE000: # Work RAM
            return self.ram[address - 0xC000]
        elif 0xE000 <= address < 0xFE00: # Shadow RAM
            return self.ram[address - 0xE000]
        elif 0xFE00 <= address < 0xFEA0: # OAM
            return self.gpu.oam[address - 0xFE00]
        elif 0xFEA0 <= address < 0xFF00: # Unused Memory Area
            return 0xFF
        elif 0xFF00 <= address < 0xFF80: # IO
            return self.read_io(address - 0xFF00)
        elif 0xFF80 <= address < 0xFFFF: # Zero page
            return self.cpu.zram[address - 0xFF80]
        elif 0xFFFF == address:
            return self.cpu.IE

    def read_io(self, address):
        assert 0x0 <= address < 0x80

        if address == joypad.P1:
            return self.joypad.get_P1()
        elif serial.SB <= address <= serial.SC:
            return self.serial.read_io(address)
        elif timer.DIV <= address <= timer.TAC:
            return self.timer.read_io(address)
        elif address == cpu.IF:
            return self.cpu.IF
        elif 0x10 <= address < 0x40:
            return self.apu.read_io(address)
        elif address == DMA:
            return self.dma_reg
        elif 0x40 <= address < 0x4C:
            return self.gpu.read_io(address)
        else:
            #print("Read from unused register {:02x}".format(address))
            return 0xFF

    def read_word(self, address):
        return (self.read(address + 1) << 8) | self.read(address)

    def write(self, address, value):
        assert 0 <= address < 65536, "Address out of range: 0x%x" % address
        assert 0 <= value < 0x100

        if   0x0000 <= address < 0x8000: # ROM, will be processed by MBC
            self.cartridge.write_rom(address, value)
        elif 0x8000 <= address < 0xA000: # VRAM
            self.gpu.write_vram(address - 0x8000, value)
        elif 0xA000 <= address < 0xC000: # Cartridge RAM
            self.cartridge.write_ram(address - 0xA000, value)
        elif 0xC000 <= address < 0xE000: # Work RAM
            self.ram[address - 0xC000] = value
        elif 0xE000 <= address < 0xFE00: # Shadow RAM
            self.ram[address - 0xE000] = value
        elif 0xFE00 <= address < 0xFEA0: # OAM
            self.gpu.oam[address - 0xFE00] = value
        elif 0xFEA0 <= address < 0xFF00: # Unused Memory Area
            pass
        elif 0xFF00 <= address < 0xFF80: # IO registers
            self.write_io(address - 0xFF00, value)
        elif 0xFF80 <= address < 0xFFFF: # Zero page
            self.cpu.zram[address - 0xFF80] = value
        elif 0xFFFF == address:
            self.cpu.IE = value

    def write_io(self, address, value):
        assert 0x0 <= address < 0x80
        assert 0x0 <= value < 0x100

        if address == joypad.P1: # 0x00
            self.joypad.set_P1(value)
        elif serial.SB <= address <= serial.SC: # 0x01 - 0x02
            self.serial.write_io(address, value)
        elif timer.DIV <= address <= timer.TAC: # 0x04 - 0x07
            self.timer.write_io(address, value)
        elif address == cpu.IF: # 0x0F
            self.cpu.IF = value
        elif 0x10 <= address < 0x40:
            self.apu.write_io(address, value)
        elif address == DMA: # 0x46
            self.dma_reg = value
            self.do_dma()
        elif 0x40 <= address < 0x4C:
            self.gpu.write_io(address, value)
        elif address == 0x50:
            self.cartridge.disable_bootrom()
        else:
            pass
            #print("Write ignored to unused register {:02x}: {:02x}".format(address, value))

    def write_word(self, address, value):
        assert 0 <= value < 0x10000

        lo, hi = value & 0x00FF, (value & 0xFF00) >> 8
        self.write(address    , lo)
        self.write(address + 1, hi)

    def do_dma(self):
        assert 0x80 <= self.dma_reg < 0xE0
        addr_hi = self.dma_reg << 8

        if   0x8000 <= addr_hi < 0xA000: # VRAM
            address = addr_hi - 0x8000
            self.gpu.oam = self.gpu.vram[address: address + 160]
        elif 0xA000 <= addr_hi < 0xC000: # Cartridge RAM
            address = addr_hi - 0xA000
            if self.cartridge.ram_bank: # Duplicate of cartridge.read_ram
                self.gpu.oam = self.cartridge.ram_bank[address: address + 160]
        elif 0xC000 <= addr_hi < 0xE000: # Work RAM
            address = addr_hi - 0xC000
            self.gpu.oam = self.ram[address: address + 160]
        else:
            print("Tried to do DMA from unsupported address {:02x}00".format(addr_hi))

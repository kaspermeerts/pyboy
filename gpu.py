import cpu

LCDC, STAT, SCY, SCX, LY, LYC, _DMA, BGP, OBP0, OBP1, WY, WX = range(0x40, 0x4C)
LCDC_ENABLE              = 0b10000000
LCDC_WIN_TILE_TABLE      = 0b01000000
LCDC_WINDOW_ENABLE       = 0b00100000
LCDC_BG_WIN_TILE_AREA    = 0b00010000
LCDC_BG_TILE_TABLE       = 0b00001000
LCDC_SPRITE_SIZE         = 0b00000100
LCDC_SPRITE_ENABLE       = 0b00000010
LCDC_BACKGROUND_ENABLE   = 0b00000001

STAT_LYC_COMPARE_IRQ     = 0b01000000
STAT_CHECK_MODE2         = 0b00100000
STAT_CHECK_MODE1         = 0b00010000
STAT_CHECK_MODE0         = 0b00001000
STAT_LYC_COMPARE         = 0b00000100

GRAY_SHADES = [0xFF, 0xAA, 0x55, 0x00]

class GPU:
    def __init__(self, mmu, cpu):
        self.cpu = cpu

        self.frames = 0

        self.vram = bytearray(8192)
        self.oam = bytearray(40 * 4)
        self._io = bytearray(0x4C)

        self.mode_cycles = 0
        self._io[LY] = 0

        self.bg_palette = [0]*4
        self.obj_palette0 = [0]*4
        self.obj_palette1 = [0]*4

        # 384 8x8 tiles in two overlapping banks
        self.tile_cache0 = bytearray(64 * 256)
        self.tile_cache1 = bytearray(64 * 256)

        self.tile_table0 = bytearray(1024)
        self.tile_table1 = bytearray(1024)

        self.background_transparent = [True] * 160 * 144
        self.pixel_buffers = [[0] * 160 * 144] * 2
        self.active_buffer = 0
        self.pixel_buffer = self.pixel_buffers[self.active_buffer]

        self.gray_buffer = [0x7F] * 160 * 144

        self.frame_buffer = self.gray_buffer
        self.frame_done = False

    def __str__(self):
        out = ""
        out += "LCDC: {:08b}\nSTAT: {:08b}\nSCY:  {:x}\nSCX:  {:x}\n"
        out += "LY:   {:x}  \nLYC:  {:x}\n"
        out = out.format(self._io[LCDC], self._io[STAT], self._io[SCY], self._io[SCX], self._io[LY], self._io[LYC])
        #LCDC, STAT, SCY, SCX, LY, LYC, _DMA, BGP, OBP0, OBP1, WY, WX = range(0x40, 0x4C)
        return out

    def read_io(self, address):
        assert 0x40 <= address < 0x4C
        return self._io[address]

    def write_io(self, address, value):
        assert 0x40 <= address < 0x4C

        if address == STAT: # The lowest three bits of STAT are read-only
            mask = 0b111
            self._io[STAT] = (value & ~mask) | (self._io[STAT] & mask)
        elif address == LCDC:
            # Enable or disable LCD controller
            if (self._io[LCDC] ^ value) & LCDC_ENABLE:
                self._io[LY] = 0
                self._io[STAT] &= ~0b11
                if value & LCDC_ENABLE:
                    self.set_mode(2)
                else:
                    self.frame_done = True
                    self.frame_buffer = self.gray_buffer

            self._io[LCDC] = value
        elif address == LY:
            self._io[LY] = 0 # ?
        elif address == BGP or address == OBP0 or address == OBP1:
            self._io[address] = value
            self.update_palettes()
        else:
            self._io[address] = value

    def write_vram(self, address, value):
        self.vram[address] = value

        # Update the tile cache
        if address < 0x1800:
            # Lines are two bytes
            line_addr = address & 0xFFFE
            byte1 = self.vram[line_addr    ]
            byte2 = self.vram[line_addr + 1] << 1
            # Tiles are 16 bytes in VRAM and 64 bytes in the tile cache
            index = line_addr * 4
            for column in range(8):
                n = (7 - column)
                color = ((byte1 >> n) & 0x1) | ((byte2 >> n) & 0x2)
                if index < 128 * 64:
                    self.tile_cache0[index + column] = color
                elif index < 256 * 64:
                    self.tile_cache0[index + column] = color
                    self.tile_cache1[index + column] = color
                elif index < 384 * 64:
                    self.tile_cache1[index - 256*64 + column] = color
        elif address < 0x1C00:
            self.tile_table0[address - 0x1800] = value
        else:
            self.tile_table1[address - 0x1C00] = value

    def dump_vram(self):
        from PIL import Image
        img = Image.new("L", (16 * 8, 3 * 8 * 8))

        for ix in range(16):
            for iy in range(3 * 8):
                i = iy*16 + ix
                for xx in range(8):
                    for yy in range(8):
                        ii = i*64 + yy*8 + xx
                        x = ix*8 + xx
                        y = iy*8 + yy
                        if iy < 2*8:
                            pi = self.tile_cache0[ii]
                        else:
                            pi = self.tile_cache1[ii - 64*256]
                        img.putpixel((x,y), self.bg_palette[pi])
        img.save("vram.png")

    def get_mode(self):
        return self._io[STAT] & 0b11

    def set_mode(self, mode):
        assert 0 <= mode <= 3
        MODE_CYCLES = [204, 456, 80, 172]
        self._io[STAT] = (self._io[STAT] & 0b11111100) | mode
        # This interrupt might be buggy in the hardware. I wonder if it's used
        # that often at all
        self.mode_cycles += MODE_CYCLES[mode]
        if (mode == 0 and self._io[STAT] & STAT_CHECK_MODE0 or
            mode == 1 and self._io[STAT] & STAT_CHECK_MODE1 and self._io[LY] == 144 or
            mode == 2 and self._io[STAT] & STAT_CHECK_MODE2):
            self.cpu.interrupt_request(cpu.IRQ_STAT)

    def step(self, cycles):
        if not self._io[LCDC] & LCDC_ENABLE:
            return

        self.mode_cycles -= cycles
        if self.mode_cycles > 0:
            return

        # The LCD controller cycles between 4 modes
        # Mode 0: HBlank         80-204 cycles
        # Mode 1: VBlank         456 cycles
        # Mode 2: Reading OAM    80 cycles
        # Mode 3: Data transfer  172-296 cycles
        # OAM is unavailable during mode 2 and 3
        # VRAM is unavailable during mode 3. However this is not currently enforced
        # for performance reasons
        # A scanline always takes 456 cycles in total. However, mode 3 takes
        # longer the more sprites there are and so less time is spent in HBlank
        # GPU timing seems to be extremely complicated. I can't find an exhaustive
        # documentation, but I don't think it matters for my goals.
        old_mode = self.get_mode()

        if old_mode == 0 or old_mode == 1:
            self._io[LY] += 1
            if self._io[LY] == 154:
                self._io[LY] = 0

            if self._io[LY] == self._io[LYC]:
                self._io[STAT] = self._io[STAT] | STAT_LYC_COMPARE
                if self._io[STAT] & STAT_LYC_COMPARE_IRQ:
                    self.cpu.interrupt_request(cpu.IRQ_STAT)
            else:
                self._io[STAT] = self._io[STAT] & ~STAT_LYC_COMPARE

            if self._io[LY] < 144:
                self.set_mode(2)
            elif self._io[LY] == 144:
                self.set_mode(1)
                self.cpu.interrupt_request(cpu.IRQ_VBLANK)
                self.frames += 1

                # Switch buffers
                self.frame_buffer = self.pixel_buffer
                self.frame_done = True
                self.active_buffer = 0 if self.active_buffer else 1
                self.pixel_buffer = self.pixel_buffers[self.active_buffer]
            else:
                self.set_mode(1)
        elif old_mode == 2:
            self.set_mode(3)
            self.draw_scanline()
        elif old_mode == 3:
            self.set_mode(0)

    def update_palettes(self):
        bgp = self._io[BGP]

        self.bg_palette[0] = GRAY_SHADES[(bgp & 0b00000011) >> 0]
        self.bg_palette[1] = GRAY_SHADES[(bgp & 0b00001100) >> 2]
        self.bg_palette[2] = GRAY_SHADES[(bgp & 0b00110000) >> 4]
        self.bg_palette[3] = GRAY_SHADES[(bgp & 0b11000000) >> 6]

        obp0 = self._io[OBP0]

        self.obj_palette0[0] = GRAY_SHADES[(obp0 & 0b00000011) >> 0]
        self.obj_palette0[1] = GRAY_SHADES[(obp0 & 0b00001100) >> 2]
        self.obj_palette0[2] = GRAY_SHADES[(obp0 & 0b00110000) >> 4]
        self.obj_palette0[3] = GRAY_SHADES[(obp0 & 0b11000000) >> 6]

        obp1 = self._io[OBP1]

        self.obj_palette1[0] = GRAY_SHADES[(obp1 & 0b00000011) >> 0]
        self.obj_palette1[1] = GRAY_SHADES[(obp1 & 0b00001100) >> 2]
        self.obj_palette1[2] = GRAY_SHADES[(obp1 & 0b00110000) >> 4]
        self.obj_palette1[3] = GRAY_SHADES[(obp1 & 0b11000000) >> 6]

    def draw_scanline(self):
        pixel_y = self._io[LY]

        # If the background is disabled, fill with white and exit
        if self._io[LCDC] & LCDC_BACKGROUND_ENABLE:
            self.draw_background_scanline(pixel_y)
        else:
            for pixel_x in range(160):
                self.pixel_buffer[pixel_y*160 + pixel_x] = 0xFF
                self.background_transparent[pixel_y*160 + pixel_x] = True

        if self._io[LCDC] & LCDC_WINDOW_ENABLE:
            self.draw_window_scanline(pixel_y)

        if self._io[LCDC] & LCDC_SPRITE_ENABLE:
            self.draw_sprites_scanline(pixel_y)

    def draw_background_scanline(self, pixel_y):
        if self._io[LCDC] & LCDC_BG_TILE_TABLE:
            tile_table = self.tile_table1
        else:
            tile_table = self.tile_table0

        if self._io[LCDC] & LCDC_BG_WIN_TILE_AREA:
            tile_cache = self.tile_cache0
        else:
            tile_cache = self.tile_cache1

        # The coordinates of the top-left of the screen in the background map
        x0 = self._io[SCX]
        y0 = self._io[SCY]
        # Tiles are 8 by 8 and wrap every 256 pixels (32 tiles)
        # We calculate the index of the tile the pixel is in, and the offset
        # of the pixel inside of the tile
        tile_y    = (y0 + pixel_y & 0xFF) // 8
        y_in_tile = (y0 + pixel_y & 0xFF)  % 8

        pixel_x = 0
        tile_x    = (x0 + pixel_x & 0xFF) // 8
        x_in_tile = (x0 + pixel_x & 0xFF)  % 8
        tile_i = tile_table[tile_y*32 + tile_x]

        for pixel_x in range(160):
            color = tile_cache[tile_i*64 + y_in_tile*8 + x_in_tile]
            self.pixel_buffer[pixel_y*160 + pixel_x] = self.bg_palette[color]
            self.background_transparent[pixel_y*160 + pixel_x] = color == 0

            x_in_tile += 1

            if x_in_tile == 8:
                x_in_tile = 0
                tile_x = (tile_x + 1) & 0x1F
                tile_i = tile_table[tile_y*32 + tile_x]

    def draw_window_scanline(self, pixel_y):
        if self._io[LCDC] & LCDC_WIN_TILE_TABLE:
            tile_table = self.tile_table1
        else:
            tile_table = self.tile_table0

        if self._io[LCDC] & LCDC_BG_WIN_TILE_AREA:
            tile_cache = self.tile_cache0
        else:
            tile_cache = self.tile_cache1

        # The coordinates of the top-left of the window in the screen
        # The offset by 7 has to do with the internals of the GPU, fascinating
        x0 = self._io[WX] - 7
        y0 = self._io[WY]

        if pixel_y < y0:
            return

        # Same as the background
        tile_y    = (pixel_y - y0 & 0xFF) // 8
        y_in_tile = (pixel_y - y0 & 0xFF)  % 8

        # WX isn't supposed to be smaller than 7, what if it is?
        # This seems to work...
        if x0 < 0:
            tile_x    = (-x0 & 0xFF) // 8
            x_in_tile = (-x0 & 0xFF)  % 8
            start_x = 0
        else:
            tile_x = 0
            x_in_tile = 0
            start_x = x0

        tile_i = tile_table[tile_y*32 + tile_x]

        for pixel_x in range(start_x, 160):
            color = tile_cache[tile_i*64 + y_in_tile*8 + x_in_tile]
            self.pixel_buffer[pixel_y*160 + pixel_x] = self.bg_palette[color]
            self.background_transparent[pixel_y*160 + pixel_x] = color == 0

            x_in_tile += 1

            if x_in_tile == 8:
                x_in_tile = 0
                tile_x = (tile_x + 1) & 0x1F
                tile_i = tile_table[tile_y*32 + tile_x]

    def draw_sprites_scanline(self, pixel_y):
        ATTR_PRIORITY = 0b10000000
        ATTR_Y_FLIP   = 0b01000000
        ATTR_X_FLIP   = 0b00100000
        ATTR_PALETTE  = 0b00010000

        DOUBLE_SIZE = self._io[LCDC] & LCDC_SPRITE_SIZE
        sprite_height = 16 if DOUBLE_SIZE else 8

        # Sprites with lower value have priority
        # Ugly syntax to reverse range(40)
        for i in range(39, -1, -1):
            # The sprite's x and y coordinates are offset by 8 and 16, so that
            # a double sized sprite can be put just outside of the screen, as
            # the x and y coordinates are unsigned
            sprite_y = self.oam[4*i] - 16
            sprite_x = self.oam[4*i + 1] - 8
            sprite_i = self.oam[4*i + 2]
            attr = self.oam[4*i + 3]
            if pixel_y < sprite_y or pixel_y >= sprite_y + sprite_height:
                continue
            if DOUBLE_SIZE:
                sprite_i &= 0xFE

            y = pixel_y - sprite_y
            if attr & ATTR_Y_FLIP:
                y = sprite_height - 1 - y

            if attr & ATTR_PALETTE:
                palette = self.obj_palette1
            else:
                palette = self.obj_palette0


            for x in range(8):
                if attr & ATTR_X_FLIP:
                    pixel_x = sprite_x + (7 - x)
                else:
                    pixel_x = sprite_x + x

                if pixel_x < 0 or pixel_x >= 160:
                    continue

                # No bank selection for sprites
                color = self.tile_cache0[sprite_i*64 + y*8 + x]
                if color != 0 and (not attr & ATTR_PRIORITY or self.background_transparent[pixel_y*160 + pixel_x]):
                    self.pixel_buffer[pixel_y*160 + pixel_x] = palette[color]

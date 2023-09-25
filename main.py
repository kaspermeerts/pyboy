import math
import time

import mmu
import cpu
import gpu
import apu
import cartridge
import timer
import joypad
import serial

class Emulator:
    def __init__(self, filepath):
        self.cycles = 0

        self.mmu = mmu.MMU()
        self.cpu = cpu.CPU(self.mmu)
        self.gpu = gpu.GPU(self.mmu, self.cpu)
        self.apu = apu.APU()
        self.cartridge = cartridge.Cartridge()
        self.joypad = joypad.Joypad()
        self.timer = timer.Timer(self.cpu)
        self.serial = serial.Serial()

        self.mmu.cpu = self.cpu
        self.mmu.gpu = self.gpu
        self.mmu.apu = self.apu
        self.mmu.cartridge = self.cartridge
        self.mmu.joypad = self.joypad
        self.mmu.timer = self.timer
        self.mmu.serial = self.serial

        self.cartridge.load_rom(filepath)

    def test_fps(self, frames=1000):
        start = self.gpu.frames
        tic = time.time()
        while self.gpu.frames - start < frames:
            clocks = self.cpu.tick()
            self.gpu.step(clocks)
            self.timer.step(clocks)
            #self.apu.step(clocks)

        toc = time.time()
        dt = toc - tic
        df = self.gpu.frames - start
        print("{} frames in {:.4f} seconds: {:0.4f} FPS".format(df, dt, df/dt))

    def step(self, num_clocks):
        count = 0
        while count < num_clocks:
            clocks = self.cpu.tick()
            self.gpu.step(clocks)
            self.timer.step(clocks)
            #self.apu.step(clocks)
            count += clocks
        return count

    def step_frame(self):
        count = 0
        while not self.gpu.frame_done:
            clocks = self.cpu.tick()
            self.gpu.step(clocks)
            self.timer.step(clocks)
            #self.apu.step(clocks)
            count += clocks
        return count

def run(emu):
    import pyglet
    import pyglet.window
    from pyglet.window import key
    #from pyglet import gl
    SCALE = 1

    clock = pyglet.clock.get_default()

    win = pyglet.window.Window(width=160*SCALE, height=144*SCALE, caption=emu.cartridge.name.title(), vsync=True)
    #fps_display = pyglet.window.FPSDisplay(win, samples=60,color=(0.7,0.7,0.0,0.7))
    # XXX Hack to get around pyglet bug. The Image constructor can only make
    # Image's with 4 channels, not 1
    image = pyglet.image.ImageData(160, 144, "L", b'', -160)

    @win.event
    def on_draw():
        win.clear()
        image.set_data("L", -160, bytes(emu.gpu.frame_buffer))
        emu.gpu.frame_done = False
        #texture = image.get_texture()
        #gl.glTexParameteri(gl.GL_TEXTURE_2D, gl.GL_TEXTURE_MAG_FILTER, gl.GL_NEAREST)
        #texture.width = 160*SCALE
        #texture.height = 144*SCALE
        #texture.blit(0, 0)
        image.blit(0, 0)
        #fps_display.draw()

    @win.event
    def on_key_press(symbol, modifiers):
        if symbol == key.X:         emu.joypad.input_a      = True
        if symbol == key.C:         emu.joypad.input_b      = True
        if symbol == key.ENTER:     emu.joypad.input_start  = True
        if symbol == key.BACKSPACE: emu.joypad.input_select = True
        if symbol == key.LEFT:      emu.joypad.input_left   = True
        if symbol == key.RIGHT:     emu.joypad.input_right  = True
        if symbol == key.UP:        emu.joypad.input_up     = True
        if symbol == key.DOWN:      emu.joypad.input_down   = True

    @win.event
    def on_key_release(symbol, modifiers):
        if symbol == key.X:         emu.joypad.input_a      = False
        if symbol == key.C:         emu.joypad.input_b      = False
        if symbol == key.ENTER:     emu.joypad.input_start  = False
        if symbol == key.BACKSPACE: emu.joypad.input_select = False
        if symbol == key.LEFT:      emu.joypad.input_left   = False
        if symbol == key.RIGHT:     emu.joypad.input_right  = False
        if symbol == key.UP:        emu.joypad.input_up     = False
        if symbol == key.DOWN:      emu.joypad.input_down   = False

    has_exit = False
    perf_frames = 0
    performance = 1
    tic = time.perf_counter()
    while not win.has_exit:

        cycles = emu.step_frame()

        toc = time.perf_counter()
        real_interval = toc - tic
        expected_interval = cycles / emu.cpu.CLOCKSPEED
        tic = toc

        #if real_interval < expected_interval - 1e-4:
        #    time.sleep(expected_interval - real_interval - 1e-4)
        #    toc = time.perf_counter()

        #    real_interval = toc - tic
        #    tic = toc

        performance = 0.99 * performance + 0.01 * expected_interval / real_interval
        perf_frames += 1
        if perf_frames == 30:
            win.set_caption("%s (%d%%)" % (emu.cartridge.name.title(), performance*100))
            perf_frames = 0

        pyglet.clock.tick(poll=True)
        win.switch_to()
        win.dispatch_events()
        win.dispatch_event('on_draw')
        win.flip()

    win.close()
    emu.cartridge.save_sram()

if __name__ == "__main__":
    import sys

    if len(sys.argv) < 2:
        sys.exit("Usage: %s rom-file" % sys.argv[0])

    filename = sys.argv[1]

    emu = Emulator(filename)

    run(emu)


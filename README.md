# Python Game Boy emulator

I don't know what came over me. Python's abysmal performace, moronic typing system and inherent distance from the underlying representation of the data types make it the worst possible imaginable programming language to emulate an 8-bit console in.

But hey! It works! You just need PyPy's JIT magic to barely get 60 fps.

There's not even a half decent API to output raw audio samples using Python, so I completely gave up on finishing the APU. The only thing this piece of garbage has going for it, is that it's relatively clean, clocking in at 1800ish lines of code. It doesn't even run right now of course, but that's because `pyglet` sucks. Good luck.

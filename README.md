# Space Invaders arcade machine

This is a CLaSH implementation of the 1978 Space Invaders arcade
machine by Taito.

[![Click to play YouTube video](https://img.youtube.com/vi/k-1MVmX2ytI/0.jpg)](https://www.youtube.com/watch?v=k-1MVmX2ytI)


## Hardware architecture

* **CPU**: Intel 8080.
* **Video**: 256x224 monochrome at 60 FPS, backed by a 7168x8 bit
  framebuffer. IRQs triggered on lines 96 and 224. The screen was
  rotated 90 degrees to be in portrait orientation.
* **Sound**: TI SN76477 connected to the CPU via two 8-bit ports
* **Inputs**: coin deposit detector, two sets of buttons for starting the
  game, and moving and firing, for two-player mode.

## CLaSH implementation

* **CPU**: Implemented based on abstract descriptions of Intel 8080 ISA,
  with no reference to real implementation. Not cycle-accurate, but
  aims to pass various functional tests.
* **Video**: Standard 640x480@60Hz VGA, with 25.175MHz pixel
  clock. Logical "pixels" are scaled 2x2, but the layout is not
  rotated to keep interrupt timings.
* **Framebuffer**: To keep the implementation simple, there is no
  CPU/video contention: framebuffer writes are simply forked into a
  second RAM that is read from only by the video system.
* **Sound**: Unimplemented.
* **Inputs**: PS/2 keyboard. `C` to deposit a coin, `Enter` to start
  1P game, and `Left` / `Right` / `RCtrl` to move and fire with P1.

## Limitations

This implementation doesn't aim to be cycle-accurate; it wouldn't
matter for much anyway, since the CPU runs at the VGA output pixel
clock of 25 MHz, whereas the real cabinet ran at 2 MHz. Luckily, Space
Invaders does all animations keyed to two 60 Hz timers triggered by
the video system.

Sound is not implemented at all.

The back-panel DIP switches and player two's buttons are not yet
mapped to anything.

# Building

Included are rudimentary Shake rules to build for the Xilinx Spartan-6
based Papilio Pro with the Arcade mega-wing. There are also some files
for building for the Altera Cyclone-V-based DE10-Nano, but there is no
input support for DE10-Nano yet.

Make a file called `build.mk` with content similar to the following:

```
BOARD = papilio-pro
BOARDCONFIG = arcade
CLASH = stack exec --
XILINX = ~/prog/docker/xilinx-ubuntu-12.04/run
```

The `CLASH` and `XILINX` fields are to optionally wrap invocations of
the CLaSH compiler and the Xilinx ISE toolchain in case you want to
run them via Stack, Docker, etc.

Then you can build for the Papilio Pro by running the included `mk`
script.


# Useful links

* [Wikipedia](https://en.wikipedia.org/wiki/Space_Invaders)
* [Computer Archeology's Space Invaders page](http://computerarcheology.com/Arcade/SpaceInvaders/)
* [Intel 8080 opcodes](http://pastraiser.com/cpu/i8080/i8080_opcodes.html)
* [Intel 8080 opcodes](http://www.classiccmp.org/dunfield/r/8080.txt)
* [Monadic descriptions of CPUs in CLaSH](https://gergo.erdi.hu/blog/2018-09-30-composable_cpu_descriptions_in_c_ash,_and_wrap-up_of_retrochallenge_2018_09/)
* [Papilio Pro FPGA dev board](https://papilio.cc/index.php?n=Papilio.PapilioPro)
* [DE10-Nano](http://de10-nano.terasic.com/)

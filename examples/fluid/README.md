A Stable Fluid Simulation
=========================

This is an implementation of the fluid simulation described in the paper
_Real-time Fluid Dynamics for Games_ (Jos Stam, GDC 2003).


Usage
-----

Running the program opens a window which displays the current state of the
simulation.

Execute with the flag `--help` to display a complete list of command-line
options and runtime controls.

To interact within the simulation window:

  * _click:_            add particles to the simulation
  * _shift-click-drag:_ introduce forces, proportional to the speed of motion
  * _r:_                reset the simulation to the initial state
  * _d:_                toggle display of the density (particle) field
  * _v:_                toggle display of velocity field lines


Notes
-----

The animation package does not drop frames that arrive late, so if you request a
framerate that is too high, the frames will back up and the program slowly grind
to a halt. You can test the capability of your hardware, instead of running the
simulation proper, by executing with the flag `--benchmark`. Select a
`--framerate` and simulation `--width` & `--height` as appropriate.

When running in the CUDA backend, it will take several seconds before all
kernels are compiled and the simulation is ready.

It is often nice to start with some initial data, rather than a blank screen.
You can use the flags `--bmp-density` and/or `--bmp-velocity` to introduce
initial conditions by reading from a bitmap image file. Try using one of the
images located in `data/images` as an initial density source.


Alternatives
------------

The original C implementation by Jus Stam is included in this distribution. The
paper describing the implementation can be found here:
<http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf>

An implementation using Repa can be found as part of the gloss-examples package.
<http://hackage.haskell.org/package/gloss-examples>



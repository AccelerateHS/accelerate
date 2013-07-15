*** Fast N-Body Simulation with CUDA *** 

This sample code originally accompanied the GPU Gems 3 article 
"Fast N-body Simulation with CUDA", by Lars Nyland, Mark Harris, 
and Jan F. Prins.  It has been enhanced since then.

To use the code, you will need The NVIDIA CUDA Toolkit version 
2.0 or later, available at http://developer.nvidia.com/cuda.
However by default it compiles support for sm_20, which requires
CUDA version 3.0 or later.  

You will also need an NVIDIA GPU with CUDA capability.

*** Building the Code ***
Windows:
1. Go to the "common" directory and open the cutil.sln, and 
   build both the Debug and Release configurations.  Exit. Open
   the paramgl.sln, and build both the Debug and Release 
   configurations.  Exit.
2. Go to the src/nbody directory and open the nbody.sln or nbody_vc90.sln.
   Build all configurations.
   
Linux:
1. Go to the common directory and type "Make; Make dbg=1".  Then
   type "Make -f Makefile_paramgl; Make -f Makefile_paramgl dbg=1".
2. Go to the projects/nbody directory and type
   "Make; Make dbg=1; Make emu=1; Make emu=1 dbg=1"

*** Running the N-Body Simulator ***

Windows:
You can either run the code from inside visual studio, or go to 
the bin/win32/release directory and type "nbody".

Linux:
Run bin/linux/release/nbody

There are three execution modes: interactive mode, benchmark mode,
and test mode, described below.  There are also a number of 
command line options.

*** Command Line Options ***

-benchmark: Runs in benchmark mode, as described below.
-compare:   Runs in test mode, as described below.

Note: If neither benchmark or interactive mode is selected, 
      interactive mode will be run.

-n=<NUM>: Sets the number of bodies in the simulation.  
           The default is 1024 * SM, where SM is the number 
           of multiprocessors on the device.
           
-p=<NUM>: Sets the width of the tile used in the simulation.
           The default is 256.
-q=<NUM>: Sets the height of the tile used in the simulation.
           The default is 1.
           
           
-i=<NUM>: Sets the number of iterations to run in benchmark mode.
           The default is 10.

-fullscreen: Render in fullscreen mode

-fp64: Use double precision computation for all simulation

-cpu: Perform the simulation on the CPU.  (Note, on MSVC 8 or later
      you can compile the "Release OpenMP" configuration to run
      the CPU simulation on multiple cores.

-hostmem: Use CUDA's "zero-copy" feature to store simulation results
          in the host memory rather than device memory.

*** Interactive Mode ***

In interactive mode, you will be able to visualize the bodies in
the simulation like stars in a galaxy.  By default, interactive 
mode starts out cycling through a set of preset demo parameters 
every 5 seconds.  To toggle this cycling, press 'c'.  

There are several keyboard and mouse controls.

Mouse Buttons:
Left: Rotate the view.
CTRL+Left: Translate the view forward and back
SHIFT+Left: Pan the view in the image plane

Keyboard:
SPACE: Pause / Un-pause the simulation
ENTER: Toggle single / double precision simulation
q:     Quit.
`:     Toggle sliders
p:     Cycle textured / colored / point body display
d:     Enable / Disable rendering
c:     Enable / Disable cycling through demo presets every 5 
       seconds
p:     Print current parameters (just cut and paste the output
       into the demoParams array in nbody.cpp to add your own 
       parameter preset!)
]:     Go to next parameter preset
[:     Go to previous parameter preset
1:     Reset the bodies in randomized rotating "shell" 
       configuration.
2:     Reset the bodies in randomized spherical configuration 
       with randomized velocities.
3:     Reset the bodies in randomized configuration with 
       "expanding" velocity.
       
Sliders:
Velocity Damping: Controls the damping factor applied to the 
                  velocity each time step.
Softening Factor: Controls the softening factor epsilon in the 
                  gravitational force calculation.
Time step:        Controls the simulation step size.
Cluster Scale:    This is the relative size of the starting cluster.  
                  This affects the initial conditions when you press
                  '1', '2', or '3'.
Velocity Scale:   This is the scale of the initial velocities of the 
                  starting cluster. This affects the initial conditions 
                  when you press '1', '2', or '3'.

      
*** Benchmark Mode ***

In benchmark mode, the simulation is run for a number iterations
without rendering and timed, and the simulator reports the total
time, average time, and average body-body interactions per second
and GFLOP/s rates.

*** Test Mode ***

In test mode, the simulation is run on both the CPU and GPU and 
a comparison is done.  If all positions in the GPU simulation are
within a tolerance of the CPU simulation, the simulator reports
"Test PASSED".  Otherwise it reports "Test FAILED".



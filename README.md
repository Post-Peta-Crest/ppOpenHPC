
### ppOpen-APPL/AMR-FDM ver.0.3.0 ###
### advAMR3D            ver.0.3.0 ###

***********************************************************************************
1. Descriptions

This directory contains ppOpen-APPL/AMR-FDM (ver.0.3.0) and sample codes with
with data sets. ppOpen-APPL/AMR-FDM is an adaptive mesh refinement (AMR) framework 
for development of 3D parallel finite-difference method (FDM) applications by
the following capabilities:

- Cell-based adaptive mesh structure
- Dynamic domain decomposition (DDD) to avoid load imbalance problem
- Explicit time integration
- Flat MPI & OpenMP/MPI hybrid parallel programming models

All the codes are written in Fortran 90.

***********************************************************************************
2. Directories

 * $(CUR): current directory

 $(CUR)/src                   : source code files of ppOpen-APPL/AMR-FDM
   
 $(CUR)/examples/advAMR3D/src : source code files of advAMR3D using ppOpen-APPL/
                                AMR-FDM
 $(CUR)/examples/advAMR3D/run : sample data sets of advAMR3D (control data)
 $(CUR)/examples/advAMR3D/run/results 
                              : directory storing sample results
 $(CUR)/examples/advAMR3D/post_vtk
                              : source code file of post-processing program to 
                                convert from output data files by advAMR3D to 
                                Visualization Toolkit (VTK) format data files,
                                and sample data sets (control data)
 $(CUR)/examples/advAMR3D/post_vtk/output
                              : directory storing VTK format data files
          
***********************************************************************************
3. Quick Start

(1) Preparation

 - Fortran 90 compliers (Operations have been confirmed by Intel and Fujitsu compilers)
 - MPI library
 - OpenMP must be supported if you want to develop OpenMP/MPI Hybrid codes

(2) Modify 'Makefile.in', samples are found in $(CUR)/etc
 $(CUR)/etc/Makefile.in.fx10.flatmpi  : Flat MPI for Fujitsu FX10
 $(CUR)/etc/Makefile.in.intel.flatmpi :              Intel Compiler

 $(CUR)/etc/Makefile.in.fx10.hybrid   : OpenMP/MPI Hybrid for Fujitsu FX10
 $(CUR)/etc/Makefile.in.intel.hybrid  :                       Intel Compiler

 $(MPIF90)           : FORTRAN 90 with MPI
 $(F90)              : FORTRAN 90 for single core
 $(sFFLAGS)          : Compiler options for Optimizations for post_vtk
                       (DO NOT INCLUDE "OpenMP" flags)
 $(pFFFLAGS)         : Compiler options for Optimizations for ppOpen-APPL/AMR-FDM
                       and advAMR3D

 $(PREFIX)/include   : directory that holds the installed module files
 $(PREFIX)/lib       :                                    libraries
 $(PREFIX)/bin       :                                    exec. files

 *** NOTICE ***: $(PREFIX) directory must be specified as ABSOLUTE/FULL path.

(3) Complile/install ppOpen-APPL/FVM
 $>  cd $(CUR)/
 $>  make                 -> ppOpen-APPL/AMR-FDM
                             $<CUR>/lib/ppohAMRFDMlib.a

 $>  make install         -> Install ppohAMRFDM library to the destination
                             $<PREFIX>/lib/ppohAMRFDMlib.a

(4) Compile/install advAMR3D and post_vtk
 $>  cd $(CUR)/
 $>  make advAMR3D        -> advAMR3D + post_vtk
                             $<CUR>/bin/advAMR3D
                             $<CUR>/bin/post_vtk

 $>  make bin_install     -> Install advAMR3D/post_vtk to the destination
                             $<PREFIX>/bin/advAMR3D
                             $<PREFIX>/bin/post_vtk

 *** NOTICE ***: Processes (4) must be done after (3).

(5) Running the code 
     (advAMR3D)
 $>  cd $(CUR)/examples/advAMR3D/run
 $>  mpiexec -n 8 <$PREFIX>/bin/advAMR3D with appropriate thread number for OpenMP
     (or corresponding operations)
 $>  cat $(CUR)/examples/hybNS/run/prop*****rank****.dat 
     -> '*' means iteration count, and rank number for MPI process 

     (post_vtk)
 $>  cd $(CUR)/examples/advAMR3D/post_vtk
 $>   <$PREFIX>/bin/post_vtk
     (or corresponding operations)
 $>  cat $(CUR)/examples/advAMR3D/post_vtk/output/prop*****.dat
      -> '*' means iteration count

(5) Clean/Unistall
 $>  cd $(CUR)/
 $>  make clean          -> Clean files
 $>  make unistall       -> Delete all installed files and directories

***********************************************************************************

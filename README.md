

   ppOpen-APPL/DEM ver.1.0.0 - ppOpen-APPL/DEM utility code

					Copyright (c) 2014 JAMSTEC                         

======================================================================
1. Introduction:

  This README file is a part of ppOpen-APPL/DEM. 
  ppOpen-APPL/DEM provides fundamental components of the particle
  simulations based on the discrete element method (DEM). 
  ppOpen-APPL/DEM (ver.1.0.0) includes the libraries for the DEM, 
  sample codes, and data sets. ppOpen-APPL/DEM-Util provides the 
  preconditioning utilities. This utility prepares data sets of distributed
  data files from the mesh data sets. 
  
  All the codes are written in Fortran 90.
                            
  ppOpen-APPL/DEM is a free software, which can be used under the
  terms of The MIT License (MIT). See LICENSE file and User's guide
  for more details.
                                                               
  This software is one of the results of the project "ppOpen-HPC:
  Open Source Infrastructure for Development and Execution of
  Large-Scale Scientific Applications on Post-Peta-Scale
  Supercomputers with Automatic Tuning (AT)."

  See also the project web site:                  

    http://ppopenhpc.cc.u-tokyo.ac.jp/

========================================================================
2. Directories:

  * $(CUR): current directory

$(CUR)/src 	source code files of ppOpen-APPL/DEM 
$(CUR)/utils 	source code files of ppOpen-APPL/DEM-Util 
$(CUR)/utils/data 	sample data sets of ppOpen-APPL/DEM-Util 
$(CUR)/utils/input 	sample control data of ppOpen-APPL/DEM-Util 
$(CUR)/examples 	sample source code files using ppOpen-APPL/DEM 
$(CUR)/examples/data 	Particles output files  
$(CUR)/examples/obj 	Objects output files 
$(CUR)/examples/pov 	POV-Ray output files 
$(CUR)/include 	directory that stores created module files 
$(CUR)/lib 	directory that stores created libraries 
$(CUR)/bin 	directory that stores created executable programs 
$(CUR)/doc 	documents 
$(CUR)/etc 	examples of eMakefile.inf 
  
========================================================================
3. Quick Start:


(1) Preparation 

Prepare Fortran 90 compilers (Operations have been confirmed 
with GNU Fortran and Fujitsu compilers) and MPI and OpenMP libraries. 


(2) Modify eMakefile.inf 

Samples of eMakefile.inf are found in $(CUR)/etc. 

$(CUR)/etc/Makefile.in.gfortran	GNU Fortran
$(CUR)/etc/Makefile.in.fx10	Fujitsu FX10


(3) Compile/install ppOpen-APPL/DEM-Util

$> cd $(CUR)
$> make clean_util
$> make util
$> make install_util


(4) Compile/install ppOpen-APPL/DEM and a sample code

$> cd $(CUR)
$> make clean
$> make
$> make install


(5) Set input files

The sample input data files are in the directories, $(CUR)/utils/input 
and $(CUR)/examples. 


(6) Running the code

sample_DEMutil

$> cd $(CUR)/util
$> mpirun np 4 ./sample_DEMutil 

	With appropriate thread number for OpenMP (or corresponding operations).

sample_mail01 

$> cd $(CUR)/examples
$> cp ../utils/distance_* ./		copy distance data files 
$> cp ../utils/object_mesh.stl ./ 	copy the 3D objects file 
$> ./sample_main01 


(7) Clean/uninstall

$> cd $(CUR)/
$> make clean		clean files. 
$> make uninstall	      delete all installed files and directories. 


                                                                        
   HACApK code                                                          
                                                                        
Copyright (c) 2015 <Akihiro Ida and Takeshi Iwashita>                   
========================================================================

Overview:
HACApK is a software library for hierarchical matrices with adaptive 
cross approximationwhich works on distributed memory computer systems.
In the current version, the MPI library is mainly utilized for parallel 
computing, and the OpenMP paradigm only partially for parallel computing 
on shared memory.
As the directory including this README file is the current directory,
The directory "./src/" includes the sorce codes of HACApK and 
the directory "./doc/" includes the manual for HACApK.

Introduction:
  This file is a part of HACApK.
  HACApK is a free software application, you can use it under the terms
  of The MIT License (MIT). See LICENSE file and User's guide for more
  details.

  This software is one of the results of the "ppOpen-HPC: Open Source
  Infrastructure for Development and Execution of Large-Scale Scientific
  Applications on Post-Peta-Scale Supercomputers with Automatic Tuning
  (AT)" project.

  See also the project web site:
    http://ppopenhpc.cc.u-tokyo.ac.jp/
========================================================================

How to try our software anyway:
 1. Copy all files under ./src/HACApK_with_BEM-BB-framework_1.0.0 
    to user's arbitrary directory.
 2. Compile and link all source files (Fortran90) with MPI library and 
    the option for OpenMP.
    (1) Modify 'Makefile' to adapt to the computational environment
        (Currently, we have confirmed the compilation by Intel and 
         Fujitsu Fortran compilers).
    (2) Complile HACApK with BEM-BB-framework.
        $>  cd ./
        $>  make    -> bem-bb-SCM.out
 3. Run it with MPI library
        $>  cd ./
        $>  mpirun -np 4 ./bem-bb-SCM.out (or corresponding operations)
 4. Find a file named 'sample.output.vtk' in the current directory.
    The execution file 'bem-bb-SCM.out' will output the file if it is 
    successfully carried out.
    You can visualize the result by a visualization software 'paraview'.

The sample code 'bem-bb-SCM.out' above simulates electrostatic analyses.
If you intends to apply HACApK to your own problem,
please read the file 'manual_Hacapk_1.0.0.pdf' under the directory "./doc/".

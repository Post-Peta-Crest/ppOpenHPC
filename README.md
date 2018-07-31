                                                                        
   ppohBEM - ppOpen-APPL/BEM code                                       
                                                                        
Copyright (c) 2015 <Takeshi Iwashita, Takeshi Mifune, Yuki Noseda,      
                    Yasuhito Takahashi, Masatoshi Kawai, Akihiro Ida>   
========================================================================

ppohBEM is software used to support a boundary element analysis executed
on a parallel computer. The current version includes a software framework
 for a parallel BEM analysis and an H-matrix library. If you want to use the 
 framework based on dense matrix computations, please move to the directory
 "bem-bb-framework_dense". If you want to use the H-matrix library, please move
 to the directly "Hacapk".
 
------------------------------------------------------------------------ 
  This file is part of ppohBEM.
  ppohBEM is a free software application, you can use it under the terms
  of The MIT License (MIT). See LICENSE file and User's guide for more
  details.

  This software is one of the results of the "ppOpen-HPC: Open Source
  Infrastructure for Development and Execution of Large-Scale Scientific
  Applications on Post-Peta-Scale Supercomputers with Automatic Tuning
  (AT)" project.

  See also the project web site:
    http://ppopenhpc.cc.u-tokyo.ac.jp/

------------------------------------------------------------------------ 

 --- Change History ---
（Changes in release 0.4.0）

・The new version of HACApK which is paralleized in the hybrid parallel programming model is included.
・The BEM-BB framework was minorly updated.

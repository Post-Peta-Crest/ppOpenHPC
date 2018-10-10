## What is ppOpen-HPC

[ppOpen-HPC](http://ppopenhpc.cc.u-tokyo.ac.jp/ppopenhpc/) is an open source infrastructure for the 
development and execution of optimized and reliable simulation code on post-peta-scale (pp) parallel 
computers based on many-core architectures comprising various types of libraries that cover general 
procedures for scientific computations. Source code developed on a PC with a single processor is 
linked to these libraries, and the generated parallel code is optimized for post-peta-scale systems. 
The target post-peta-scale systems are many-core-based supercomputers, such as the Oakforest-PACS with 
8,208 Intel Xeon Phi (Knights Landing) operated by Joint Center for Advanced High Performance 
Computing ([JCAHPC](http://jcahpc.jp/eng/index.html)). ppOpen-HPC is part of a five-year project 
(FY.2011-FY.2015) spawned from the "Development of System Software Technologies for Post-Peta Scale 
High-Performance Computing" project funded by JST-CREST. The framework covers various types of 
procedures for scientific computations, such as the parallel I/O of distributed datasets, matrix 
assembly, linear solvers with practical and scalable preconditioners, visualization, adaptive mesh 
refinement, and dynamic load balancing, in various types of computational models, such as FEM, FDM, 
FVM, BEM, and DEM. Automatic tuning (AT) technology enables the automatic generation of optimized 
libraries and applications under various types of environments. We released the most updated version 
of ppOpen-HPC as an open source software every year in November from 2012 to 2015.

In 2016, the ppOpen-HPC team joined the Equipping Sparse Solvers for Exascale ([ESSEX-II](http://blogs.fau.de/essex/)) 
project (led by P.I. Professor Gerhard Wellein of the University of Erlangen-Nuremberg)
, which is funded by JST-CREST and the German DFG priority program 1648 
Software for Exascale Computing (SPPEXA) under a Japan (JST)-Germany (DFG) collaboration, which 
continues until FY.2018. In the ESSEX-II project, we are developing pK-Open-HPC (an extended version 
of ppOpen-HPC, which is a framework for exa-feasible applications), preconditioned iterative solvers 
for quantum sciences, parallel reordering methods, fault resilient capabilities with checkpoint 
restarting and a framework for AT with a performance model. 

![Image of ppOpen-HPC](https://github.com/Post-Peta-Crest/ppOpenHPC/blob/master/Image_ppOpen-HPC.png "image")

### How to use

This repository has 8 branches.
Each branch has another libraries.
Please checkout branch for using library which you want.

### Support or Contact

Having trouble with Pages? Check out our documentation in each branch or [contact support]() and we’ll help you sort it out.

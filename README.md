## What is ppOpen-HPC

"ppOpen-HPC" is an open source infrastructure for the development and execution of optimized and 
reliable simulation code on post-peta-scale (pp) parallel computers based on many-core architectures 
comprising various types of libraries that cover general procedures for scientific computations. Source 
code developed on a PC with a single processor is linked to these libraries, and the generated parallel 
code is optimized for post-peta-scale systems. The target post-peta-scale system is the Post T2K System. 
"ppOpen-HPC" is part of a five-year project (FY.2011-FY.2015) spawned from the "Development of System 
Software Technologies for Post-Peta Scale High-Performance Computing" project funded by JST-CREST. The 
framework covers various types of procedures for scientific computations, such as the parallel I/O of 
datasets, matrix assembly, linear solvers with practical and scalable preconditioners, visualization, 
adaptive mesh refinement, and dynamic load balancing, in various types of computational models, such as 
FEM, FDM, FVM, BEM, and DEM. Automatic tuning (AT) technology enables the automatic generation of 
optimized libraries and applications under various types of environments. We released the most updated 
version of ppOpen-HPC as an open source software every year in November from 2012 to 2015
(available at http://ppopenhpc.cc.u-tokyo.ac.jp/ppopenhpc/). In 2016, the ppOpen-HPC team joined the Equipping Sparse 
Solvers for Exascale (ESSEX-II) project (led by P.I. Professor Gerhard Wellein of the University of 
Erlangen-Nuremberg, http://blogs.fau.de/essex/), which is funded by JST-CREST and the German DFG 
priority program 1648 "Software for Exascale Computing" (SPPEXA) under a Japan (JST)-Germany (DFG) 
collaboration, which continues until FY.2018. In the ESSEX-II project, we are developing pK-Open-HPC (an 
extended version of ppOpen-HPC, which is a framework for exa-feasible applications), preconditioned 
iterative solvers for quantum sciences, parallel reordering methods, and a framework for AT with a 
performance model.

### How to use

This repository has *** branches.
Each branch has another libraries.
Please checkout branch for using library which you want.

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/Post-Peta-Crest/sample/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.

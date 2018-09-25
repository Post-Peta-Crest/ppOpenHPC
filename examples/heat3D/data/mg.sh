#!/bin/sh
#PJM -L "node=1"
#PJM -L "elapse=00:05:00"
#PJM -L "rscgrp=debug"
#PJM -g "gc26"
#PJM -j
#PJM -o "mg.lst"
#PJM --mpi "proc=8"

mpiexec ./../../../bin/pmesh
rm wk.*




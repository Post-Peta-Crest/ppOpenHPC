#!/bin/sh

#PJM -L "rscgrp=debug"
#PJM -L "node=8"
#PJM --mpi "proc=8"
#PJM -g "gc26"
#PJM -L "elapse=00:10:00"
#PJM -j
#PJM -o "test.lst"

export OMP_NUM_THREADS=16

mpiexec ./heat3D

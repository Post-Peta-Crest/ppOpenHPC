#!/bin/sh
#PJM -L "node=4"
#PJM -L "elapse=00:20:00"
#PJM -L "rscgrp=debug"
#PJM -g "gc26"
#PJM -j
#PJM -o "test.lst"
#PJM --mpi "proc=64"

mpiexec ./fdm_test




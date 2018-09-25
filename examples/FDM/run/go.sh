#!/bin/sh

#PJM -L "rscgrp=short"
#PJM -L "node=1"
#PJM -g "gc26"
#PJM --mpi "proc=8"
#PJM -L "elapse=01:00:00"
#PJM -j
#PJM -o "test.out"

mpiexec ./fdm_test

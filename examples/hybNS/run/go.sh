#!/bin/sh
#PJM -N "g16-01"
#PJM -L "node=8"
#PJM -L "rscgrp=debug"
#PJM -L "elapse=00:08:00"
#PJM -g "gc26"
#PJM -j 
#PJM -o "result.lst"
#PJM --mpi "proc=8"

LPG="/opt/FJSVxosmmm/sbin/lpgparm -p 32MB -d 32MB -h 32MB -s 32MB -t 32MB"
export OMP_NUM_THREADS=16
mpiexec ./hybNS



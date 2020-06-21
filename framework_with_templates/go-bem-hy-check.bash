#!/bin/bash
#PJM -L "rscgrp=debug"
#PJM -L "node=8"
#PJM --mpi "proc=8"
#PJM -L "elapse=30:00"

export OMP_NUM_THREADS=16

#Execute without AT
export OAT_EXEC=0
mpirun ./bem-bb-SCM_AT.out

#Execute before execute-time AT 
export OAT_EXEC=1
mpirun ./bem-bb-SCM_AT.out

#Execute with the tuned parameters
export OAT_EXEC=0
mpirun ./bem-bb-SCM_AT.out


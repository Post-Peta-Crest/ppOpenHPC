#!/bin/bash
#PJM -L "rscgrp=debug"
#PJM -L "node=8"
#PJM --mpi "proc=8"
#PJM -L "elapse=0:30:00"

rm OAT_InstallppohFDM_*.dat
rm OAT_Seism3d_timings*.dat


export OMP_NUM_THREADS=16

# execution without AT
export OAT_EXEC=0
mpirun ./seism3d3n
mv ./OAT_Seism3d_timings.dat ./OAT_Seism3d_timings_withoutAT.dat

# execution with AT
export OAT_EXEC=1
mpirun ./seism3d3n
mv ./OAT_Seism3d_timings.dat ./OAT_Seism3d_timings_withAT.dat

# execution with result of AT
export OAT_EXEC=0
mpirun ./seism3d3n
mv ./OAT_Seism3d_timings.dat ./OAT_Seism3d_timings_withAT_exec0.dat


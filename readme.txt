
*****************************************************************
  How to use ppOpen-APPL/BEM with Auto-Tuning version 0.1.0
*****************************************************************
2014.9.20

* How to install source codes to original distribution.

  1. Please put the files of "framework_with_templates" in this distribution 
   to directory "ppohBEM_0.1.0/bem-bb-framework_dense/framework_with_templates/" 
   in original distribution, such as:

>scp -r ./framework_with_templates <Root directory of ppOhBEM>/ppohBEM_0.1.0/bem-bb-framework_dense/framework_with_templates/

  2. Modify "Makefile" file to fit your environment.

  3. Type "make" as same as original distribution of ppOpen-APPL/BEM.

  4. Submit the job with the executable file which is made by the process 3.


* How to use Auto-tuning 

  The auto-tuner can be invocated by using the environment variable 
of OAT_EXEC. The value is defiled as follows:

  OAT_EXEC = 0 : 
    Do not auto-tuning, or use the best parameter of auto-tuned 
  in before invocation.

  OAT_EXEC=1 : 
    Do auto-tuning. If users did auto-tuning previous, 
  the parameters are ignored and updated in this invocation.  

The followings are an example of usage of auto-tuning in bash shell.

 # execution without AT
 export OAT_EXEC=0
 mpirun -np 8 ./bem-bb-SCM.out (or corresponding operations)
   
In this phase, no auto-tuning is executed, and use de-fault parameters.

 # execution with AT
 export OAT_EXEC=1
 mpirun -np 8 ./bem-bb-SCM.out (or corresponding operations)

In this phase, auto-tuning is performed, and use the best parameters 
within the execution. In addition, the best parameters are stored in 
files in current directly.

 # execution with result of AT
 export OAT_EXEC=0
 mpirun -np 8 ./bem-bb-SCM.out (or corresponding operations)

In this phase, auto-tuning is not performed, but use the best parameters 
from the file. Hence the best parameters are set in this invocation. 
Hence the tuned code is automatically set in this execution.

After that, you do not need to perform auto-tuning, unless 
you do not change the problem size and the execution state, 
such as the number of MPI processes and the number of OpenMP threads. 

If you modify these, you need to perform auto-tuning again.


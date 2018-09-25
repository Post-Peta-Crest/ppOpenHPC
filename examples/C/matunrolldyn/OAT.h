#include <stdio.h>
#include <math.h>
#define OAT_ALL 0
#define OAT_INSTALL 1
#define OAT_STATIC 2
#define OAT_DYNAMIC 3

#define OATLSM_MAX_N 512
#define OATLSM_MAX_NPARM 512

#define OATLSM_MAX_M 10
#define OATLSM_MAX_SAMP 512

char OAT_AllRoutines[10000];
char OAT_InstallRoutines[10000];
char OAT_StaticRoutines[10000];
char OAT_DynamicRoutines[10000];

int myid;
int nprocs;
int OAT_STATICTUNE;
int OAT_DYNAMICTUNE;
int OAT_DEBUG;
int OAT_NUMPROCS;
int OAT_STARTTUNESIZE;
int OAT_ENDTUNESIZE;
int	OAT_SAMPDIST;

double OAT_Wtime();



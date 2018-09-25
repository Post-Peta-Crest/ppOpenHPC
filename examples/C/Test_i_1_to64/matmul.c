#include "OAT.h"
#define N 500

main()
{
	int iauto;
	int n;
	double A[N][N], B[N][N], C[N][N];

#pragma OAT call OAT_ATset(OAT_ALL, OAT_AllRoutines)
#pragma OAT call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
#pragma OAT call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
#pragma OAT call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)

	myid = 0;
	iauto = 1;
	n = 10;

#pragma OAT OAT_DEBUG = 1

	if (iauto == 1){
#pragma OAT OAT_NUMPROCS = 4
#pragma OAT OAT_STARTTUNESIZE = 100
#pragma OAT OAT_ENDTUNESIZE = 500
#pragma OAT OAT_SAMPDIST = 100
#pragma OAT call OAT_ATexec(OAT_INSTALL, OAT_InstallRoutines,n,A,B,C)
	}
	InitData(A, B, C, n);
	MatMul(A, B, C, n);
	OutputData(A, B, C, n);
	return 0;
}

MatMul(double A[N][N],double B[N][N], double C[N][N], int n)
{
	double da1, da2;
	double dc;
	int i,j,k;

#pragma OAT call OAT_BPset("n")
#pragma OAT install unroll (i) region start
#pragma OAT name MyMatMul
#pragma OAT varied (i) from 1 to 64
#pragma OAT debug (pp)
#pragma OAT allocate (auto)
#pragma omp parallel for
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			da1 =  A[i][j];
			for(k = 0 ; k < n ; k++){
				dc = C[k][j];
				da1 = da1 + B[i][k] * dc;
			}
			A[i][j] = da1;
		}
	}
#pragma OAT install unroll (i) region end
	return 0;
}

InitData(double A[N][N],double B[N][N], double C[N][N], int n)
{
	int i,j;

	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			A[i][j] = 0.0;
		}
	}
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			B[j][i] =(i+1)*(j+1);
			C[j][i] = 1.0/((i+1)*(j+1));
		}
	}
}

OutputData(double A[N][N],double B[N][N], double C[N][N], int n)
{
	FILE *fp;
	int i,j;

	fp = fopen("output.txt","wt");
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			fprintf(fp,"%20.8f\n",A[i][j]);
		}
	}
	fclose(fp);
}


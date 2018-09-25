#include "OAT.h"
#define N 500

#include "OAT_Routines.h"

int OAT_iusw1_MyMatMul = 0;
int OAT_iusw1_MyMatMul_flag = 0;

main()
{
	int iauto;
	int n;
	double A[N][N], B[N][N], C[N][N];

       OAT_ATset(OAT_ALL, OAT_AllRoutines);
       OAT_ATset(OAT_INSTALL, OAT_InstallRoutines);
       OAT_ATset(OAT_STATIC, OAT_StaticRoutines);
       OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines);

	myid = 0;
	iauto = 1;
	n = 10;

       OAT_DEBUG = 1;

	if (iauto == 1){
       OAT_NUMPROCS = 4;
       OAT_STARTTUNESIZE = 100;
       OAT_ENDTUNESIZE = 500;
       OAT_SAMPDIST = 100;
	 OAT_ATexec(OAT_INSTALL, OAT_InstallRoutines,n,A,B,C);
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
	OAT_SetParm(1,"MyMatMul",n,&OAT_iusw1_MyMatMul);
	OAT_InstallMyMatMul(n,A,C,B,OAT_iusw1_MyMatMul);
//#pragma OAT install unroll (i) region start
//#pragma OAT name MyMatMul
//#pragma OAT varied (i) from 1 to 64
//#pragma OAT debug (pp)
	if(OAT_DEBUG >= 1){
		printf("myid: %d\n",myid);
		printf("Install Routine: MyMatMul\n");
		printf("iusw1: %d\n",OAT_iusw1_MyMatMul);
	}
//#pragma OAT allocate (auto)
//#pragma omp parallel for
//	for(i = 0 ; i < n ; i++){
//		for(j = 0 ; j < n ; j++){
//			da1 =  A[i][j];
//			for(k = 0 ; k < n ; k++){
//				dc = C[k][j];
//				da1 = da1 + B[i][k] * dc;
//			}
//			A[i][j] = da1;
//		}
//	}
//#pragma#pragma OAT install unroll (i) region end
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


#include "OAT_ControlRoutines.c"
#include "OAT_DynamicRoutines.c"
#include "OAT_InstallRoutines.c"
#include "OAT_StaticRoutines.c"


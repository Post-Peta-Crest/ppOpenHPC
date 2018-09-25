#include "OAT.h"

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#define N 500
double gettimeofday_sec();

int main(void)
{
	int i,in;
	double A[N][N],B[N][N],C[N][N];
	double t1,t2;
	int iauto=1;
	in = N;
	#pragma OAT call OAT_ATset(OAT_ALL, OAT_AllRoutines)
	#pragma OAT call OAT_ATset(OAT_INSTALL, OAT_InstallRoutines)
	#pragma OAT call OAT_ATset(OAT_STATIC, OAT_StaticRoutines)
	#pragma OAT call OAT_ATset(OAT_DYNAMIC, OAT_DynamicRoutines)
	
	#pragma OAT OAT_DEBUG = 1
	InitData(A, B, C, in);
	if (iauto == 1){
	#pragma OAT OAT_NUMPROCS = 4
	#pragma OAT OAT_STARTTUNESIZE = 500
	#pragma OAT OAT_ENDTUNESIZE = 500
	#pragma OAT OAT_SAMPDIST = 100
	#pragma OAT call OAT_ATexec(OAT_INSTALL, OAT_InstallRoutines,A,B,C,in)
	}else{
	
	//InitData(A, B, C, in);
	//PrintData(A,B,C,in);

	t1 = gettimeofday_sec();
	MatMul(A, B, C, in);
	t2 = gettimeofday_sec();
	printf("time = %lf \n",t2-t1);
	
	OutputData(A, B, C, in);
	//PrintData(A,B,C,in);
	}
	
	return 0;
	
}


MatMul(double A[N][N],double B[N][N], double C[N][N], int n)
{
	int i,j,k;
	#pragma OAT call OAT_BPset("n")
	#pragma OAT install unroll (i) region start
	#pragma OAT name MyMatMul
	#pragma OAT fitting dspline
	#pragma OAT varied (i) from 1 to 16
	#pragma OAT debug (pp)
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
			}
		}
	}
	#pragma OAT install unroll (i) region end
	return 0;
}


InitData(double A[N][N],double B[N][N], double C[N][N], int n)
{
	int i,j;
	srand(time(NULL));


	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			C[i][j] = 0.0;
			B[i][j] = 0.0;
		}
	}
	
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			A[j][i] = rand()%10;
			
		}
	}
	for(i=0;i<n;i++)
		B[i][i] = 1;
}


OutputData(double A[N][N],double B[N][N], double C[N][N], int n)
{

	int i,j;
	
	for(i=0;i<n;i++){
		for(j=0;j<n;j++){
			if(A[i][j] != C[i][j]){
			printf("error!!\n");
			return;
			}
		}
	}
	printf("ok\n");
}


double gettimeofday_sec()
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + tv.tv_usec * 1e-6;
}


PrintData(double A[N][N],double B[N][N], double C[N][N], int n)
{
	int i,j,k;
	printf("A\n");
	for(i=0;i<n;i++){
		for(j=0;j<n;j++){
			printf("%.0lf ",A[i][j]);
		}printf("\n");
	}
	
	printf("B\n");
	for(i=0;i<n;i++){
		for(j=0;j<n;j++){
			printf("%.0lf ",B[i][j]);
		}printf("\n");
	}	
	
	printf("C\n");
	for(i=0;i<n;i++){
		for(j=0;j<n;j++){
			printf("%.0lf ",C[i][j]);
		}printf("\n");
	}	
}

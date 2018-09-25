#include <time.h>
#include <stdio.h>
#include <stdlib.h>

double OAT_Wtime()
{
	return (double)clock()/CLOCKS_PER_SEC;
}
//============================================================
//=== OAT_ATset
//============================================================
int OAT_ATset(int OAT_TYPE, char *OAT_Routines)
{
//==== All routines
	if(OAT_TYPE == 0){
		strcpy(OAT_Routines,"MyMatMul");
        OAT_iusw1_MyMatMul_flag = 0;
	}
//==== Install Optimization
	if(OAT_TYPE == 1){
		strcpy(OAT_Routines,"");
	}
//==== Before Execution-invocation Optimization Routines
	if(OAT_TYPE == 2){
		strcpy(OAT_Routines,"");
	}
//==== Run-time Optimization Routines
	if(OAT_TYPE == 3){
		OAT_DYNAMICTUNE = 0;
		strcpy(OAT_Routines,"MyMatMul");
        OAT_iusw1_MyMatMul_flag = 0;
	}
	return 0;
}

//============================================================
//=== OAT_SetParm
//============================================================
int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n_bpset,int *isw,double A[N][N],double B[N][N],double C[N][N],int n)
{
	int ibsw;
	char cbuf[500];
	char digit[20];
	FILE *fp;
	int oat_i,oat_j,oat_inum;

// ==== Install Optimization
	if(OAT_TYPE == 1){
	}
//=== end of OAT_Install
//-----------------------------------------------

//==== Before Execution-invocation Optimization
	if(OAT_TYPE == 2){
	}
//=== end of OAT_Static
//-----------------------------------------------

//==== Run-time Optimization
	if(OAT_TYPE == 3){
		if(OAT_DYNAMICTUNE){
			OAT_ATexecDynamicMyMatMul(OAT_Routines,isw,A,B,C,n);
		}else{
			*isw = 1;
		}
	}
//=== end of OAT_Dynamic
// -----------------------------------------------

	return 0;
}


//============================================================
//=== OAT_ATexec
//============================================================
int OAT_ATexec(int OAT_TYPE,char *OAT_Routines,double A[N][N],double B[N][N],double C[N][N],int n)
{
	if(getenv("OAT_EXEC") != 0){
		if(atoi(getenv("OAT_EXEC")) != 1){
			return 0;
		}
	}else if(getenv("OAT_ATEXEC") != 0){
		if(atoi(getenv("OAT_ATEXEC")) != 1){
			return 0;
		}
	}
//==== Install Optimization
	if(OAT_TYPE == 1){
	}

//==== Before Execution-invocation Optimization
	if(OAT_TYPE == 2){
	}

//==== Run-time Optimization
	if(OAT_TYPE == 3){
		OAT_DYNAMICTUNE = 1;
	}

	return 0;
}
//============================================================

//==============================================================
//==== Dynamic Optimization Routines
//==============================================================
int OAT_ATexecDynamicMyMatMul(char *OAT_Routines,int *iBestSw1,double A[N][N],double B[N][N],double C[N][N],int n)
{
	int iusw1;
	int F1[16];
	FILE *fp11;
	FILE *fp12;
	FILE *fp13;
	int iloop_n,iloop_dynamic;


	double t1, t2, t_all, bt;
	double dBestTime1;


//---- file create
//-----------------------------------------
	if(myid == 0){
		fp11 = fopen("OAT_DynamicMyMatMulParam.dat","wt");
		fprintf(fp11,"(MyMatMul\n");
	}
//----------------------------------------

//---- Start tune
//-----------------------------------------
	F1[0] = 1;
	F1[1] = 2;
	F1[2] = 3;
	F1[3] = 4;
	F1[4] = 5;
	F1[5] = 6;
	F1[6] = 7;
	F1[7] = 8;
	F1[8] = 9;
	F1[9] = 10;
	F1[10] = 11;
	F1[11] = 12;
	F1[12] = 13;
	F1[13] = 14;
	F1[14] = 15;
	F1[15] = 16;

	for(iloop_dynamic=0;iloop_dynamic<16;iloop_dynamic++){

		iusw1 = F1[iloop_dynamic];

		OAT_DynamicMyMatMul(C,A,B,OAT_STARTTUNESIZE,iusw1);
	}

	iloop_n=0;

		for(iloop_dynamic=0;iloop_dynamic<16;iloop_dynamic++){

			iusw1 = F1[iloop_dynamic];


			t1 = OAT_Wtime();

			OAT_DynamicMyMatMul(C,A,B,n,iusw1);

			t2 = OAT_Wtime();
			t_all = t2 - t1;
			bt = t_all;
			t_all = bt;

			if(OAT_DEBUG >= 1){
				if(myid == 0){
					printf("N=%d iusw1=%d %f\n",iloop_n, iusw1, t_all);
				}
			}

			if (iloop_dynamic == 0){
				dBestTime1 = t_all;
				*iBestSw1 = F1[0];
			}else{
				if(t_all < dBestTime1){
					dBestTime1 = t_all;
					*iBestSw1 = F1[iloop_dynamic];
				}
			}

		}

		if(OAT_DEBUG >= 1){
			if(myid == 0){
				printf("N=%d BestSw=%d\n",iloop_n,*iBestSw1);
			}
		}
//--- file write
		if(myid == 0){
			fprintf(fp11,"  (OAT_PROBSIZE %d\n", iloop_n);
			fprintf(fp11,"     (MyMatMul_I %d)\n",*iBestSw1);
			fprintf(fp11,"  )\n");
		}
//-----------------------------------------

//--- file close
	if (myid == 0){
		fprintf(fp11,")\n");
		fclose(fp11);
	}
//---------------------------------------------

	return 0;
}
//==== End of Dynamic Optimization Routines
//==============================================================



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
		strcpy(OAT_Routines,"MyMatMul");
        OAT_iusw1_MyMatMul_flag = 0;
	}
//==== Before Execution-invocation Optimization Routines
	if(OAT_TYPE == 2){
		strcpy(OAT_Routines,"");
	}
//==== Run-time Optimization Routines
	if(OAT_TYPE == 3){
		OAT_DYNAMICTUNE = 0;
		strcpy(OAT_Routines,"");
	}
	return 0;
}

//============================================================
//=== OAT_SetParm
//============================================================
int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n_bpset,int *isw)
{
	int ibsw;
	char cbuf[500];
	char digit[20];
	FILE *fp;
	int oat_i,oat_j,oat_inum;

// ==== Install Optimization
	if(OAT_TYPE == 1){
		if(strstr(OAT_Routines,"MyMatMul") != 0){
		if(OAT_iusw1_MyMatMul_flag == 0){
			OAT_iusw1_MyMatMul_flag = 1;
			*isw = -1;
			ibsw = 1;
//---- file create
//-----------------------------------------
			if(myid == 0){
				fp = fopen("OAT_InstallMyMatMulParam.dat","rt");
				if(fp == NULL){
					goto L102;
				}
//--- Seek object file
				if(fgets(cbuf,sizeof(cbuf),fp) == NULL){
					goto L100;
				}
				while(strstr(cbuf,"MyMatMul") == 0){
					if(fgets(cbuf,sizeof(cbuf),fp) == NULL){
						goto L100;
					}
				}
				while(1){
//--- Find problemsize
					if(fgets(cbuf,sizeof(cbuf),fp) == NULL){
						goto L100;
					}
					while (strstr(cbuf,"OAT_PROBSIZE") == 0){
						if(fgets(cbuf,sizeof(cbuf),fp) == NULL){
							goto L100;
						}
					}
// -------------------------------------------

//---- find space
					oat_i = (char *)strstr(cbuf,"OAT_PROBSIZE")-cbuf+12;
					while(cbuf[oat_i] == ' '){
						oat_i = oat_i + 1;
					}
//---------------------------------

//---- store digit and change it to integer
					oat_j = 0;
					while(cbuf[oat_i] > ' '){
						digit[oat_j] = cbuf[oat_i];
						oat_i = oat_i + 1;
						oat_j = oat_j + 1;
					}
//---------------------------------
					digit[oat_j] = '\0';
					oat_inum = atoi(digit);
//-----------------------------------------

//--- Find parameter
					if(fgets(cbuf,sizeof(cbuf),fp) == NULL){
						goto L100;
					}
					while (strstr(cbuf,"MyMatMul_I") == 0){
						if(fgets(cbuf,sizeof(cbuf),fp) == NULL){
							goto L100;
						}
					}
//-------------------------------------------

//---- find space
					oat_i = (char *)strstr(cbuf,"MyMatMul_I")-cbuf+10;
					while(cbuf[oat_i] == ' '){
						oat_i = oat_i + 1;
					}
//---------------------------------

//---- store digit and change it to integer
					oat_j = 0;
					while(cbuf[oat_i] > ' '){
						digit[oat_j] = cbuf[oat_i];
						oat_i = oat_i + 1;
						oat_j = oat_j + 1;
					}
//---------------------------------
					digit[oat_j] = '\0';
					ibsw = atoi(digit);

//--- inputed n_bpset is less than filed num?
					if (n_bpset <= oat_inum){
						*isw = ibsw;
						goto L100;
					}
				}
//=== end of seeking loop for n
L100:

//--- File close
				fclose(fp);

//--- This is last parameter
L102:
				if (*isw == -1){
					*isw = ibsw;
				}

			}
//=== end of myid == 1

//=== braodcast best param

		}
		return;
		}
//=== end of MyMatMul
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
int OAT_ATexec(int OAT_TYPE,char *OAT_Routines,int n,double A[N][N],double B[N][N],double C[N][N])
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
		if(strstr(OAT_Routines,"MyMatMul") != 0){
			OAT_ATexecInstallMyMatMul(OAT_Routines,n,A,B,C);
		}
	}

//==== Before Execution-invocation Optimization
	if(OAT_TYPE == 2){
	}

//==== Run-time Optimization
	if(OAT_TYPE == 3){
	}

	return 0;
}
//============================================================

//==============================================================
//==== Install Optimization Routines
//==============================================================
int OAT_ATexecInstallMyMatMul(char *OAT_Routines,int n,double A[N][N],double B[N][N],double C[N][N])
{
	int iusw1;
	int F1[64];
	FILE *fp11;
	FILE *fp12;
	FILE *fp13;
	int iloop_n,iloop_install;

	int iBestSw1;

	double t1, t2, t_all, bt;
	double dBestTime1;


//---- file create
//-----------------------------------------
	if(myid == 0){
		fp11 = fopen("OAT_InstallMyMatMulParam.dat","wt");
		fprintf(fp11,"(MyMatMul\n");
		fprintf(fp11,"  (OAT_NUMPROCS %d)\n",OAT_NUMPROCS);
		fprintf(fp11,"  (OAT_SAMPDIST %d)\n",OAT_SAMPDIST);
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
	F1[16] = 17;
	F1[17] = 18;
	F1[18] = 19;
	F1[19] = 20;
	F1[20] = 21;
	F1[21] = 22;
	F1[22] = 23;
	F1[23] = 24;
	F1[24] = 25;
	F1[25] = 26;
	F1[26] = 27;
	F1[27] = 28;
	F1[28] = 29;
	F1[29] = 30;
	F1[30] = 31;
	F1[31] = 32;
	F1[32] = 33;
	F1[33] = 34;
	F1[34] = 35;
	F1[35] = 36;
	F1[36] = 37;
	F1[37] = 38;
	F1[38] = 39;
	F1[39] = 40;
	F1[40] = 41;
	F1[41] = 42;
	F1[42] = 43;
	F1[43] = 44;
	F1[44] = 45;
	F1[45] = 46;
	F1[46] = 47;
	F1[47] = 48;
	F1[48] = 49;
	F1[49] = 50;
	F1[50] = 51;
	F1[51] = 52;
	F1[52] = 53;
	F1[53] = 54;
	F1[54] = 55;
	F1[55] = 56;
	F1[56] = 57;
	F1[57] = 58;
	F1[58] = 59;
	F1[59] = 60;
	F1[60] = 61;
	F1[61] = 62;
	F1[62] = 63;
	F1[63] = 64;

	for(iloop_install=0;iloop_install<64;iloop_install++){

		iusw1 = F1[iloop_install];

		OAT_InstallMyMatMul(OAT_STARTTUNESIZE,A,C,B,iusw1);
	}

	for(iloop_n=OAT_STARTTUNESIZE;iloop_n<=OAT_ENDTUNESIZE;iloop_n+=OAT_SAMPDIST){

		for(iloop_install=0;iloop_install<64;iloop_install++){

			iusw1 = F1[iloop_install];

			n = iloop_n;

			t1 = OAT_Wtime();

			OAT_InstallMyMatMul(n,A,C,B,iusw1);

			t2 = OAT_Wtime();
			t_all = t2 - t1;
			bt = t_all;
			t_all = bt;

			if(OAT_DEBUG >= 1){
				if(myid == 0){
					printf("N=%d iusw1=%d %f\n",iloop_n, iusw1, t_all);
				}
			}

			if (iloop_install == 0){
				dBestTime1 = t_all;
				iBestSw1 = F1[0];
			}else{
				if(t_all < dBestTime1){
					dBestTime1 = t_all;
					iBestSw1 = F1[iloop_install];
				}
			}

		}

		if(OAT_DEBUG >= 1){
			if(myid == 0){
				printf("N=%d BestSw=%d\n",iloop_n,iBestSw1);
			}
		}
//--- file write
		if(myid == 0){
			fprintf(fp11,"  (OAT_PROBSIZE %d\n", iloop_n);
			fprintf(fp11,"     (MyMatMul_I %d)\n",iBestSw1);
			fprintf(fp11,"  )\n");
		}
//-----------------------------------------
	}

//--- file close
	if (myid == 0){
		fprintf(fp11,")\n");
		fclose(fp11);
	}
//---------------------------------------------

	return 0;
}
//==== End of Install Optimization Routines
//==============================================================



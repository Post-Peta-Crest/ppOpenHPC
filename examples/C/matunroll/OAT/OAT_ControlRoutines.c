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
		if(strstr(OAT_Routines,"MyMatMul") != 0){
			OAT_ATexecInstallMyMatMul(OAT_Routines,A,B,C,n);
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
int OAT_ATexecInstallMyMatMul(char *OAT_Routines,double A[N][N],double B[N][N],double C[N][N],int n)
{
	int iusw1;
	int F1[16];
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

	for(iloop_install=0;iloop_install<16;iloop_install++){

		iusw1 = F1[iloop_install];

		OAT_InstallMyMatMul(C,A,B,OAT_STARTTUNESIZE,iusw1);
	}

	for(iloop_n=OAT_STARTTUNESIZE;iloop_n<=OAT_ENDTUNESIZE;iloop_n+=OAT_SAMPDIST){

		for(iloop_install=0;iloop_install<16;iloop_install++){

			iusw1 = F1[iloop_install];

			n = iloop_n;

			iBestSw1 = dspgiv(16,A,B,C,n);
			break;
			t1 = OAT_Wtime();

			OAT_InstallMyMatMul(C,A,B,n,iusw1);

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


#include <stdio.h>
#include <stdlib.h>

#define DEBUG 2

int dspgiv(int npN,double A[N][N],double B[N][N],double C[N][N],int n)
{
	int nn=npN*3;
	double DE[nn+1][nn+1];
	double R[nn+1][nn+1];
	double G[nn+1][nn+1];
	double r,c,s;
	int p,q,q2;
	double data;
	int count=1;
	double x[n];
	double temp;
	int temp2,bestP=0,nextP,prebestP;
	double select2[npN][2];
	int h[npN],kk=0;
	double alfa=0.1;
	double t1,t2;
	int F2[4];
	int i,j,k,i4;

	FILE *fpA;
	char *fname = "d-SplineData.csv";
	fpA = fopen(fname,"w");

	for(i=0;i<nn+1;i++){
		for(j=0;j<nn+1;j++){
			DE[i][j]=0;
			R[i][j]=0;
		}
	}

	for(i=0;i<nn-2;i++){
		DE[i][i]=1*alfa;
		DE[i][i+1]=-2*alfa;
		DE[i][i+2]=1*alfa;
	}

	for(i=0;i<=npN;i++){
		h[i]=npN+1;
	}

	F2[0]=1;
	F2[1]=(2+npN)/3;
	F2[2]=(1+2*npN)/3;
	F2[3]=npN;

 	p=nn;

	for(i4=0;i4<4;i4++){
		q=F2[i4];

		t1 = OAT_Wtime();
		OAT_InstallMyMatMul(C, A, B, n, q);

		t2 = OAT_Wtime();
		data = t2 - t1;

		printf("\n---para is %d time is %lf--- \n",q,data);

		q--;
		q2=q*3;
		h[kk]=q;

		DE[p][q2]=1;
		DE[p][nn]=data;

		for(q2=q2;q2<nn;q2++){

			for(i=0;i<nn+1;i++){
				G[i][i]=0;
			}

			G[nn][nn]=1;
			r=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );

			if(r==0)
				break;

			c=DE[q2][q2]/r;
			s=DE[p][q2]/r;

			G[p][p]=c;
			G[q2][q2]=c;

			G[q2][p]=s;
			G[p][q2]=-1*s;

			R[q2][q2]+=G[q2][q2]*DE[q2][q2];
			R[q2][q2]+=G[q2][nn]*DE[nn][q2];
			if( (q2+1)!=nn && (q2+1)<=nn ){
			R[q2][q2+1]+=G[q2][q2]*DE[q2][q2+1];
			R[q2][q2+1]+=G[q2][nn]*DE[nn][q2+1];
			}

			if( (q2+2)!=nn && (q2+2)<=nn ){
			R[q2][q2+2]+=G[q2][q2]*DE[q2][q2+2];
			R[q2][q2+2]+=G[q2][nn]*DE[nn][q2+2];
			}

			R[q2][nn]+=G[q2][q2]*DE[q2][nn];
			R[q2][nn]+=G[q2][nn]*DE[nn][nn];

			R[nn][q2]+=G[nn][q2]*DE[q2][q2];
			R[nn][q2]+=G[nn][nn]*DE[nn][q2];

			if( (q2+1)!=nn && (q2+1)<=nn ){
			R[nn][q2+1]+=G[nn][q2]*DE[q2][q2+1];
			R[nn][q2+1]+=G[nn][nn]*DE[nn][q2+1];
			}

			if( (q2+2)!=nn && (q2+2)<=nn ){
			R[nn][q2+2]+=G[nn][q2]*DE[q2][q2+2];
			R[nn][q2+2]+=G[nn][nn]*DE[nn][q2+2];
			}

			R[nn][nn]+=G[nn][q2]*DE[q2][nn];
			R[nn][nn]+=G[nn][nn]*DE[nn][nn];

			DE[q2][q2]=R[q2][q2];
			DE[q2][q2+1]=R[q2][q2+1];
			DE[q2][q2+2]=R[q2][q2+2];
			DE[q2][nn]=R[q2][nn];

			DE[nn][q2]=R[nn][q2];
			DE[nn][q2+1]=R[nn][q2+1];
			DE[nn][q2+2]=R[nn][q2+2];
			DE[nn][nn]=R[nn][nn];

			R[q2][q2]=0;
			R[q2][q2+1]=0;
			R[q2][q2+2]=0;
			R[q2][nn]=0;

			R[nn][q2]=0;
			R[nn][q2+1]=0;
			R[nn][q2+2]=0;
			R[nn][nn]=0;
		}

		kk++;
	}

	for(i=0;i<nn;i++){
		x[i]=DE[i][nn];
	}

	for(i=nn-1; i>=0;i--){
		for(j=i+1;j<nn;j++){
			x[i]-=DE[i][j]*x[j];
		}
		x[i] /= DE[i][i];
	}

		if(DEBUG>0){
			printf("\nestimation\n");
			for(i=0;i<npN;i++){
				printf("[%d],%lf\n",i+1,x[i*3]);
				}printf("\n");
		}

	temp=x[0];
	bestP=0;

	for(i=0;i<npN;i++){
		if(x[i*3]<temp){
			temp=x[i*3];
			bestP=i;
		}
	}

	printf("\nbest para = %d time = %lf\n",bestP+1,x[bestP*3]);

	for(i=0;i<npN;i++){
		select2[i][1]=i;
		select2[i][0]=0;
	}

	for(i=1;i<npN-1;i++){
		select2[i][0]=fabs( x[i-1] - 2*x[i] + x[i+1] );
	}

	for(i=1;i<npN-1;i++){
		select2[i][0]=fabs( x[i-3] - 2*x[i] + x[i+3] );
	}

	for(i=0 ; i<npN-1 ; i++){
		for(j=npN-1 ; j>i ; j--){
			if(select2[j-1][0] < select2[j][0]){
				temp=select2[j][0];
				temp2=select2[j][1];
				select2[j][0]=select2[j-1][0];
				select2[j][1]=select2[j-1][1];
				select2[j-1][0]=temp;
				select2[j-1][1]=temp2;
			}
		}
	}

	for(i=0;i<npN;i++){
		if(bestP==h[i]){
			for(j=0;j<npN;j++){
				for(k=0;k<npN;k++){
					if(select2[j][1]==h[k]){
						break;
					}
					if(k+1==npN){
						nextP=select2[j][1];
						goto OUT1;
					}
				}
			}
			break;
		}
		else{
			nextP=bestP;
		}
	}
	OUT1:
	printf("nextP=%d bestP=%d \n\n",nextP+1,bestP+1);

	prebestP=bestP;


	for(i=0;i<nn;i++){
		fprintf(fpA,"%lf\n",x[i]);
	}
	fprintf(fpA,"end\n\n");

	printf("\n----------end 4 point---------- \n");

	do{

		q=nextP;
		q2=q*3;

		t1 = omp_get_wtime();
		OAT_InstallMyMatMul(C, A, B, n, q+1);
		t2 = omp_get_wtime();

		data = t2 - t1;

		printf("\n---para is %d time is %lf--- \n",q+1,data);

		h[kk]=q;
		DE[p][q2]=1;
		DE[p][nn]=data;

		for(q2=q2;q2<nn;q2++){

			for(i=0;i<nn+1;i++){
				G[i][i]=0;
			}

			G[nn][nn]=1;
			r=sqrt( (DE[q2][q2])*(DE[q2][q2]) + (DE[p][q2])*(DE[p][q2]) );

			if(r==0)
				break;

			c=DE[q2][q2]/r;
			s=DE[p][q2]/r;

			G[p][p]=c;
			G[q2][q2]=c;

			G[q2][p]=s;
			G[p][q2]=-1*s;

			R[q2][q2]+=G[q2][q2]*DE[q2][q2];
			R[q2][q2]+=G[q2][nn]*DE[nn][q2];
			if( (q2+1)!=nn && (q2+1)<=nn ){
			R[q2][q2+1]+=G[q2][q2]*DE[q2][q2+1];
			R[q2][q2+1]+=G[q2][nn]*DE[nn][q2+1];
			}

			if( (q2+2)!=nn && (q2+2)<=nn ){
			R[q2][q2+2]+=G[q2][q2]*DE[q2][q2+2];
			R[q2][q2+2]+=G[q2][nn]*DE[nn][q2+2];
			}

			R[q2][nn]+=G[q2][q2]*DE[q2][nn];
			R[q2][nn]+=G[q2][nn]*DE[nn][nn];

			R[nn][q2]+=G[nn][q2]*DE[q2][q2];
			R[nn][q2]+=G[nn][nn]*DE[nn][q2];

			if( (q2+1)!=nn && (q2+1)<=nn ){
			R[nn][q2+1]+=G[nn][q2]*DE[q2][q2+1];
			R[nn][q2+1]+=G[nn][nn]*DE[nn][q2+1];
			}

			if( (q2+2)!=nn && (q2+2)<=nn ){
			R[nn][q2+2]+=G[nn][q2]*DE[q2][q2+2];
			R[nn][q2+2]+=G[nn][nn]*DE[nn][q2+2];
			}

			R[nn][nn]+=G[nn][q2]*DE[q2][nn];
			R[nn][nn]+=G[nn][nn]*DE[nn][nn];

			DE[q2][q2]=R[q2][q2];
			DE[q2][q2+1]=R[q2][q2+1];
			DE[q2][q2+2]=R[q2][q2+2];
			DE[q2][nn]=R[q2][nn];

			DE[nn][q2]=R[nn][q2];
			DE[nn][q2+1]=R[nn][q2+1];
			DE[nn][q2+2]=R[nn][q2+2];
			DE[nn][nn]=R[nn][nn];

			R[q2][q2]=0;
			R[q2][q2+1]=0;
			R[q2][q2+2]=0;
			R[q2][nn]=0;

			R[nn][q2]=0;
			R[nn][q2+1]=0;
			R[nn][q2+2]=0;
			R[nn][nn]=0;
		}


		for(i=0;i<nn;i++){
			x[i]=DE[i][nn];
		}

		for(i=nn-1; i>=0;i--){
			for(j=i+1;j<nn;j++){
				x[i]-=DE[i][j]*x[j];
			}
			x[i] /= DE[i][i];
		}

		if(DEBUG>0){
			printf("\nestimation\n");
			for(i=0;i<npN;i++){
				printf("[%d],%lf\n",i+1,x[i*3]);
			}
			printf("\n");
		}

		temp=x[0];
		bestP=0;

		for(i=0;i<npN;i++){
			if(x[i*3]<temp){
				temp=x[i*3];
				bestP=i;
			}
		}

		printf("\nbest para = %d time = %lf\n",bestP+1,x[bestP*3]);

		for(i=0;i<npN;i++){
			select2[i][1]=i;
			select2[i][0]=0;
		}

		for(i=1;i<npN-1;i++){
			select2[i][0]=fabs( x[i-3] - 2*x[i] + x[i+3] );
		}

		for(i=0 ; i<npN-1 ; i++){
			for(j=npN-1 ; j>i ; j--){
				if(select2[j-1][0] < select2[j][0]){
					temp=select2[j][0];
					temp2=select2[j][1];
					select2[j][0]=select2[j-1][0];
					select2[j][1]=select2[j-1][1];
					select2[j-1][0]=temp;
					select2[j-1][1]=temp2;
			}
			}
		}

		for(i=0;i<npN;i++){
			if(bestP==h[i]){
				for(j=0;j<npN;j++){
					for(k=0;k<npN;k++){
						if(select2[j][1]==h[k]){
							break;
						}
						if(k+1==npN){
							nextP=select2[j][1];
							goto OUT2;
						}
					}
				}
				break;
			}
			else{
				nextP=bestP;
			}
		}
		OUT2:
		printf("nextP=%d bestP=%d \n\n",nextP+1,bestP+1);

		if(prebestP==bestP){
			count++;
		}
		else{
			count=1;
		}

		prebestP=bestP;

		kk++;

		for(i=0;i<nn;i++){
			fprintf(fpA,"%lf\n",x[i]);
		}
		fprintf(fpA,"end\n\n");

		printf("count = %d \n",count);

	}while(count<3);

	printf("count end\n");
	printf("usedParaNums = %d\n",kk);

	fclose(fpA);

	return bestP+1;
}

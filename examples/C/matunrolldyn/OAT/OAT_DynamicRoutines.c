
int OAT_DynamicMyMatMul(double C[N][N],double A[N][N],double B[N][N],int n,int iusw1)
{
	switch(iusw1){
	case 1:
		OAT_DynamicMyMatMul_1(C, A, B, n);
		break;
	case 2:
		OAT_DynamicMyMatMul_2(C, A, B, n);
		break;
	case 3:
		OAT_DynamicMyMatMul_3(C, A, B, n);
		break;
	case 4:
		OAT_DynamicMyMatMul_4(C, A, B, n);
		break;
	case 5:
		OAT_DynamicMyMatMul_5(C, A, B, n);
		break;
	case 6:
		OAT_DynamicMyMatMul_6(C, A, B, n);
		break;
	case 7:
		OAT_DynamicMyMatMul_7(C, A, B, n);
		break;
	case 8:
		OAT_DynamicMyMatMul_8(C, A, B, n);
		break;
	case 9:
		OAT_DynamicMyMatMul_9(C, A, B, n);
		break;
	case 10:
		OAT_DynamicMyMatMul_10(C, A, B, n);
		break;
	case 11:
		OAT_DynamicMyMatMul_11(C, A, B, n);
		break;
	case 12:
		OAT_DynamicMyMatMul_12(C, A, B, n);
		break;
	case 13:
		OAT_DynamicMyMatMul_13(C, A, B, n);
		break;
	case 14:
		OAT_DynamicMyMatMul_14(C, A, B, n);
		break;
	case 15:
		OAT_DynamicMyMatMul_15(C, A, B, n);
		break;
	case 16:
		OAT_DynamicMyMatMul_16(C, A, B, n);
		break;
	}
	return 0;
}

int OAT_DynamicMyMatMul_1(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;


	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_2(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 2) * 2;

	for(i = 0 ; i < im ; i+=2){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];

			}
		}
	}
	if ((n % 2) != 0){
		for(i =(n/2)*2 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_3(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 3) * 3;

	for(i = 0 ; i < im ; i+=3){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];

			}
		}
	}
	if ((n % 3) != 0){
		for(i =(n/3)*3 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_4(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 4) * 4;

	for(i = 0 ; i < im ; i+=4){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];

			}
		}
	}
	if ((n % 4) != 0){
		for(i =(n/4)*4 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_5(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 5) * 5;

	for(i = 0 ; i < im ; i+=5){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];

			}
		}
	}
	if ((n % 5) != 0){
		for(i =(n/5)*5 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_6(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 6) * 6;

	for(i = 0 ; i < im ; i+=6){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];

			}
		}
	}
	if ((n % 6) != 0){
		for(i =(n/6)*6 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_7(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 7) * 7;

	for(i = 0 ; i < im ; i+=7){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];

			}
		}
	}
	if ((n % 7) != 0){
		for(i =(n/7)*7 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_8(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 8) * 8;

	for(i = 0 ; i < im ; i+=8){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];

			}
		}
	}
	if ((n % 8) != 0){
		for(i =(n/8)*8 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_9(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 9) * 9;

	for(i = 0 ; i < im ; i+=9){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];

			}
		}
	}
	if ((n % 9) != 0){
		for(i =(n/9)*9 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_10(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 10) * 10;

	for(i = 0 ; i < im ; i+=10){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];

			}
		}
	}
	if ((n % 10) != 0){
		for(i =(n/10)*10 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_11(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 11) * 11;

	for(i = 0 ; i < im ; i+=11){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];
				C[(i+10)][j] += A[(i+10)][k] * B[k][j];

			}
		}
	}
	if ((n % 11) != 0){
		for(i =(n/11)*11 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_12(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 12) * 12;

	for(i = 0 ; i < im ; i+=12){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];
				C[(i+10)][j] += A[(i+10)][k] * B[k][j];
				C[(i+11)][j] += A[(i+11)][k] * B[k][j];

			}
		}
	}
	if ((n % 12) != 0){
		for(i =(n/12)*12 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_13(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 13) * 13;

	for(i = 0 ; i < im ; i+=13){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];
				C[(i+10)][j] += A[(i+10)][k] * B[k][j];
				C[(i+11)][j] += A[(i+11)][k] * B[k][j];
				C[(i+12)][j] += A[(i+12)][k] * B[k][j];

			}
		}
	}
	if ((n % 13) != 0){
		for(i =(n/13)*13 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_14(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 14) * 14;

	for(i = 0 ; i < im ; i+=14){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];
				C[(i+10)][j] += A[(i+10)][k] * B[k][j];
				C[(i+11)][j] += A[(i+11)][k] * B[k][j];
				C[(i+12)][j] += A[(i+12)][k] * B[k][j];
				C[(i+13)][j] += A[(i+13)][k] * B[k][j];

			}
		}
	}
	if ((n % 14) != 0){
		for(i =(n/14)*14 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_15(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 15) * 15;

	for(i = 0 ; i < im ; i+=15){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];
				C[(i+10)][j] += A[(i+10)][k] * B[k][j];
				C[(i+11)][j] += A[(i+11)][k] * B[k][j];
				C[(i+12)][j] += A[(i+12)][k] * B[k][j];
				C[(i+13)][j] += A[(i+13)][k] * B[k][j];
				C[(i+14)][j] += A[(i+14)][k] * B[k][j];

			}
		}
	}
	if ((n % 15) != 0){
		for(i =(n/15)*15 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}


int OAT_DynamicMyMatMul_16(double C[N][N],double A[N][N],double B[N][N],int n)
{
	int i, j, k;
	int im;

	im = (n / 16) * 16;

	for(i = 0 ; i < im ; i+=16){
		for(j = 0 ; j < n ; j++){
			for(k = 0 ; k < n ; k++){
				C[i][j] += A[i][k] * B[k][j];
				C[(i+1)][j] += A[(i+1)][k] * B[k][j];
				C[(i+2)][j] += A[(i+2)][k] * B[k][j];
				C[(i+3)][j] += A[(i+3)][k] * B[k][j];
				C[(i+4)][j] += A[(i+4)][k] * B[k][j];
				C[(i+5)][j] += A[(i+5)][k] * B[k][j];
				C[(i+6)][j] += A[(i+6)][k] * B[k][j];
				C[(i+7)][j] += A[(i+7)][k] * B[k][j];
				C[(i+8)][j] += A[(i+8)][k] * B[k][j];
				C[(i+9)][j] += A[(i+9)][k] * B[k][j];
				C[(i+10)][j] += A[(i+10)][k] * B[k][j];
				C[(i+11)][j] += A[(i+11)][k] * B[k][j];
				C[(i+12)][j] += A[(i+12)][k] * B[k][j];
				C[(i+13)][j] += A[(i+13)][k] * B[k][j];
				C[(i+14)][j] += A[(i+14)][k] * B[k][j];
				C[(i+15)][j] += A[(i+15)][k] * B[k][j];

			}
		}
	}
	if ((n % 16) != 0){
		for(i =(n/16)*16 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(k = 0 ; k < n ; k++){
					C[i][j] += A[i][k] * B[k][j];
				}
			}
		}
	}

	return 0;
}



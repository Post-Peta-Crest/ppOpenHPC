//============================================================
//=== OAT_Routines Header
//============================================================
int OAT_DynamicMyMatMul(double C[N][N],double A[N][N],double B[N][N],int n,int iusw1);
int OAT_DynamicMyMatMul_1(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_2(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_3(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_4(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_5(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_6(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_7(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_8(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_9(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_10(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_11(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_12(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_13(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_14(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_15(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_DynamicMyMatMul_16(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_ATset(int OAT_TYPE, char *OAT_Routines);
int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n_bpset,int *isw,double A[N][N],double B[N][N],double C[N][N],int n);
int OAT_ATexec(int OAT_TYPE,char *OAT_Routines,double A[N][N],double B[N][N],double C[N][N],int n);
int OAT_ATexecDynamicMyMatMul(char *OAT_Routines,int *iBestSw1,double A[N][N],double B[N][N],double C[N][N],int n);

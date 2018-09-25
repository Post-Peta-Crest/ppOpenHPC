//============================================================
//=== OAT_Routines Header
//============================================================
int OAT_InstallMyMatMul(double C[N][N],double A[N][N],double B[N][N],int n,int iusw1);
int OAT_InstallMyMatMul_1(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_2(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_3(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_4(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_5(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_6(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_7(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_8(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_9(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_10(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_11(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_12(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_13(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_14(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_15(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_InstallMyMatMul_16(double C[N][N],double A[N][N],double B[N][N],int n);
int OAT_ATset(int OAT_TYPE, char *OAT_Routines);
int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n_bpset,int *isw);
int OAT_ATexec(int OAT_TYPE,char *OAT_Routines,double A[N][N],double B[N][N],double C[N][N],int n);
int OAT_ATexecInstallMyMatMul(char *OAT_Routines,double A[N][N],double B[N][N],double C[N][N],int n);

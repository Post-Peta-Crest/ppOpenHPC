/* =====================================================================*
 *                                                                     *
 *   Software Name : ppOpen-AT                                         *
 *         Version : 0.2                                               *
 *                                                                     *
 *   License                                                           *
 *     This file is part of ppOpen-AT.                                 *
 *     ppOpen-AT is a free software, you can use it under the terms    *
 *     of The MIT License (MIT). See LICENSE file and User's guide     *
 *     for more details.                                               *
 *                                                                     *
 *   ppOpen-HPC project:                                               *
 *     Open Source Infrastructure for Development and Execution of     *
 *     Large-Scale Scientific Applications on Post-Peta-Scale          *
 *     Supercomputers with Automatic Tuning (AT).                      *
 *                                                                     *
 *   Organizations:                                                    *
 *     The University of Tokyo                                         *
 *       - Information Technology Center                               *
 *       - Atmosphere and Ocean Research Institute (AORI)              *
 *       - Graduate School of Interdisciplinary Information Studies    *
 *         /Earthquake Research Institute (ERI)                        *
 *       - Graduate School of Frontier Science                         *
 *     Kyoto University                                                *
 *       - Academic Center for Computing and Media Studies             *
 *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
 *                                                                     *
 *   Sponsorship:                                                      *
 *     Japan Science and Technology Agency (JST), Basic Research       *
 *     Programs: CREST, Development of System Software Technologies    *
 *     for post-Peta Scale High Performance Computing.                 *
 *                                                                     *
 *   Copyright (c) 2012                                                *
 *      Information Technology Center, The University of Tokyo         *
 *                                                                     *
 *===================================================================== */
// ---------------------------------------------------------------------------
#include "main.h"

#include "TuneRegion.h"
#include "Visual.h"

/* ---------------------------------------------------------------------------- */
//
// 概要
//
// メインプログラム
//
// 作成者
//
/* ---------------------------------------------------------------------------- */

TMainF *MainF = NULL;

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
// メイン
//
// 2.パラメタ説明
// int argc コマンドライン引数の数
// char *argc[] コマンドライン引数
//
// 3.概要
// 生成時に呼ばれ、コマンドラインの解析とMainF作成とコード生成を行う。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
	string s;
	string OatPath_EnvStr;
	string SrcFileName,DirName;
	char *EnvStr;
	int i;

	EnvStr = getenv("OAT_PATH");
   	if (EnvStr != NULL) {
		OatPath_EnvStr = EnvStr;
	}
	else {
		OatPath_EnvStr = "";
	}

#ifdef _DEBUG // TEST 用

//#define BASE_DIR "c:/MinGW/msys/1.0/home/sakamoto/ppOpen-AT/"

#define BASE_DIR "c:/ppOpenAT/"	// 新しいテスト用のパス。


//
// 2016のテストデータ
//
	// C言語でのLoopFusion
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_C/LoopFusionSplit1_C/source.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_C/LoopFusionSplit2_C/source.c"};

	// シンプルなサンプル
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues1/simple1/simple1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues1/simple4a/simple4a.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues1/simple4b/simple4b.f"};


	// Fortran言語でのLoopFusion+Simle
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_F90/LoopFusionSplit1/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_F90/LoopFusionSplit2/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_F90/sample1/source.f90"};

	// select内にcallを含んだ場合の処理（新規）
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/call_in_select/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/call_in_select/source.f90"};
//	char* test_argv[] = {"test.exe " ,"-insert_module_head=source2.f", BASE_DIR "examples/call_in_select/source.f90"
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/call_in_select/source.f90"
//				, BASE_DIR "examples/call_in_select/source2.f90"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/call_in_select/source.f90"};
	// replase リプレーステスト（新規）
//-	char* test_argv[] = {"test.exe " , BASE_DIR "examples/replase1/source.f"};
	// 部分的に一致する名称ががある場合の問題（新規）
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/samename/source.f"};


	// GWV テスト
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/gwv1/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/gwv1_c/source.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/gwv2/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/gwv3/source.f"};
	// Implicit テスト
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/implicit/source.f"};
	// List テスト
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/List/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/List_C/source.c"};

	// unroll テスト
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/unroll/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/unroll_C/source.c"};

	// vsd テスト
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/vsd1/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/vsd2/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/vsd3/source.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/vsd1_C/source.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/vsd2_C/source.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/issues_OpenACC/vsd3_C/source.c"};

	// prefeatch sample
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/prefetch_sample/srcc-a/solver_ICCG_mc2_p3_AT.f"};
//	char* test_argv[] = {"test.exe " , "-free", BASE_DIR "examples/prefetch_sample/srcc-a/solver_ICCG_mc2_p3_AT.f"};

	// FDM
#if 0
	char* test_argv[] = {"oat.exe" , "-mpi" , BASE_DIR "examples/ppohFDM_AT_1.0.0/After/3.hybrid_AT/seism3d3n.f90"
											, BASE_DIR "examples/ppohFDM_AT_1.0.0/After/3.hybrid_AT/m_stress.f90"
											, BASE_DIR "examples/ppohFDM_AT_1.0.0/After/3.hybrid_AT/m_velocity.f90"
											, BASE_DIR "examples/ppohFDM_AT_1.0.0/After/3.hybrid_AT/m_pfd3d.f90"};
#endif

// exsampes_0.1.0 Fortran
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Dynamic_Set1/dynamic1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Dynamic_Set2/dynamic2.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Fit_Sample1/matmal_fit.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Fit_Sample2/matmal_fit2.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/MGSker/MGSker.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/SetDebug/debg_num.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/SetNum/number.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/TDRker/TRDker.f"};

//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_i_1_to64/matmul1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_ijk_1_to_4/matmul1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_j_1_to64/matmul1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_k_1_to64/matmul1.f"};


//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_Select_1/select1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_Select_1/select1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_Select_2/select2.f"};
//	char* test_argv[] = {"test.exe ", "-free", BASE_DIR "examples/F90/Test_Select_2/select2.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/Test_Select_3/param1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/F90/TrdMatVec/TrdMatVec.f"};

// exsampes_0.1.0 C
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C/matunroll/matunroll.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C/Test_i_1_to64/matmul.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C/matunrolldyn/matunrolldyn.c"};

// formatText
	char* test_argv[] = {"test.exe " , BASE_DIR "examples/formattest/simple1/simple1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/formattest/simple1/simple1.f90"};

// 動作テスト用。
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/matunroll/matunroll.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/matunroll/matunroll.c",
//		BASE_DIR "examples/C_test/matunroll/sub.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/matunroll/matunroll.f",
//		BASE_DIR "examples/C_test/matunroll/sub.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/List_C/source.c"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/simple1/simple1.f"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/simple1/simple1.f77"};
//	char* test_argv[] = {"test.exe " ,"-free", BASE_DIR "examples/C_test/simple1/simple1.f"};
//	char* test_argv[] = {"test.exe " ,"-free", BASE_DIR "examples/C_test/simple1/simple1.f90"};
//	char* test_argv[] = {"test.exe " , BASE_DIR "examples/C_test/simple1/simple1.f90"};


// TEST
//	char* test_argv[] = {"oat.exe" , "-mpi" ,"-omp_inner" , BASE_DIR "F90examples/test/test.f90"};
//	char* test_argv[] = {"oat.exe" , "-mpi" ,"-omp_inner" , BASE_DIR "F90examples/ppOhBEM_0.1.0targetAT/bem-bb-fw-dense-0.1.0.f90"};

	argc = sizeof(test_argv) / sizeof(test_argv[0]);
	// argc = 2;
	argv = test_argv;

#endif
	MainF = new TMainF;
	MainF->GetCommandLineArg(argc, argv);
	if (MainF->LogFp == NULL) {
		if(MainF->SrcFileNameList.Count != 0){
			SrcFileName = MainF->SrcFileNameList.Strings[0];
			DirName = SrcFileName.substr(0, SrcFileName.rfind("/") + 1);
		}else{
			DirName = "./";
		}
		s = DirName + "OATLog.txt";
		MainF->LogFp = fopen(s.c_str(), "wt");
		if (MainF->LogFp == NULL) {
			MainF->ErrMessage(-1, "LogFile " + s + " Open error.");
		}
	}
	// printf("--- ppOpenAT version 1.0.0 ---\n");	// Version String
	// printf("--- ppOpenAT version 1.0.1 ---\n");	// Version String
	// printf("--- ppOpenAT version 1.0.2 ---\n");	// Version String
	// printf("--- ppOpenAT version 1.0.3 ---\n");	// Version String 2012/05/17
	// MainF->print("--- ppOpenAT version 1.0.4 ---"); // Version String 2012/09/27
	// MainF->print("--- ppOpenAT version 1.0.5 ---"); // Version String 2013/03/21 Fortran90対応
	//	MainF->print("--- ppOpenAT version 1.0.6 ---"); // Version String 2013/07/24 Update
	// MainF->print("--- ppOpenAT version 0.2.0 ---"); // Version String 2013/08/11 Update
	//	MainF->print("--- ppOpenAT version 0.2.2 ---"); // Version String 2015/03/01 Update
//	MainF->print("--- ppOpenAT version 0.2.3 ---"); // Version String 2016/03/08 Update
	MainF->print("--- ppOpenAT version 1.0.0 ---"); // Version String 2016/03/29 Update
	MainF->print("OAT_PATH = \""+OatPath_EnvStr+"\"");
	s = "";
	for (int i = 0; i < argc; i++) {
		s += argv[i] + (string)" ";
	}
	MainF->print("CommandLine = "+s);
	MainF->CodeGen();

	delete MainF;
	MainF = NULL;

	//
	// 環境変数指定があれば、そちらのパスに対しても実行する。
	//
	if (OatPath_EnvStr != "") {
		MainF = new TMainF;
		MainF->GetCommandLineArg(argc, argv);
		for (i = 0; i < MainF->SrcFileNameList.Count; i++) {
			SrcFileName = MainF->SrcFileNameList.Strings[i];
			s = SrcFileName.substr(SrcFileName.rfind("/") + 1);
			MainF->SrcFileNameList.Strings[i] = OatPath_EnvStr + "/" + s;
		}
		MainF->CodeGen();
		delete MainF;
		MainF = NULL;
	}
#ifdef _DEBUG
	getchar();
#endif
	return 0;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
// MainF生成
//
// 2.パラメタ説明
//
// 3.概要
// 生成時に呼ばれ変数の初期化を行う。
//                                       r
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TMainF::TMainF() {
	LogFp = NULL;
	VisualF = false;
	DebugF = true;
	EECntlF = false;
	CloseReqF = false;
	EndF = false;
	NoMPIF = false;
	MPIF = false;
	OMP_OuterF = false;
	OMP_InnerF = false;
	TimeFunc = "";
	my_timer_start = "";
	my_timer_stop = "";
	cc_option_str = "";
	TokStrList = new TStringList;
	TokenList = new TList;
	ValDataList = new TList;
	TuneRegionList = new TList;
	Call_SetParam_Script = NULL; // #pragma call OAT_SetParam() のスクリプト
	Call_ATExec_Script = NULL; // #pragma call OAT_ATexe() のスクリプト
	Call_ATExec_ArgList = new TStringList;
    IncludeCodeInContainsF = false;    // Include code in Contains
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// 破棄時に呼ばれ、変数を開放する。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TMainF::~TMainF() {
	int i;

	for (i = 0; i < TokenList->Count; i++) {
		delete(TToken*)TokenList->Items[i];
	}
	delete TokenList;
	for (i = 0; i < ValDataList->Count; i++) {
		delete(TValData*)ValDataList->Items[i];
	}
	delete ValDataList;
	for (i = 0; i < TuneRegionList->Count; i++) {
		delete(TTuneRegion*)TuneRegionList->Items[i];
	}
	delete TuneRegionList;
	if (TokStrList != NULL) {
		delete TokStrList;
	}
	if (Call_ATExec_ArgList != NULL) {
		delete Call_ATExec_ArgList;
		Call_ATExec_ArgList = NULL;
	}

	if (LogFp != NULL) {
		fclose(LogFp);
		LogFp = NULL;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// コマンドラインの解析と実行を行う。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TMainF::GetCommandLineArg(int argc, char* argv[]) {
	int i, j, cp;
	string s, SrcFileName;
	TSrcCodeType CodeType;
	bool Fortran77F = false;
	bool Fortran90F = false;

	NotSkipByErrF = false; // エラーがあっても実行継続
	for (i = 1; i < argc; i++) {
		s = LowerCase(argv[i]);
		if ((s.compare("-visualization") == 0) && (i < argc - 1)) {
			s = LowerCase(argv[++i]);
			if (s.compare("on") == 0) {
				VisualF = true;
			}
			continue;
		}
		if ((s.compare("-debug") == 0) && (i < argc - 1)) {
			s = LowerCase(argv[++i]);
			if (s.compare("on") == 0) {
				DebugF = true;
			}
			else {
				DebugF = false;
			}
			continue;
		}
		if (s.compare("-nompi") == 0) {
			NoMPIF = true;
			MPIF = false;
			continue;
		}
		if (s.compare("-mpi") == 0) {
			NoMPIF = false;
			MPIF = true;
			continue;
		}
		if (s.compare("-omp") == 0) {
			OMP_OuterF = true;
			continue;
		}
		if (s.compare("-omp_outer") == 0) {
			OMP_OuterF = true;
			continue;
		}
		if (s.compare("-omp_inner") == 0) {
			OMP_InnerF = true;
			continue;
		}
		if (s.compare("-gcforce") == 0) {
			NotSkipByErrF = true; // エラーがあっても実行継続
			continue;
		}
		if ((s.compare("-time") == 0) && (i < argc - 1)) {
			TimeFunc = LowerCase(argv[++i]);
/*
			if (s.compare("-mpi") == 0) {
				s = "MPI_Wtime";
			}
			else if (s.compare("-oat") == 0) {
				s = "OAT_Wtime";
			}
			TimeFunc = s;
*/
			continue;
		}
		if ((s.compare("-stime_and_etime") == 0) && (i < argc - 2)) {
			s = LowerCase(argv[++i]);
			my_timer_start = s;
			s = LowerCase(argv[++i]);
			my_timer_stop = s;
			continue;
		}
		if (s.compare("-eecnt") == 0) { // -eecntlは ON指定なしで有効。
			EECntlF = true;
			continue;
		}
		if ((s.substr(0, 4).compare("-cc=") == 0) && (i < argc - 1)) {
			if (s.compare("-cc=pgi") == 0) {
				cc_option_str = "PGI";
			}
			else if (s.compare("-cc=ompcuda") == 0) {
				cc_option_str = "OMPCUDA";
			}
			else {
				// print("-cc=の後が、PGIまたはOMPCUDAでありません。");
				ErrMessage(-1, "-cc= is not PGI or OMPCUDA.");
				EndF = true;
				break;
			}
			continue;
		}
		if ((s.substr(0, 20).compare("-insert_module_head=") == 0) && (i < argc - 1)) {
			//
			// -insert_module_head=filename 対象を追加する。
			//
			s = s.substr(20,s.length());
			InsertModulseHeadFileNameList.Add(s); // 対象ファイル名を追加する。
			continue;
		}
		if (s.compare("-fixed") == 0) {
			Fortran77F = true;
			continue;
		}
		if (s.compare("-free") == 0) {
			Fortran90F = true;
			continue;
		}
		if (s.find(".") != string::npos) {
			SrcFileName = argv[i];
			// ファイル名の \\ を / に変更する。
			for (j = 0; j < (int)SrcFileName.length(); j++) {
				if (SrcFileName[j] == '\\') {
					SrcFileName[j] = '/';
				}
			}
			SrcFileNameList.Add(SrcFileName); // 対象ソースファイル名リスト。
		}
	}
	//
	// ソースコードの種類を調べる。拡張子にfが含まれていればFortranとする。
	// .f は Fortran  .fxx は　f90として扱う。
	// 複数ファイル指定で異なるタイプが混在する場合はエラーとする。
	//
	for (i = 0; i < SrcFileNameList.Count; i++) {
		SrcFileName = SrcFileNameList.Strings[i];
		CodeType = sctC;
		s = LowerCase(SrcFileName);
		cp = s.rfind('.');
		if ((cp != -1) && (cp + 1 < (int)s.length())) {
			if (s[cp + 1] == 'f') {
				if (IsF90SrcFile(SrcFileName)) {
					CodeType = sctFortran90;
				}
				else {
					CodeType = sctFortran77;
				}
			}
		}
		if (i == 0) {
			SrcCodeType = CodeType;
		}
		else {
			if (SrcCodeType != CodeType) {
				// ErrMessage(-1,"異なる拡張子が混在しています。");
				ErrMessage(-1, "A different file extension is intermingled.");
			}
		}
	}
	// オプション指定がある場合は、その指定を優先。
	if(Fortran77F){
		SrcCodeType = sctFortran77;
	}
	else if(Fortran90F){
		SrcCodeType = sctFortran90;
	}
	if (TimeFunc == "") { // TimeFuncの指定がない場合はデフォルトをセットする。
		if(MPIF){
			TimeFunc = "MPI_Wtime";
		}else{
			TimeFunc = "OAT_Wtime";
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// SrcFileNmae ソースファイル名
//
// 3.概要
// ソースファイル名から、f90のファイルかどうかを返す。
//
// 4.機能説明
//
// 5.戻り値
// f90のファイルの場合にtrue
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
bool TMainF::IsF90SrcFile(string SrcFileName)
{
	string s, s2;
	int cp;

	//
	// ソースコードの拡張子を抜き出して比較する。拡張子が.f90ならば f90形式とする。
	//
	if ((SrcFileName == "")&&(SrcFileNameList.Count > 0)) {
		SrcFileName = SrcFileNameList.Strings[0];
	}
	s = LowerCase(SrcFileName);
	cp = s.rfind('.');
	if (cp != -1) {
		s2 = s.substr(cp, s.length());
		if (s2 == ".f90") {
			return true;
		}
	}
	return false;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// コード生成をを行う。各パスを順次に呼び出す。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TMainF::CodeGen() {
	int i;
	string s;
	TPass4 *Pass4 = NULL;
	string DirName, SrcFileName;

	ErrCount = 0;
	ErrF = false;
	if (SrcFileNameList.Count == 0) {
		print("SrcFile Open error.");
		return;
	}
	print("**** Start Code Generation ****");
	for (i = 0; i < SrcFileNameList.Count; i++) {
		SrcFileNameList.Objects[i] = (void*)(long)TokenList->Count;
		// TokenListの開始位置をセット
		SrcFileName = SrcFileNameList.Strings[i];
		print("SrcFile =\"" + SrcFileName + "\"");
		if (!FileExists(SrcFileName)) {
			printf("SrcFile Open error.\n");
			print("**** End Code Generation **** Error !");
			return;
		}
		print("Pass1 Start");
		TokenList->Add((void*)new TToken("", tid_null)); // 先頭にnullを追加。
		Pass1(SrcFileName, TokenList); // SrcFileをトークンに解析し、TokenListを生成
		TokenList->Add((void*)new TToken("", tid_null)); // 最後
#if 0	// Pass1の後の表示確認用
		DispTokenList();
		print("DispTokenList Count = "+IntToStr(TokenList->Count) + " ----------------------------\n");
#endif
	}
	if (!ErrF || NotSkipByErrF) {
		print("Pass2 Start");
		Pass2(TokenList, ValDataList); // TokenListから構文解析を行い、ValDataリストを生成
#if 0	// Pass2の後の表示確認用
		for (int i = 0; i < TokenList->Count; i++) {
			string s;

			s = IntToStr(i) + " [" + ((TToken*)TokenList->Items[i])
				->Str + "] Id = "
			// + IntToStr(((TToken *)TokenList->Items[i])->TokId)
			+ ((TToken*)TokenList->Items[i])->GetTokIdStr()
				+ " Line=" + IntToStr(((TToken*)TokenList->Items[i])->LineNo)
				+ " ModuleIdx=" + IntToStr(((TToken*)TokenList->Items[i])->ModuleIdx)
				+ " NestLevel=" + IntToStr(((TToken*)TokenList->Items[i])->NestLevel)
				+ " [" + ((TToken*)TokenList->Items[i])->OrgStr + "]";
			if (((TToken*)TokenList->Items[i])->LineEndF) {
				s += " [LineEnd] ";
			}
			if (((TToken*)TokenList->Items[i])->TokId == tid_Val) {
				s += "Val ";
			}
			else if (((TToken*)TokenList->Items[i])->TokId == tid_Func) {
				s += "Func ";
			}
			MainF->print(s);
		}
#endif
	}
#if 1	// 変数リスト表示
	print("------ Val List ---------");
	for (int i = 0; i < ValDataList->Count; i++) {
		TValData *ValData = (TValData*)ValDataList->Items[i];
		print(IntToStr(i) + ":" + ((TValData*)ValDataList->Items[i])->ToString
			() + " DefPos=" + IntToStr(ValData->DefPos));
	}
#endif
	// スクリプト解析
	if (!ErrF || NotSkipByErrF) {
		print("Pass3 Start");
		Pass3(TokenList, ValDataList);
	}

	// チューニングリージョン生成
	if (!ErrF || NotSkipByErrF) {
		print("Pass4 Start");
		Pass4 = new TPass4(TokenList, ValDataList, TuneRegionList);
		Pass4->Exec();
	}
#if 1 // 生成されたチューニングリージョンの表示
	if (!ErrF || NotSkipByErrF) {
		print("------ Tuning Region -- Count = " + IntToStr(TuneRegionList->Count)
			);
		for (int i = 0; i < TuneRegionList->Count; i++) {
			print("Tuning Region : " + IntToStr(i));
			print(((TTuneRegion*)TuneRegionList->Items[i])->GetInfo());
		}
	}
#endif
	// 出力ファイル生成
	if (Call_ATExec_Script == NULL) {
		ErrMessage(-1, "!OAT$ call OAT_ATexec() Script is not found.");
	}
	if (!ErrF || NotSkipByErrF) {
		print("Pass5 Start");
		Pass5 = new TPass5(TokenList, ValDataList, TuneRegionList);
		((TPass5 *)Pass5)->Exec();
	}
	//
	// Visual化 ファイル出力
	//
	if (!ErrF || NotSkipByErrF) {
		if (VisualF) {
			TVisualDM VisualDM;
			VisualDM.Exec();
		}
	}
	if (Pass4) {
		delete Pass4;
	}
	if (Pass5) {
		delete (TPass5 *)Pass5;
	}
	if (CloseReqF) {
		print("**** End Code Generation **** Aboart !");
		ErrF = true;
	}
	else if (ErrF) {
		print("**** End Code Generation **** Error ! ErrorCount = " + IntToStr
			(ErrCount));
	}
	else {
		print("**** End Code Generation **** OK !");
	}
	EndF = true;
	if (LogFp != NULL) {
		fclose(LogFp);
		LogFp = NULL;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// s   出力文字列
//
// 3.概要
// デバッグ用のトークン出力
//
// 4.機能説明
// デバッグ用にトークンを出力する。
//
// 5.戻り値
// なし
//
// 6.備考
//
//
/* ---------------------------------------------------------------------------- */
void TMainF::DispTokenList() {
	string s, ks;

	for (int i = 0; i < TokenList->Count; i++) {

		s = IntToStr(i) + " [" + ((TToken*)TokenList->Items[i])->Str + "]("
		// + IntToStr(((TToken *)TokenList->Items[i])->TokId)
		+ ((TToken*)TokenList->Items[i])->GetTokIdStr()
		// + IntToStr(((TToken *)TokenList->Items[i])->LineNo)
		+ ")[" + ((TToken*)TokenList->Items[i])->OrgStr + "]";
		if (((TToken*)TokenList->Items[i])->LineEndF) {
			s += " [LineEnd] ";
		}
		if (((TToken*)TokenList->Items[i])->TokId == tid_Val) {
			s += "Val";
		}
		else if (((TToken*)TokenList->Items[i])->TokId == tid_Func) {
			s += "Func";
		}
		else if (((TToken*)TokenList->Items[i])->TokId == tid_Opr) {
			s += "Opr";
		}
		else if (((TToken*)TokenList->Items[i])->TokId == tid_DataType) {
			s += "Type";
		}
		print(s);
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// s   出力文字列
//
// 3.概要
// デバッグ用出力関数
//
// 4.機能説明
// デバッグ用出力関数。文字列Ｓを出力する。
//
// 5.戻り値
// なし
//
// 6.備考
// デバッグ時以外は、使用しないこと。
//
/* ---------------------------------------------------------------------------- */
void DP(string s) {
	MainF->print(s);
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// string s 表示する文字列
//
// 3.概要
// メッセージの表示とログファイルへの出力を行う。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TMainF::print(string s) {
	printf("%s\n", s.c_str());
	if (LogFp != NULL) {
		fprintf(LogFp, "%s\n", s.c_str());
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// string s 表示する文字列
//
// 3.概要
// エラーメッセージの表示とログファイルへの出力を行う。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TMainF::Err(string s) {
	ErrF = true;
	printf("*** ERROR *** %s\n", s.c_str());
	if (LogFp != NULL) {
		fprintf(LogFp, "*** ERROR *** %s\n", s.c_str());
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// string s 表示する文字列
//
// 3.概要
// エラーメッセージの表示とログファイルへの出力を行う。
//
// 4.機能説明
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TMainF::ErrMessage(int Pos, string s) {
	int i;
	TToken *Token;
	int LineNo;
	int sPos;
	string FileName = "";

	ErrF = true;
	ErrCount++;
	printf("*** ERROR *** %s\n", s.c_str());
	if (LogFp != NULL) {
		fprintf(LogFp, "*** ERROR *** %s\n", s.c_str());
	}
	//
	// Posの示すSrcFileNameを所得する。
	//
	for (i = 0; i < SrcFileNameList.Count; i++) {
		sPos = (long)SrcFileNameList.Objects[i];
		if (Pos >= sPos) {
			FileName = SrcFileNameList.Strings[i];
		}
	}
	if ((Pos >= 0) && (Pos < TokenList->Count)) {
		Token = (TToken*)TokenList->Items[Pos];
		if (Token == NULL) {
			return;
		}
		LineNo = Token->LineNo;
		s = "";
		for (i = Pos; i > 0; i--) {
			Token = (TToken*)TokenList->Items[i];
			if (LineNo != Token->LineNo) {
				i++;
				break;
			}
		}
		for (; i < TokenList->Count; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token->TokId == tid_LineEnd) { // １行単位で解析を行う。
				break;
			}
			s += Token->OrgStr;
		}
		s = FileName + " LineNo = " + IntToStr(LineNo) + " " + s;
		s = Trim(s);
		printf("  %s\n", s.c_str());
		if (LogFp != NULL) {
			fprintf(LogFp, "    %s\n", s.c_str());
		}
	}
}

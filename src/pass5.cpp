/* =====================================================================*
 *                                                                     *
 *   Software Name : ppOpen-AT                                         *
 *         Version : 0.1                                               *
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
/* ---------------------------------------------------------------------------- */
//
// 概要
// パス５
//
// 作成者
//
/* ---------------------------------------------------------------------------- */

#include "pass5.h"
#include "TuneRegion.h"

#ifdef _WIN32
#include <dir.h>
#endif
#include <sys/stat.h>

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// aTokenList  トークンリスト
// aValDataList    変数リスト
// aTuneRegionList チューニングリージョンリスト
//
// 3.概要
// パス５クラス生成
// 変数の初期化と出力ファイルのオープンを行う。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TPass5::TPass5(TList *aTokenList, TList *aValDataList, TList *aTuneRegionList) {
	string DirName;
	string fname;
	string s;
	string FileExtStr;
	struct stat sb;
	int i;
	FILE *fpOut;

	if (MainF->SrcCodeType == MainF->sctFortran77) {
		Comment = 'c';
	}
	else {
		Comment = '!';
	}
	TokenList = aTokenList;
	ValDataList = aValDataList;
	TuneRegionList = aTuneRegionList;
	Call_SubroutineNameInRegionList = new TStringList;

	fname = MainF->SrcFileNameList.Strings[0];
	DirName = fname.substr(0, fname.rfind("/") + 1);
	if (DirName == "") {
		DirName = "./";
	}
	s = DirName + "OAT";
	if (stat(s.c_str(), &sb) == -1) {
#ifdef _WIN32
		if (mkdir(s.c_str()) != 0) {
			MainF->ErrMessage(-1, "フォルダが作成出来ません。 ");
		}
#else
		mkdir(s.c_str(),
			S_IRUSR | S_IWUSR | S_IXUSR | /* rwx */ S_IRGRP | S_IWGRP |
			S_IXGRP | /* rwx */ S_IROTH | S_IXOTH | S_IXOTH);
#endif
	}
	for (i = 0; i < MainF->SrcFileNameList.Count; i++) {
		fname = MainF->SrcFileNameList.Strings[i];
		SrcFname = fname.substr(fname.rfind("/") + 1);
		fname = DirName + "OAT/OAT_" + SrcFname;
		fpOut = fopen(fname.c_str(), "wt"); // 出力ファイル
		if (fpOut == NULL) {
			MainF->ErrMessage(-1, fname + " ファイルが開けません");
		}
		fpOutList.Add(fpOut);
	}
	if (MainF->SrcCodeType == MainF->sctFortran77) {
		FileExtStr = ".f";
	}
	else if (MainF->SrcCodeType == MainF->sctFortran90) {
		FileExtStr = ".f90";
	}
	else {
		FileExtStr = ".c";
	}
	fname = DirName + "OAT/OAT_ControlRoutines" + FileExtStr;
	fpOutControl = fopen(fname.c_str(), "wt");
	if (fpOutControl == NULL) {
		MainF->ErrMessage(-1, fname + " ファイルが開けません");
	}
	fname = DirName + "OAT/OAT_InstallRoutines" + FileExtStr;
	fpOutInstall = fopen(fname.c_str(), "wt");
	if (fpOutInstall == NULL) {
		MainF->ErrMessage(-1, fname + " ファイルが開けません");
	}
	fname = DirName + "OAT/OAT_StaticRoutines" + FileExtStr;
	fpOutStatic = fopen(fname.c_str(), "wt");
	if (fpOutStatic == NULL) {
		MainF->ErrMessage(-1, fname + " ファイルが開けません");
	}
	fname = DirName + "OAT/OAT_DynamicRoutines" + FileExtStr;
	fpOutDynamic = fopen(fname.c_str(), "wt");
	if (fpOutDynamic == NULL) {
		MainF->ErrMessage(-1, fname + " ファイルが開けません");
	}
	if (MainF->SrcCodeType == MainF->sctC) {
		fname = DirName + "OAT/OAT_Routines.h";
		fpOutHeader = fopen(fname.c_str(), "wt");
		if (fpOutHeader == NULL) {
			MainF->ErrMessage(-1, fname + " ファイルが開けません");
		}
		fprintf(fpOutHeader,
			"//============================================================\n");
		fprintf(fpOutHeader, "//=== OAT_Routines Header\n");
		fprintf(fpOutHeader,
			"//============================================================\n");
	}
	else {
		fpOutHeader = NULL;
	}
	//
	// Fortranの場合は、OAT_base.hの後に変数を追加した OAT.h を出力する。
	//
	/** **************************************************************************************** */
	//
	// Kogakuin Irie
	// OAT_base.hではなく，OAT.hに変数を追加して，追加後のOAT.hを出力する．
	// また，この処理をFortran77でも行うようにする
	// 既存のコードはコメントアウト
	//
	// if(MainF->SrcCodeType == MainF->sctFortran90){
	// fname = DirName + "OAT_base.h";
	// fpInOATHeader = fopen(fname.c_str(),"rt");
	// if(fpInOATHeader == NULL){
	// MainF->ErrMessage(-1,fname + " ファイルが開けません");
	// }
	// fname = DirName + "OAT/OAT.h";
	// fpOutOATHeader = fopen(fname.c_str(),"wt");
	// if(fpOutOATHeader == NULL){
	// MainF->ErrMessage(-1,fname + " ファイルが開けません");
	// }
	// }
	if (MainF->SrcCodeType == MainF->sctFortran90 || MainF->SrcCodeType ==
		MainF->sctFortran77) {
		fname = DirName + "OAT.h";
		fpInOATHeader = fopen(fname.c_str(), "rt");
		if (fpInOATHeader == NULL) {
			MainF->ErrMessage(-1, fname + " ファイルが開けません");
		}
		fname = DirName + "OAT/OAT.h";
		fpOutOATHeader = fopen(fname.c_str(), "wt");
		if (fpOutOATHeader == NULL) {
			MainF->ErrMessage(-1, fname + " ファイルが開けません");
		}
	}
	else { // Ｃ言語の場合は、そのまま複写のみを行う。
		int ch;

		fname = DirName + "OAT.h";
		fpInOATHeader = fopen(fname.c_str(), "rt");
		if (fpInOATHeader == NULL) {
			MainF->ErrMessage(-1, fname + " ファイルが開けません");
		}
		fname = DirName + "OAT/OAT.h";
		fpOutOATHeader = fopen(fname.c_str(), "wt");
		if (fpOutOATHeader == NULL) {
			MainF->ErrMessage(-1, fname + " ファイルが開けません");
		}
		while (true) {
			ch = fgetc(fpInOATHeader);
			if (ch == EOF) {
				break;
			}
			fputc(ch, fpOutOATHeader);
		}
		if (fpInOATHeader != NULL) {
			fclose(fpInOATHeader);
		}
		if (fpOutOATHeader != NULL) {
			fclose(fpOutOATHeader);
		}
		fpInOATHeader = NULL;
		fpOutOATHeader = NULL;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// パス５クラスの破棄
// ファイルのクローズ処理を行う
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
TPass5::~TPass5() {
	int i;
	FILE *fpOut;

	for (i = 0; i < fpOutList.Count; i++) {
		fpOut = (FILE*)fpOutList.Items[i];
		if (fpOut != NULL) {
			fclose(fpOut);
		}
	}
	fpOutList.Clear();
	fclose(fpOutControl);
	fclose(fpOutInstall);
	fclose(fpOutStatic);
	fclose(fpOutDynamic);
	if (fpOutHeader != NULL) {
		fclose(fpOutHeader);
	}
	if (fpInOATHeader != NULL) {
		fclose(fpInOATHeader);
	}
	if (fpOutOATHeader != NULL) {
		fclose(fpOutOATHeader);
	}
	if (Call_SubroutineNameInRegionList != NULL) {
		delete Call_SubroutineNameInRegionList;
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// パス５の実行
// TureResionListを参照して、複数のチューニングリージョンに対応したソースコー
// ドを出力する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::Exec() {
	int i, j;
	TTuneRegion *TuneRegion;
	string s, s1, s2;
	bool InstallRegionF = false;
	bool StaticRegionF = false;
	bool DynamicRegionF = false;

	FittingF = false; // Fittingありの場合の変数定義追加用
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			if (TuneRegion->FittingType != 0) {
				FittingF = true; // Fittingありの場合の変数定義追加用
			}
		}
	}
	//
	// TRの中にcallが含まれていれば、そのcall先にTRが含まれていないかチェックして
	// 含まれていれば、サブルーチンの複写と呼び先の _OATへの変更を行う。
	// 検索結果は、CallInRegionDataとしてCallInRegionDataListに追加される。
	// 必要に応じて参照され、コードに複写と_OATへの変更が行われる。
	//
	Call_SubroutineNameInRegionList->Clear();
	if (MainF->SrcCodeType != MainF->sctC) {
		Make_Call_SubroutineNameInRegionList();
	}
	//
	// TRの名前を降順に並べ替える。
	// 例えば、ABC2 と ABC は、あいまいとなる。(index関数で検索しているため）
	// 降順に並べかれてから、（ABC2 が先で ABC）一致後はreturnとする。
	// (index関数は先に見つかった文字列が有効なため）
	//
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		s1 = TuneRegion->Name; // TRの名前
		for (j = i + 1; j < TuneRegionList->Count; j++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[j];
			s2 = TuneRegion->Name;
			if (s1.compare(s2) < 0) {
				TuneRegionList->Items[j] = TuneRegionList->Items[i];
				TuneRegionList->Items[i] = TuneRegion;
			}
		}
	}
	// 文字列の長さを決定する。（全てのTRの名前＋,の長さとする）
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		s += TuneRegion->Name + ",";
	}
	MainF->CharMaxLen = s.length() - 1;

	//
	// リージョン種類を調べる。
	//
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			InstallRegionF = true;
		}
		if (TuneRegion->TuneGroup == tgStatic) {
			StaticRegionF = true;
		}
		if (TuneRegion->TuneGroup == tgDynamic) {
			DynamicRegionF = true;
		}
	}
	//
	// 各モジュールについての宣言を追加する。
	//
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fpOutInstall, "      module %s\n", "ppohAT_InstallRoutines");
		fprintf(fpOutInstall, "\n");
		if (InstallRegionF) {
			OutputModuleStart(fpOutInstall);
		}
		fprintf(fpOutStatic, "      module %s\n", "ppohAT_StaticRoutines");
		fprintf(fpOutStatic, "\n");
		if (StaticRegionF) {
			OutputModuleStart(fpOutStatic);
		}
		fprintf(fpOutDynamic, "      module %s\n", "ppohAT_DynamicRoutines");
		fprintf(fpOutDynamic, "\n");
		if (DynamicRegionF) {
			OutputModuleStart(fpOutDynamic);
		}
	}
	for (i = 0; i < TuneRegionList->Count; i++) {
		MainF->print("TuneRegion = " + IntToStr(i + 1) + "/" + IntToStr
			(TuneRegionList->Count));
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			if (MainF->SrcCodeType != MainF->sctC) {
				TuneRegion->OutputExecCode_Fortran(fpOutInstall);
			}
			else {
				TuneRegion->OutputExecCode_C(fpOutInstall, fpOutHeader);
			}
		}
		else if (TuneRegion->TuneGroup == tgStatic) {
			if (MainF->SrcCodeType != MainF->sctC) {
				TuneRegion->OutputExecCode_Fortran(fpOutStatic);
			}
			else {
				TuneRegion->OutputExecCode_C(fpOutStatic, fpOutHeader);
			}
		}
		else if (TuneRegion->TuneGroup == tgDynamic) {
			if (MainF->SrcCodeType != MainF->sctC) {
				TuneRegion->OutputExecCode_Fortran(fpOutDynamic);
			}
			else {
				TuneRegion->OutputExecCode_C(fpOutDynamic, fpOutHeader);
			}
		}
		if (MainF->CloseReqF) {
			return;
		}
	}
	//
	// リージョン内のCall先がリージョンを持っている場合にサブルーチンを別名に複写する。
	//
	if (MainF->SrcCodeType != MainF->sctC) {
		if (Call_SubroutineNameInRegionList->Count != 0) {
			Output_Call_SubroutineNameInRegionList();
		}
	}
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fpOutInstall, "      end module ppohAT_InstallRoutines\n");
		fprintf(fpOutDynamic, "      end module ppohAT_DynamicRoutines\n");
		fprintf(fpOutStatic, "      end module ppohAT_StaticRoutines\n");
	}
	// 元ソースを変更したファイルを生成。
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		MakeSrcCode_Fortran(); // OAT/OAT_(元のソースファイル名).f 作成
	}
	else if (MainF->SrcCodeType == MainF->sctFortran77) {
		MakeSrcCode_Fortran77(); // OAT/OAT_(元のソースファイル名).f 作成
	}
	else {
		MakeSrcCode_C(); // OAT/OAT_(元のソースファイル名).c 作成
	}
	// コントロールコードを生成
	if (MainF->SrcCodeType != MainF->sctC) {
		MakeControlCode_Fortran(); // OAT/OAT_ControlRoutines.f 作成
	}
	else {
		MakeControlCode_C(); // OAT/OAT_ControlRoutines.c 作成
	}
	//
	// OAT/OAT.h ヘッダファイルを OAT.h ファイルを元にして作成する。
	//
	if ((fpInOATHeader != NULL) && (fpInOATHeader != NULL)) {
		// OAT_base.h の内容を複写する。
		int ch;
		while (true) {
			ch = fgetc(fpInOATHeader);
			if (ch == EOF) {
				break;
			}
			fputc(ch, fpOutOATHeader);
		}
		fprintf(fpOutOATHeader, "!       === AT region variables\n");
		if (MainF->OMP_InnerF) {
			fprintf(fpOutOATHeader, "      integer  oat_mythread_num\n");
			fprintf(fpOutOATHeader,
				"       common  /OAT_OMPval/oat_mythread_num\n");
			fprintf(fpOutOATHeader, "!$omp threadprivate(/OAT_OMPval/)\n");
			fprintf(fpOutOATHeader, "\n");
		}
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			fprintf(fpOutOATHeader, "      integer iusw1_%s\n",
				TuneRegion->Name.c_str());
		}
		fprintf(fpOutOATHeader, "\n");
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			if (MainF->SrcCodeType == MainF->sctFortran90) {
				if (i == 0) {
					fprintf(fpOutOATHeader,
						"      common /OAT_ATswitches/iusw1_%s",
						TuneRegion->Name.c_str());
				}
				else {
					fprintf(fpOutOATHeader,
						"     &                       iusw1_%s",
						TuneRegion->Name.c_str());
				}
				if (i < TuneRegionList->Count - 1) {
					fprintf(fpOutOATHeader, ", &\n");
				}
				else {
					fprintf(fpOutOATHeader, "\n");
				}
			}
			else {
				if (i == 0) {
					fprintf(fpOutOATHeader,
						"      common /OAT_ATswitches/iusw1_%s",
						TuneRegion->Name.c_str());
				}
				else {
					fprintf(fpOutOATHeader,
						"     &                       iusw1_%s",
						TuneRegion->Name.c_str());
				}
				if (i < TuneRegionList->Count - 1) {
					fprintf(fpOutOATHeader, ",\n");
				}
				else {
					fprintf(fpOutOATHeader, "\n");
				}
			}
		}
		fprintf(fpOutOATHeader, "\n");
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			fprintf(fpOutOATHeader, "      integer iusw1_%s_flag\n",
				TuneRegion->Name.c_str());
		}
		fprintf(fpOutOATHeader, "\n");
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			if (MainF->SrcCodeType == MainF->sctFortran90) {
				if (i == 0) {
					fprintf(fpOutOATHeader,
						"      common /OAT_ATswitchFlags/iusw1_%s_flag",
						TuneRegion->Name.c_str());
				}
				else {
					fprintf(fpOutOATHeader,
						"     &                       iusw1_%s_flag",
						TuneRegion->Name.c_str());
				}
				if (i < TuneRegionList->Count - 1) {
					fprintf(fpOutOATHeader, ", &\n");
				}
				else {
					fprintf(fpOutOATHeader, "\n");
				}
			}
			else {
				if (i == 0) {
					fprintf(fpOutOATHeader,
						"      common /OAT_ATswitchFlags/iusw1_%s_flag",
						TuneRegion->Name.c_str());
				}
				else {
					fprintf(fpOutOATHeader,
						"     &                       iusw1_%s_flag",
						TuneRegion->Name.c_str());
				}
				if (i < TuneRegionList->Count - 1) {
					fprintf(fpOutOATHeader, ",\n");
				}
				else {
					fprintf(fpOutOATHeader, "\n");
				}
			}
		}
		fprintf(fpOutOATHeader, "\n");
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// OAT/OAT_ControlRoutines.c を作成する。
//
// 4.機能説明
//
// 5.戻り値

//

// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::MakeControlCode_C() {
	FILE *fp = fpOutControl;
	string s, s1, s2, s3;
	int i;
	TTuneRegion *TuneRegion;
	int TempLineNo;
	string ArgStr = "";

	fprintf(fp, "#include <time.h>\n");
	// Add
	fprintf(fp, "#include <stdio.h>\n");
	fprintf(fp, "#include <stdlib.h>\n");
	fprintf(fp, "\n");
	//
	fprintf(fp, "double OAT_Wtime()\n");
	fprintf(fp, "{\n");
	fprintf(fp, "\treturn (double)clock()/CLOCKS_PER_SEC;\n");
	fprintf(fp, "}\n");

	//
	// ==== OAT_ATset(Type,Routines)
	//
	s = s1 = s2 = s3 = "";
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (s != "") {
			s += ",";
		}
		s += TuneRegion->Name;
		if (TuneRegion->TuneGroup == tgInstall) {
			if (s1 != "") {
				s1 += ",";
			}
			s1 += TuneRegion->Name;
		}
		else if (TuneRegion->TuneGroup == tgStatic) {
			if (s2 != "") {
				s2 += ",";
			}
			s2 += TuneRegion->Name;
		}
		else if (TuneRegion->TuneGroup == tgDynamic) {
			if (s3 != "") {
				s3 += ",";
			}
			s3 += TuneRegion->Name;
		}
	}
	fprintf(fpOutHeader, "int OAT_ATset(int OAT_TYPE, char *OAT_Routines);\n");
	fprintf(fp,
		"//============================================================\n");
	fprintf(fp, "//=== OAT_ATset\n");
	fprintf(fp,
		"//============================================================\n");
	fprintf(fp, "int OAT_ATset(int OAT_TYPE, char *OAT_Routines)\n");
	fprintf(fp, "{\n");
	fprintf(fp, "//==== All routines\n");
	fprintf(fp, "\tif(OAT_TYPE == 0){\n");
	fprintf(fp, "\t\tstrcpy(OAT_Routines,\"%s\");\n", s.c_str());
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		fprintf(fp, "        OAT_iusw1_%s_flag = 0;\n",TuneRegion->Name.c_str());
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "//==== Install Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 1){\n");
	s = s1;
	fprintf(fp, "\t\tstrcpy(OAT_Routines,\"%s\");\n", s.c_str());
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			fprintf(fp, "        OAT_iusw1_%s_flag = 0;\n",TuneRegion->Name.c_str());
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "//==== Before Execution-invocation Optimization Routines\n");
	fprintf(fp, "\tif(OAT_TYPE == 2){\n");
	s = s2;
	fprintf(fp, "\t\tstrcpy(OAT_Routines,\"%s\");\n", s.c_str());
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgStatic) {
			fprintf(fp, "        OAT_iusw1_%s_flag = 0;\n",TuneRegion->Name.c_str());
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "//==== Run-time Optimization Routines\n");
	fprintf(fp, "\tif(OAT_TYPE == 3){\n");
	fprintf(fp, "\t\tOAT_DYNAMICTUNE = 0;\n");
	s = s3;
	fprintf(fp, "\t\tstrcpy(OAT_Routines,\"%s\");\n", s.c_str());
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			fprintf(fp, "        OAT_iusw1_%s_flag = 0;\n",TuneRegion->Name.c_str());
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "\treturn 0;\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");

	//
	// OAT_SetParam(Type,Routine,n,*isw)
	// isw パラメータを所得する。 複数のTR(名前で識別)に対応する。
	//
	// fprintf(fpOutHeader,"int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n,int *isw);\n");
	//
	// Dynamic以外はSetParamの引数は最小にする。2016/03/12
	// call OAT_xxx_OAT()での呼び出し時に、その先からSetParama_OATを呼ぶ必要が出てくるため。
	//
	bool DynamicRegionF = false;
	for (int i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			DynamicRegionF = true;
			break;
		}
	}
	if (DynamicRegionF) {
		if (MainF->Call_ATExec_Script == NULL) {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(true, "", "");
			delete Script;
		}
		else {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "", "");
		}
	}
	else {
		ArgStr = "";
	}
	// fprintf(fpOutHeader,
	// "int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int *isw%s);\n",
	// ArgStr.c_str());
	fprintf(fpOutHeader,
		"int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n_bpset,int *isw%s);\n"
		, ArgStr.c_str());
	fprintf(fp,
		"//============================================================\n");
	fprintf(fp, "//=== OAT_SetParm\n");
	fprintf(fp,
		"//============================================================\n");
	fprintf(fp,
		// "int OAT_SetParm(int OAT_TYPE,char *OAT_Routines , int *isw%s)\n",
		"int OAT_SetParm(int OAT_TYPE,char *OAT_Routines,int n_bpset,int *isw%s)\n"
		, ArgStr.c_str());

	fprintf(fp, "{\n");

	if (FittingF) { // Fitting用配列
		fprintf(fp, "//!!!!!! fitting用配列\n");
		fprintf(fp, "// === for target coefficients\n");
		// fprintf(fp,"      real*8  a_lsm(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)\n");
		// fprintf(fp,"      real*8  dtemp\n");
		fprintf(fp, "\tdouble a_lsm[OATLSM_MAX_M][OATLSM_MAX_NPARM-1];\n");
		fprintf(fp, "\tdouble dtemp;\n");
		fprintf(fp, "\n");
		fprintf(fp, "\tint nparm,nsamp;\n");
		fprintf(fp, "\tint m_lsm,iii;\n");
	}
	// fprintf(fp,"\n");
	fprintf(fp, "\tint ibsw;\n");
	// fprintf(fp,"\tchar cbuf[100];\n");
	fprintf(fp, "\tchar cbuf[500];\n");
	fprintf(fp, "\tchar digit[20];\n");
	fprintf(fp, "\tFILE *fp;\n");
	// fprintf(fp,"\tint i,j,inum;\n");
	fprintf(fp, "\tint oat_i,oat_j,oat_inum;\n");
	fprintf(fp, "\n");

	TempLineNo = 100;
	fprintf(fp, "// ==== Install Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 1){\n");

	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			TuneRegion->OutputSetParamCode_C(fp, TempLineNo);
			TempLineNo += 10;
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "//=== end of OAT_Install\n");
	fprintf(fp, "//-----------------------------------------------\n");

	fprintf(fp, "\n");
	fprintf(fp, "//==== Before Execution-invocation Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 2){\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgStatic) {
			TuneRegion->OutputSetParamCode_C(fp, TempLineNo);
			TempLineNo += 10;
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "//=== end of OAT_Static\n");
	fprintf(fp, "//-----------------------------------------------\n");

	fprintf(fp, "\n");
	fprintf(fp, "//==== Run-time Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 3){\n");
	fprintf(fp, "\t\tif(OAT_DYNAMICTUNE){\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			// Dynamicは、SetParamで、ファイルから読込みしない。
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "\t\t\tOAT_ATexecDynamic%s(OAT_Routines,isw%s);\n",
					TuneRegion->Name.c_str(), ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
				(false, "", "");
				fprintf(fp, "\t\t\tOAT_ATexecDynamic%s(OAT_Routines,isw%s);\n",
					TuneRegion->Name.c_str(), ArgStr.c_str());
			}
		}
	}
	fprintf(fp, "\t\t}else{\n");
	fprintf(fp, "\t\t\t*isw = 1;\n");
	fprintf(fp, "\t\t}\n");
	fprintf(fp, "\t}\n");
	fprintf(fp, "//=== end of OAT_Dynamic\n");
	fprintf(fp, "// -----------------------------------------------\n");
	fprintf(fp, "\n");
	fprintf(fp, "\treturn 0;\n");
	fprintf(fp, "}\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");

	//
	// OAT_ATexec(Type,Routines)
	//
	//
	fprintf(fp,
		"//============================================================\n");
	fprintf(fp, "//=== OAT_ATexec\n");
	fprintf(fp,
		"//============================================================\n");
	if (MainF->Call_ATExec_Script == NULL) {
		TScript *Script = new TScript(MainF->TokenList, 0, NULL, ValDataList);
		string ArgStr = Script->GetATExecArgStr(true, "", "");
		fprintf(fp, "int OAT_ATexec(int OAT_TYPE,char *OAT_Routines%s)\n",
			ArgStr.c_str());
		fprintf(fpOutHeader,
			"int OAT_ATexec(int OAT_TYPE,char *OAT_Routines%s);\n",
			ArgStr.c_str());
		delete Script;
	}
	else {
		string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "",
			"");
		fprintf(fp, "int OAT_ATexec(int OAT_TYPE,char *OAT_Routines%s)\n",
			ArgStr.c_str());
		fprintf(fpOutHeader,
			"int OAT_ATexec(int OAT_TYPE,char *OAT_Routines%s);\n",
			ArgStr.c_str());
	}
	// fprintf(fp,"OAT_ATexec(int OAT_TYPE,char *OAT_Routines)\n");
	fprintf(fp, "{\n");

	fprintf(fp, "\tif(getenv(\"OAT_EXEC\") != 0){\n");
	fprintf(fp, "\t\tif(atoi(getenv(\"OAT_EXEC\")) != 1){\n");
	fprintf(fp, "\t\t\treturn 0;\n");
	fprintf(fp, "\t\t}\n");
	fprintf(fp, "\t}else if(getenv(\"OAT_ATEXEC\") != 0){\n");
	fprintf(fp, "\t\tif(atoi(getenv(\"OAT_ATEXEC\")) != 1){\n");
	fprintf(fp, "\t\t\treturn 0;\n");
	fprintf(fp, "\t\t}\n");
	fprintf(fp, "\t}\n");
	fprintf(fp, "//==== Install Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 1){\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			fprintf(fp, "\t\tif(strstr(OAT_Routines,\"%s\") != 0){\n",
				TuneRegion->Name.c_str());
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "\t\t\tOAT_ATexecInstall%s(OAT_Routines%s);\n",
					TuneRegion->Name.c_str(), ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
				(false, "", "");
				fprintf(fp, "\t\t\tOAT_ATexecInstall%s(OAT_Routines%s);\n",
					TuneRegion->Name.c_str(), ArgStr.c_str());
			}
			// fprintf(fp,"\t\t\tOAT_ATexecInstall%s(OAT_Routines);\n",TuneRegion->Name.c_str());
			fprintf(fp, "\t\t}\n");
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "\n");
	fprintf(fp, "//==== Before Execution-invocation Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 2){\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgStatic) {
			fprintf(fp, "\t\tif(strstr(OAT_Routines,\"%s\") != 0){\n",
				TuneRegion->Name.c_str());
			if (MainF->Call_ATExec_Script == NULL) {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				string ArgStr = Script->GetATExecArgStr(false, "", "");
				fprintf(fp, "\t\t\tOAT_ATexecStatic%s(OAT_Routines%s);\n",
					TuneRegion->Name.c_str(), ArgStr.c_str());
				delete Script;
			}
			else {
				string ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr
				(false, "", "");
				fprintf(fp, "\t\t\tOAT_ATexecStatic%s(OAT_Routines%s);\n",
					TuneRegion->Name.c_str(), ArgStr.c_str());
			}
			fprintf(fp, "\t\t}\n");
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "\n");
	fprintf(fp, "//==== Run-time Optimization\n");
	fprintf(fp, "\tif(OAT_TYPE == 3){\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			fprintf(fp, "\t\tOAT_DYNAMICTUNE = 1;\n");
		}
	}
	fprintf(fp, "\t}\n");
	fprintf(fp, "\n");
	fprintf(fp, "\treturn 0;\n");
	fprintf(fp, "}\n");
	fprintf(fp,
		"//============================================================\n");
	fprintf(fp, "\n");

	//
	// OAT_ATexecGGGFFF サブルーチンのコードの生成 (時間計測->ファイル)
	//
	// for(i = TuneRegionList->Count-1 ; i >= 0 ; i--){
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		TuneRegion->OutputAutoExecCode_C(fp, fpOutHeader);
	}
	if (FittingF) { // Fitting用サブルーチンを追加
		FILE *fpSrc;
		char cBuff[1024];
		string fname;
		string DirName;

#if 0
		fname = MainF->SrcFileNameList.Strings[0];
		DirName = fname.substr(0, fname.rfind("/"));
		fname = DirName + "/OAT_DataControlSubrutions.c";
#else
		fname = "./OAT_DataControlSubrutions.c";
#endif
		fpSrc = fopen(fname.c_str(), "rb"); // 出力ファイル
		if (fpSrc == NULL) {
			MainF->ErrMessage(-1, "File " + fname + " open error");
		}
		else {
			while (fgets(cBuff, sizeof(cBuff), fpSrc) != NULL) {
				fprintf(fp, "%s", cBuff);
			}
			fclose(fpSrc);
		}
	}
	//
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// OAT/OAT_ControlRoutines.f を作成する。
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::MakeControlCode_Fortran(FILE *fpAdd) {
	FILE *fp;
	string s, s1, s2, s3;
	int i;
	TTuneRegion *TuneRegion;
	int TempLineNo;
	int len;
	string ArgStr;

	if (fpAdd == NULL) {
		fp = fpOutControl;
	}
	else {
		fp = fpAdd;
	}
	if (fpAdd == NULL) {
		//
		// ==== OAT_ATset(Type,Routines)
		//
		s = s1 = s2 = s3 = "";
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			if (s != "") {
				s += ",";
			}
			s += TuneRegion->Name;
			if (TuneRegion->TuneGroup == tgInstall) {
				if (s1 != "") {
					s1 += ",";
				}
				s1 += TuneRegion->Name;
			}
			else if (TuneRegion->TuneGroup == tgStatic) {
				if (s2 != "") {
					s2 += ",";
				}
				s2 += TuneRegion->Name;
			}
			else if (TuneRegion->TuneGroup == tgDynamic) {
				if (s3 != "") {
					s3 += ",";
				}
				s3 += TuneRegion->Name;
			}
		}
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp, "      module ppohAT_ControlRoutines\n");
			fprintf(fp, "\n");
			fprintf(fp, "      use ppohAT_InstallRoutines\n");
			fprintf(fp, "      use ppohAT_StaticRoutines\n");
			fprintf(fp, "      use ppohAT_DynamicRoutines\n");
			fprintf(fp, "\n");
			fprintf(fp, "      implicit none\n");
			fprintf(fp, "      public\n");
			fprintf(fp, "\n");
			fprintf(fp, "      contains\n");
			fprintf(fp, "\n");
			fprintf(fp, "\n");
		}

		fprintf(fp, "%c     === OAT_ATset\n", Comment);
		fprintf(fp,
			"%c     ============================================================\n"
			, Comment);
		fprintf(fp, "      subroutine OAT_ATset(OAT_TYPE, OAT_Routines)\n");
		fprintf(fp, "      integer   OAT_TYPE\n");
		fprintf(fp, "      character*%d OAT_Routines\n", MainF->CharMaxLen);
		fprintf(fp, "\n");
		fprintf(fp, "      include 'OAT.h'\n");
		fprintf(fp, "\n");
		if (MainF->OMP_InnerF) {
			fprintf(fp, "       if (oat_mythread_num .eq. 0) then\n");
		}
		fprintf(fp, "\n");
		fprintf(fp, "%c     ==== All routines\n", Comment);
		fprintf(fp, "      if (OAT_TYPE .eq. 0) then\n");
		// 長い文字列は、分解して設定する。
		if (s == "") {
			fprintf(fp, "        OAT_Routines(1:%d) = '%s'\n", (int)s.length(),
				s.c_str());
		}
		else {
			for (i = 1; i <= (int)s.length(); i += 35) {
				len = s.length() - i + 1;
				if (len > 35) {
					len = 35;
				}
				fprintf(fp, "        OAT_Routines(%d:%d) = '%s'\n", i,
					i + len - 1, s.substr(i - 1, len).c_str());
			}
		}
		// Fortran77でも有効へ　ATEXECと違いOAT.hと無関係なので問題なし。
		// if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (true) {
			for (int kk = 0; kk < TuneRegionList->Count; kk++) {
				TuneRegion = (TTuneRegion*)TuneRegionList->Items[kk];
				fprintf(fp, "        iusw1_%s_flag = 0\n",
					TuneRegion->Name.c_str());
			}
		}
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c     ==== Install Optimization\n", Comment);
		fprintf(fp, "      if (OAT_TYPE .eq. 1) then\n");
		// 長い文字列は、分解して設定する。
		s = s1;
		if (s == "") {
			fprintf(fp, "         OAT_Routines(1:%d) = '%s'\n",
				(int)s1.length(), s1.c_str());
		}
		else {
			for (i = 1; i <= (int)s.length(); i += 35) {
				len = s.length() - i + 1;
				if (len > 35) {
					len = 35;
				}
				fprintf(fp, "         OAT_Routines(%d:%d) = '%s'\n", i,
					i + len - 1, s.substr(i - 1, len).c_str());
			}
		}
		// Fortran77でも有効へ　ATEXECと違いOAT.hと無関係なので問題なし。
		// if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (true) {
			for (int kk = 0; kk < TuneRegionList->Count; kk++) {
				TuneRegion = (TTuneRegion*)TuneRegionList->Items[kk];
				fprintf(fp, "        iusw1_%s_flag = 0\n",
					TuneRegion->Name.c_str());
			}
		}
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
		fprintf(fp,
			"%c     ==== Before Execution-invocation Optimization Routines\n",
			Comment);
		fprintf(fp, "      if (OAT_TYPE .eq. 2) then\n");
		// 長い文字列は、分解して設定する。
		s = s2;
		if (s == "") {
			fprintf(fp, "        OAT_Routines(1:%d) = '%s'\n",
				(int)s2.length(), s2.c_str());
		}
		else {
			for (i = 1; i <= (int)s.length(); i += 35) {
				len = s.length() - i + 1;
				if (len > 35) {
					len = 35;
				}
				fprintf(fp, "        OAT_Routines(%d:%d) = '%s'\n", i,
					i + len - 1, s.substr(i - 1, len).c_str());
			}
		}
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
		fprintf(fp, "%c     ==== Run-time Optimization Routines\n", Comment);
		fprintf(fp, "      if (OAT_TYPE .eq. 3) then\n");

		fprintf(fp, "        OAT_DYNAMICTUNE = .false.\n");
		// 長い文字列は、分解して設定する。
		s = s3;
		if (s == "") {
			fprintf(fp, "        OAT_Routines(1:%d) = '%s'\n",
				(int)s3.length(), s3.c_str());
		}
		else {
			for (i = 1; i <= (int)s.length(); i += 35) {
				len = s.length() - i + 1;
				if (len > 35) {
					len = 35;
				}
				fprintf(fp, "        OAT_Routines(%d:%d) = '%s'\n", i,
					i + len - 1, s.substr(i - 1, len).c_str());
			}
		}
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");

		if (MainF->OMP_InnerF) {
			fprintf(fp, "!$omp flush(OAT_Routines,OAT_DYNAMICTUNE)\n");
			for (int kk = 0; kk < TuneRegionList->Count; kk++) {
				TuneRegion = (TTuneRegion*)TuneRegionList->Items[kk];
				fprintf(fp, "!$omp flush(iusw1_%s_flag)\n",
					TuneRegion->Name.c_str());
			}
			fprintf(fp, "\n");
			fprintf(fp, "       endif\n");
			fprintf(fp, "\n");
			fprintf(fp, "!$omp barrier\n");
			fprintf(fp, "\n");
		}

		fprintf(fp, "      return\n");
		if (MainF->SrcCodeType == MainF->sctFortran90) {
			fprintf(fp, "      end subroutine OAT_ATset\n");
			// Change 2013/06/20
		}
		else {
			fprintf(fp, "      end\n");
		}
		fprintf(fp,
			"%c     ============================================================\n"
			, Comment);
		fprintf(fp, "\n");
		fprintf(fp, "\n");
	}
	//
	// OAT_SetParam(Type,Routine,n,isw)
	// isw パラメータを所得する。 複数のTR(名前で識別)に対応する。
	//

	fprintf(fp, "%c     === OAT_SetParm\n", Comment);
	fprintf(fp,
		"%c     ============================================================\n"
		, Comment);
	/** ************************************************************************************************ */
	//
	// Kogakuin Irie
	// OAT_SetParmの引数を変更
	// 既存コードはコメントアウト
	//
	// if(MainF->SrcCodeType == MainF->sctFortran90){
	// if(MainF->Call_ATExec_Script != NULL){
	// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
	// }else{
	// ArgStr = "";
	// }
	// //
	// //	n も引数が必要だが、すでにある場合もある。 Dynamicでの引数を持ってくる必要がなければ
	// //	問題ないんだが。　nの変数名を base_n に変えるのが良いのでは。
	// //
	////		s = "      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine , isw"+ArgStr+")";
	// s = "      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine , n_bpset , isw"+ArgStr+")";
	// fprintf(fp,"%s\n",SepLongStr(s).c_str());
	// if(MainF->MPIF){
	// fprintf(fp,"      use mpi\n");  // Add 2013/06/20
	// }
	// fprintf(fp,"      integer OAT_TYPE\n");
	// fprintf(fp,"      character*%d OAT_Routine\n",MainF->CharMaxLen);
	////		fprintf(fp,"      integer isw\n");
	// fprintf(fp,"      integer n_bpset , isw\n");
	// //	ArgValでの引数リストを追加。Fortran用は、改行コードを含む。
	// //
	// if(MainF->Call_ATExec_Script != NULL){
	// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true,"","");
	// fprintf(fp,"%s",ArgStr.c_str());
	// }
	// }else{
	// fprintf(fp,"      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine, n, isw)\n");
	// fprintf(fp,"      integer OAT_TYPE\n");
	// fprintf(fp,"      character*%d OAT_Routine\n",MainF->CharMaxLen);
	// fprintf(fp,"      integer n,isw\n");
	// }

	//
	// Dynamic以外はSetParamの引数は最小にする。2016/03/12
	// call OAT_xxx_OAT()での呼び出し時に、その先からSetParama_OATを呼ぶ必要が出てくるため。
	//
	bool DynamicRegionF = false;
	for (int i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			DynamicRegionF = true;
			break;
		}
	}
	if (DynamicRegionF) {
		if (MainF->Call_ATExec_Script == NULL) {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(false, "", "");
			delete Script;
		}
		else {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "", "");
		}
	}
	else {
		ArgStr = "";
	}
	if (fpAdd == NULL) {
		s =
		"      subroutine OAT_SetParm(OAT_TYPE, OAT_Routine, n_bpset, isw"
		+ ArgStr + ")";
	}
	else {
		s =
		"      subroutine OAT_SetParm_OAT(OAT_TYPE, OAT_Routine, n_bpset, isw"
		+ ArgStr + ")";
	}
	fprintf(fp, "%s\n", SepLongStr(s).c_str());

	if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (MainF->MPIF) {
			// fprintf(fp,"      use mpi\n");
		}
	}

	fprintf(fp, "      integer OAT_TYPE\n");
	fprintf(fp, "      character*%d OAT_Routine\n", MainF->CharMaxLen);
	fprintf(fp, "      integer n_bpset , isw\n");
	if (DynamicRegionF) {
		if (MainF->Call_ATExec_Script != NULL) {
			ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "%s", ArgStr.c_str());
		}
		else {
			TScript *Script = new TScript(MainF->TokenList, 0, NULL,
				ValDataList);
			ArgStr = Script->GetATExecArgStr(true, "", "");
			fprintf(fp, "%s", ArgStr.c_str());
			delete Script;
		}
	}
	//
	// ここまで
	//
	/** ************************************************************************************************ */
	fprintf(fp, "\n");
	fprintf(fp, "      include 'OAT.h'\n");
	fprintf(fp, "\n");

	if (FittingF) { // Fitting用配列
		fprintf(fp, "%c     !!!!!! fitting用配列\n", Comment);
		fprintf(fp, "%c     === for target coefficients\n", Comment);
		fprintf(fp,
			"      real*8  a_lsm(0:OATLSM_MAX_M, 0:OATLSM_MAX_NPARM-1)\n");
		fprintf(fp, "      real*8  dtemp\n");
		fprintf(fp, "\n");
		fprintf(fp, "      integer nparm, nsamp\n");
	}
	fprintf(fp, "\n");
	fprintf(fp, "      integer ibsw\n");
//	fprintf(fp, "      real*8 OAT_Wtime\n"); // Add 2016/03/29
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      integer inum,i,j\n");
		fprintf(fp, "\n");
		fprintf(fp, "      integer ierr\n");
	}
	fprintf(fp, "\n");

	fprintf(fp, "      character*100 cbuf\n");
	fprintf(fp, "      character*20 digit\n");
	fprintf(fp, "      integer oat_i,oat_j,oat_inum\n"); // Add 2016/03/
	fprintf(fp, "\n");
	if (MainF->OMP_InnerF) {
		fprintf(fp, "\n");
		fprintf(fp, "      if (oat_mythread_num .eq. 0) then\n");
		fprintf(fp, "\n");
	}
	fprintf(fp, "\n");

	TempLineNo = 100;
	fprintf(fp, "%c     ==== Install Optimization\n", Comment);
	fprintf(fp, "      if (OAT_TYPE .eq. 1) then\n");

	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
			if (MainF->SrcCodeType != MainF->sctC) {
				TuneRegion->OutputSetParamCode_Fortran(fp, TempLineNo, fpAdd);
			}
			else {
				TuneRegion->OutputSetParamCode_C(fp, TempLineNo);
			}
			TempLineNo += 10;
		}
	}
	fprintf(fp, "      endif\n");
	fprintf(fp, "%c     === end of OAT_Install\n", Comment);
	fprintf(fp, "%c     -----------------------------------------------\n",
		Comment);

	fprintf(fp, "\n");
	fprintf(fp, "%c     ==== Before Execution-invocation Optimization\n",
		Comment);
	fprintf(fp, "      if (OAT_TYPE .eq. 2) then\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgStatic) {
			if (MainF->SrcCodeType != MainF->sctC) {
				TuneRegion->OutputSetParamCode_Fortran(fp, TempLineNo, fpAdd);
			}
			else {
				TuneRegion->OutputSetParamCode_C(fp, TempLineNo);
			}
			TempLineNo += 10;
		}
	}
	fprintf(fp, "      endif\n");
	fprintf(fp, "%c     === end of OAT_Static\n", Comment);
	fprintf(fp, "%c     -----------------------------------------------\n",
		Comment);

	fprintf(fp, "\n");
	fprintf(fp, "%c     ==== Run-time Optimization\n", Comment);
	fprintf(fp, "      if (OAT_TYPE .eq. 3) then\n");
	//
	// g95コンパイラでのエラー回避（Operands of comparision oprator are Logical(4)/Logical(4)
	// 論理値どうしの比較を行っているエラー回避のため、論理値をifで直接使用に修正した。
	// 2007/1/18
	//
	// fprintf(fp,"        if (OAT_DYNAMICTUNE .eq. .true.) then\n");
	fprintf(fp, "        if (OAT_DYNAMICTUNE) then\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			// Dynamicは、SetParamで、ファイルから読込みしない。
			/** ***************************************************************************************************** */
			//
			// Kogakuin Irie
			// 必要な引数を表示させるための変更
			// 既存コードはコメントアウト
			//
			// if(MainF->SrcCodeType == MainF->sctFortran90){
			// if(MainF->Call_ATExec_Script != NULL){
			// ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,"","");
			// }else{
			// ArgStr = "";
			// }
			// s = "          call OAT_ATexecDynamic"+TuneRegion->Name+"(OAT_Routine"+ArgStr+")";
			// fprintf(fp,"%s\n",SepLongStr(s).c_str());
			// }else{
			// fprintf(fp,"          call OAT_ATexecDynamic%s(OAT_Routines,n,isw)\n",TuneRegion->Name.c_str());
			// }
			if (MainF->Call_ATExec_Script != NULL) {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
			}
			else {
				TScript *Script = new TScript(MainF->TokenList, 0, NULL,
					ValDataList);
				ArgStr = Script->GetATExecArgStr(false, "", "");
				delete Script;
			}
			s = "          call OAT_ATexecDynamic" + TuneRegion->Name +
			"(OAT_Routine,isw" + ArgStr + ")";
			fprintf(fp, "%s\n", SepLongStr(s).c_str());
			//
			// ここまで
			//
			/** ***************************************************************************************************** */
		}
	}
	fprintf(fp, "        else\n");
	fprintf(fp, "          isw = 1\n");
	fprintf(fp, "        endif\n");
	fprintf(fp, "\n");
	if (MainF->OMP_InnerF) {
		fprintf(fp, "!$omp flush(isw)\n");
	}
	fprintf(fp, "      endif\n");
	fprintf(fp, "%c     === end of OAT_Dynamic\n", Comment);
	fprintf(fp, "%c     -----------------------------------------------\n",
		Comment);
	fprintf(fp, "\n");
	if (MainF->OMP_InnerF) {
		fprintf(fp, "       endif\n");
		fprintf(fp, "!$omp barrier\n");
		fprintf(fp, "\n");
	}
	fprintf(fp, "      return\n");
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (fpAdd == NULL) {
			fprintf(fp, "      end subroutine OAT_SetParm\n");
		}
		else {
			fprintf(fp, "      end subroutine OAT_SetParm_OAT\n");
		}
	}
	else {
		fprintf(fp, "      end\n"); // Change 2013/06/20
	}
	fprintf(fp, "\n");
	fprintf(fp, "\n");

	//
	// 文字列操作のサブルーチン
	//
	if (fpAdd == NULL) {
		fprintf(fp, "      subroutine OATCharToNum(coption, inum)\n");
	}
	else {
		fprintf(fp, "      subroutine OATCharToNum_OAT(coption, inum)\n");
	}
	fprintf(fp, "      character*20 coption\n");
	fprintf(fp, "      integer inum\n");
	fprintf(fp, "\n");
	fprintf(fp, "      integer j\n");
	fprintf(fp, "      integer idec\n");
	fprintf(fp, "      character ctemp\n");
	fprintf(fp, "\n");
	fprintf(fp, "      inum = 0\n");
	fprintf(fp, "      j = 1\n");
	fprintf(fp, "      do while(coption(j:j) .ne. ' ')\n");
	fprintf(fp, "         ctemp = coption(j:j)\n");
	fprintf(fp, "         if (ctemp .eq. ' ') goto 100\n");
	fprintf(fp, "         if (ctemp .eq. '0') idec = 0\n");
	fprintf(fp, "         if (ctemp .eq. '1') idec = 1\n");
	fprintf(fp, "         if (ctemp .eq. '2') idec = 2\n");
	fprintf(fp, "         if (ctemp .eq. '3') idec = 3\n");
	fprintf(fp, "         if (ctemp .eq. '4') idec = 4\n");
	fprintf(fp, "         if (ctemp .eq. '5') idec = 5\n");
	fprintf(fp, "         if (ctemp .eq. '6') idec = 6\n");
	fprintf(fp, "         if (ctemp .eq. '7') idec = 7\n");
	fprintf(fp, "         if (ctemp .eq. '8') idec = 8\n");
	fprintf(fp, "         if (ctemp .eq. '9') idec = 9\n");
	fprintf(fp, "         inum = inum*10 + idec\n");
	fprintf(fp, "         j = j + 1\n");
	fprintf(fp, "       enddo\n");
	fprintf(fp, " 100   continue\n");
	fprintf(fp, "\n");
	fprintf(fp, "      return\n");
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		if (fpAdd == NULL) {
			fprintf(fp, "      end subroutine OATCharToNum\n");
		}
		else {
			fprintf(fp, "      end subroutine OATCharToNum_OAT\n");
		}
	}
	else {
		fprintf(fp, "      end\n"); // Change 2013/06/20
	}
	fprintf(fp, "\n");
	fprintf(fp,
		"%c     ============================================================\n"
		, Comment);
	fprintf(fp, "\n");
	fprintf(fp, "\n");

	//
	// OAT_ATexec(Type,Routines)
	//
	if (fpAdd != NULL) {
		return; // Routinesに追加の場合は、ここまでで ATexecは追加しない。
	}
	//
	fprintf(fp, "%c     === OAT_ATexec\n", Comment);
	fprintf(fp,
		"%c     ============================================================\n"
		, Comment);
	if (MainF->Call_ATExec_Script != NULL) {
		ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "", "");
		s = "      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines" + ArgStr + ")";
		fprintf(fp, "%s\n", SepLongStr(s).c_str());
	}
	/** ************************************************************************ */
	//
	// Kogakuin Irie
	// 引数が不足しているため修正，およびuse文の消去
	// 既存コードはコメントアウト
	//
	// else{
	// fprintf(fp,"      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines)\n");
	// }
	else {
		TScript *Script = new TScript(MainF->TokenList, 0, NULL, ValDataList);
		ArgStr = Script->GetATExecArgStr(false, "", "");
		s = "      subroutine OAT_ATexec(OAT_TYPE, OAT_Routines" + ArgStr + ")";
		fprintf(fp, "%s\n", SepLongStr(s).c_str());
		delete Script;
	}
	if (MainF->MPIF) {
		// fprintf(fp,"       use mpi\n");
	}
	//
	// ここまで
	//
	/** ************************************************************************ */
	fprintf(fp, "      integer   OAT_TYPE\n");
	fprintf(fp, "      character*%d OAT_Routines\n", MainF->CharMaxLen);
	//
	// ここにArgValでの引数リストを追加。Fortran用は、改行コードを含んだ形とする。
	//
	if (MainF->Call_ATExec_Script != NULL) {
		ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(true, "", "");
		fprintf(fp, "%s", ArgStr.c_str());
	}
	/** ************************************************************************ */
	//
	// Kogakuin Irie
	// 仮引数の宣言部分を追加
	//
	else {
		TScript *Script = new TScript(MainF->TokenList, 0, NULL, ValDataList);
		ArgStr = Script->GetATExecArgStr(true, "", "");
		fprintf(fp, "%s", ArgStr.c_str());
		delete Script;
	}
	//
	// ここまで
	//
	/** ************************************************************************ */

#if 0 // この定義は oat.h で行うこと。ここで追加すると従来のコードと重複する。2016/03/13
	/** ********************************************** */
	//
	// Kogakuin Irie
	// 未宣言変数OAT_ATEXEC_FLAGを宣言させる処理を追加
	//
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      integer OAT_ATEXEC_FLAG\n");
	}
	//
	// ここまで
	//
	/** ********************************************** */
#endif

	fprintf(fp, "\n");
	fprintf(fp, "      include 'OAT.h'\n");
	/** ******************************************** */
	//
	// Kogakuin Irie
	// mpif.hをインクルードさせる処理を追加
	//
	if (MainF->MPIF) {
		fprintf(fp, "      include 'mpif.h'\n");
	}
	//
	// ここまで
	//
	/** ******************************************** */
	fprintf(fp, "\n");
	fprintf(fp, "      character*8 OAT_EXEC_Env\n");
	fprintf(fp, "      integer cp\n");

	if ((MainF->SrcCodeType == MainF->sctFortran77)||
		(MainF->SrcCodeType == MainF->sctFortran90)) {
		fprintf(fp, "\n");
		fprintf(fp, "      integer ierr\n");
		fprintf(fp, "\n");
		fprintf(fp, "      real*8 t1, t2\n");
//		fprintf(fp, "      real*8 OAT_WTime\n");
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		if (MainF->OMP_InnerF) {
			fprintf(fp, "       if (oat_mythread_num .eq. 0) then\n");
			fprintf(fp, "\n");
		}
		/* Cにはない部分の処理をすべてコメントアウト */
		/* （コンパイルを通すため） */
		fprintf(fp, "\n");
		fprintf(fp, "\n");
		fprintf(fp, "      OAT_ATEXEC_FLAG = 1\n");
		fprintf(fp, "\n");
		fprintf(fp, "      if (oat_myid .eq. 0) then\n");
		fprintf(fp, "        call getenv(\"OAT_EXEC\",OAT_EXEC_Env)\n");
		fprintf(fp, "      endif\n");
		if (MainF->MPIF) {
			fprintf(fp,
				"       call MPI_BCAST(OAT_EXEC_Env,8,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)\n");
		}
		fprintf(fp, "\n");
		fprintf(fp, "      if(TRIM(OAT_EXEC_Env) .ne. \"\")then\n");
		fprintf(fp, "        if(TRIM(OAT_EXEC_Env) .ne. \"1\")then\n");
		fprintf(fp, "          OAT_ATEXEC_FLAG = 0\n");
		fprintf(fp, "          return\n");
		fprintf(fp, "        endif\n");
		fprintf(fp, "      else\n");
		fprintf(fp, "        if (oat_myid .eq. 0) then\n");
		fprintf(fp, "          call getenv(\"OAT_ATEXEC\",OAT_EXEC_Env)\n");
		fprintf(fp, "        endif\n");
		if (MainF->MPIF) {
			fprintf(fp,
				"       call MPI_BCAST(OAT_EXEC_Env,8,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)\n");
		}
		fprintf(fp, "\n");
		fprintf(fp, "        if(TRIM(OAT_EXEC_Env) .ne. \"\")then\n");
		fprintf(fp, "          if(TRIM(OAT_EXEC_Env) .ne. \"1\")then\n");
		fprintf(fp, "            OAT_ATEXEC_FLAG = 0\n");
		fprintf(fp, "            return\n");
		fprintf(fp, "          endif\n");
		fprintf(fp, "        endif\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
		if (MainF->OMP_InnerF) {
			fprintf(fp, "!$omp flush(OAT_ATEXEC_FLAG)\n");
			fprintf(fp, "\n");
			fprintf(fp, "       endif\n");
			fprintf(fp, "!$omp barrier\n");
			fprintf(fp, "\n");
			fprintf(fp, "       if (OAT_ATEXEC_FLAG .eq. 0) return\n");
			fprintf(fp, "!      print *, oat_myid, OAT_ATEXEC_FLAG\n");
			fprintf(fp, "\n");
			fprintf(fp, "!----\n");
			fprintf(fp, "\n");
			fprintf(fp, "       if (oat_mythread_num .eq. 0) then\n");
		}
		//
		// ここまで
		//
		fprintf(fp, "\n");
		fprintf(fp, "      t1 = OAT_Wtime()\n");
		fprintf(fp, "\n");

	}
	else {
		/** ****************************************************************************** */
		//
		// Kogakuin Irie
		// #if と #else で処理する内容を逆にする
		// 既存コードはコメントアウト
		//
		// #if 1
		// fprintf(fp,"\n");
		// fprintf(fp,"      call getenv(\"OAT_EXEC\",OAT_EXEC_Env)\n");
		// fprintf(fp,"      if(TRIM(OAT_EXEC_Env) .eq. \"\")then\n");
		// fprintf(fp,"        call getenv(\"OAT_ATEXEC\",OAT_EXEC_Env)\n");
		// fprintf(fp,"      endif\n");
		// fprintf(fp,"      if(OAT_EXEC_Env .ne. \"1\")then\n");
		// fprintf(fp,"        return\n");
		// fprintf(fp,"      endif\n");
		// fprintf(fp,"\n");
		// #else
		// fprintf(fp,"\n");
		// fprintf(fp,"      call getenv(\"OAT_EXEC\",OAT_EXEC_Env)\n");
		// fprintf(fp,"      if(TRIM(OAT_EXEC_Env) .ne. \"\")then\n");
		// fprintf(fp,"        if(TRIM(OAT_EXEC_Env) .ne. \"1\")then\n");
		// fprintf(fp,"          return\n");
		// fprintf(fp,"        endif\n");
		// fprintf(fp,"      else\n");
		// fprintf(fp,"        call getenv(\"OAT_ATEXEC\",OAT_EXEC_Env)\n");
		// fprintf(fp,"        if(TRIM(OAT_EXEC_Env) .ne. \"\")then\n");
		// fprintf(fp,"          if(TRIM(OAT_EXEC_Env) .ne. \"1\")then\n");
		// fprintf(fp,"            return\n");
		// fprintf(fp,"          endif\n");
		// fprintf(fp,"        endif\n");
		// fprintf(fp,"      endif\n");
		// fprintf(fp,"\n");
		// #endif
#if 1
		fprintf(fp, "\n");
		fprintf(fp, "      call getenv(\"OAT_EXEC\",OAT_EXEC_Env)\n");
		fprintf(fp, "      if(TRIM(OAT_EXEC_Env) .ne. \"\")then\n");
		fprintf(fp, "        if(TRIM(OAT_EXEC_Env) .ne. \"1\")then\n");
		fprintf(fp, "          return\n");
		fprintf(fp, "        endif\n");
		fprintf(fp, "      else\n");
		fprintf(fp, "        call getenv(\"OAT_ATEXEC\",OAT_EXEC_Env)\n");
		fprintf(fp, "        if(TRIM(OAT_EXEC_Env) .ne. \"\")then\n");
		fprintf(fp, "          if(TRIM(OAT_EXEC_Env) .ne. \"1\")then\n");
		fprintf(fp, "            return\n");
		fprintf(fp, "          endif\n");
		fprintf(fp, "        endif\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
#else
		fprintf(fp, "\n");
		fprintf(fp, "      call getenv(\"OAT_EXEC\",OAT_EXEC_Env)\n");
		fprintf(fp, "      if(TRIM(OAT_EXEC_Env) .eq. \"\")then\n");
		fprintf(fp, "        call getenv(\"OAT_ATEXEC\",OAT_EXEC_Env)\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "      if(OAT_EXEC_Env .ne. \"1\")then\n");
		fprintf(fp, "        return\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
#endif
		//
		// ここまで
		//
		/** ****************************************************************************** */
	}

	fprintf(fp, "%c     ==== Install Optimization\n", Comment);
	fprintf(fp, "      if (OAT_TYPE .eq. 1) then\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgInstall) {
#if 1
			fprintf(fp, "        cp = index(OAT_Routines,'%s')\n",
				TuneRegion->Name.c_str());
			fprintf(fp, "        if (cp .ne. 0) then\n");
			fprintf(fp, "          cp = cp + len('%s')\n",
				TuneRegion->Name.c_str());
			fprintf(fp, "          if (OAT_Routines(cp:cp) .eq. ',') then\n");
			fprintf(fp, "            cp = 99999\n");
			fprintf(fp, "          endif\n");
			fprintf(fp, "          if (cp .ge. len(OAT_Routines)) then\n");
			if (MainF->Call_ATExec_Script != NULL) {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				s = "            call OAT_ATexecInstall" + TuneRegion->Name +
				"(OAT_Routines" + ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());

			}
			else {
				fprintf(fp,
					"            call OAT_ATexecInstall%s(OAT_Routines)\n",
					TuneRegion->Name.c_str());
			}
			fprintf(fp, "          endif\n");
#else
			fprintf(fp, "        if (index(OAT_Routines,'%s') .ne. 0) then\n",
				TuneRegion->Name.c_str());
			if (MainF->Call_ATExec_Script != NULL) {
				ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false, "",
					"");
				s = "          call OAT_ATexecInstall" + TuneRegion->Name +
				"(OAT_Routines" + ArgStr + ")";
				s = SepLongStr(s);
				fprintf(fp, "%s\n", s.c_str());

			}
			else {
				fprintf(fp,
					"          call OAT_ATexecInstall%s(OAT_Routines)\n", TuneRegion->Name.c_str());
			}
#endif
			fprintf(fp, "        endif\n");
		}
	}
	fprintf(fp, "      endif\n");
	fprintf(fp, "\n");
	fprintf(fp, "%c     ==== Before Execution-invocation Optimization\n",
		Comment);
	fprintf(fp, "      if (OAT_TYPE .eq. 2) then\n");
	if (true) { // 2015/02/28
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			if (TuneRegion->TuneGroup == tgStatic) {
#if 1
				fprintf(fp, "        cp = index(OAT_Routines,'%s')\n",
					TuneRegion->Name.c_str());
				fprintf(fp, "        if (cp .ne. 0) then\n");
				fprintf(fp, "          cp = cp + len('%s')\n",
					TuneRegion->Name.c_str());
				fprintf(fp,
					"          if (OAT_Routines(cp:cp) .eq. ',') then\n");
				fprintf(fp, "            cp = 99999\n");
				fprintf(fp, "          endif\n");
				fprintf(fp, "          if (cp .ge. len(OAT_Routines)) then\n");
				if (MainF->Call_ATExec_Script != NULL) {
					ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,
						"", "");
					s = "            call OAT_ATexecStatic" +
					TuneRegion->Name + "(OAT_Routines" + ArgStr + ")";
					s = SepLongStr(s);
					fprintf(fp, "%s\n", s.c_str());
				}
				else {
					fprintf(fp,
						"            call OAT_ATexecStatic%s(OAT_Routines)\n",
						TuneRegion->Name.c_str());
				}
				fprintf(fp, "          endif\n");
#else
				fprintf(fp,
					"        if (index(OAT_Routines,'%s') .ne. 0) then\n",
					TuneRegion->Name.c_str());
				if (MainF->Call_ATExec_Script != NULL) {
					ArgStr = MainF->Call_ATExec_Script->GetATExecArgStr(false,
						"", "");
					s = "          call OAT_ATexecStatic" + TuneRegion->Name +
					"(OAT_Routines" + ArgStr + ")";
					s = SepLongStr(s);
					fprintf(fp, "%s\n", s.c_str());
				}
				else {
					fprintf(fp,
						"          call OAT_ATexecStatic%s(OAT_Routines)\n",
						TuneRegion->Name.c_str());
				}
#endif
				fprintf(fp, "        endif\n");
			}
		}
	}
	else {
		for (i = 0; i < TuneRegionList->Count; i++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
			if (TuneRegion->TuneGroup == tgStatic) {
#if 1
				fprintf(fp, "        cp = index(OAT_Routines,'%s')\n",
					TuneRegion->Name.c_str());
				fprintf(fp, "        if (cp .ne. 0) then\n");
				fprintf(fp, "          cp = cp + len('%s')\n",
					TuneRegion->Name.c_str());
				fprintf(fp,
					"          if (OAT_Routines(cp:cp) .eq. ',') then\n");
				fprintf(fp, "            cp = 99999\n");
				fprintf(fp, "          endif\n");
				fprintf(fp, "          if (cp .ge. len(OAT_Routines)) then\n");
				fprintf(fp,
					"            call OAT_ATexecStatic%s(OAT_Routines)\n",
					TuneRegion->Name.c_str());
				fprintf(fp, "          endif\n");
#else
				fprintf(fp,
					"        if (index(OAT_Routines,'%s') .ne. 0) then\n",
					TuneRegion->Name.c_str());
				fprintf(fp,
					"          call OAT_ATexecStatic%s(OAT_Routines)\n"
					, TuneRegion->Name.c_str());
#endif
				fprintf(fp, "        endif\n");
			}
		}
	}

	fprintf(fp, "      endif\n");
	fprintf(fp, "\n");
	fprintf(fp, "%c     ==== Run-time Optimization\n", Comment);
	fprintf(fp, "      if (OAT_TYPE .eq. 3) then\n");
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		if (TuneRegion->TuneGroup == tgDynamic) {
			///		  	fprintf(fp,"        if (index(OAT_Routines,'%s') .ne. 0) then\n",TuneRegion->Name);
			// fprintf(fp,"          call OAT_ATexecDynamic%s(OAT_Routines)\n",TuneRegion->Name);
			fprintf(fp, "        OAT_DYNAMICTUNE = .true.\n");
		}
	}
	fprintf(fp, "      endif\n");
	fprintf(fp, "\n");
	fprintf(fp, "\n");

	if ((MainF->SrcCodeType == MainF->sctFortran77)||
		(MainF->SrcCodeType == MainF->sctFortran90)) {
		fprintf(fp, "      t2 = OAT_Wtime()\n");
		fprintf(fp, "\n");
		fprintf(fp, "      if (OAT_DEBUG .ge. 1)then\n");
		fprintf(fp, "        if (oat_myid .eq. 0) then\n");
		fprintf(fp, "          print *, \"Auto-tuning time = \",t2-t1\n");
		fprintf(fp, "        endif\n");
		fprintf(fp, "      endif\n");
		fprintf(fp, "\n");
		if (MainF->OMP_InnerF) {
			fprintf(fp, "      endif\n");
			fprintf(fp, "!$omp barrier\n");
		}
		fprintf(fp, "\n");

	}
	fprintf(fp, "      return\n");
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      end subroutine OAT_ATexec\n");
	}
	else {
		fprintf(fp, "      end\n"); // Change 2013/06/20
	}
	fprintf(fp,
		"%c     ============================================================\n"
		, Comment);
	fprintf(fp, "\n");

	//
	// OAT_ATexecGGGFFF サブルーチンのコードの生成 (時間計測->ファイル)
	//
	// for(i = TuneRegionList->Count-1 ; i >= 0 ; i--){
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		TuneRegion->OutputAutoExecCode_Fortran(fp);
	}
	if (FittingF) { // Fitting用サブルーチンを追加
		FILE *fpSrc;
		char cBuff[1024];
		string fname;
		string DirName;

#if 0
		fname = MainF->SrcFileNameList.Strings[0];
		DirName = fname.substr(0, fname.rfind("/"));
		DirName = fname.substr(0, DirName.rfind("/"));
		fname = DirName + "/OAT_Data/ControlSubrutions.f";
#else
		fname = "./OAT_Data/ControlSubrutions.f";
#endif
		fpSrc = fopen(fname.c_str(), "rt"); // 出力ファイル
		if (fpSrc == NULL) {
			MainF->ErrMessage(-1, "ファイル " + fname + "が開けません。");
		}
		else {
			while (fgets(cBuff, sizeof(cBuff), fpSrc) != NULL) {
				fprintf(fp, "%s", cBuff);
			}
			fclose(fpSrc);
		}
		fprintf(fp, "\n");
	}
	// 時間計測用サブルーチンを追加 2012/03/22
	fprintf(fp, "      function OAT_Wtime()\n");
	fprintf(fp, "      real*8 OAT_Wtime\n");
	/* -mpi オプションがあっても omp_get_wtime()を使用に変更 2013/08/04
	呼び出し元側がMainF->TimeFuncを参照。
	if (MainF->MPIF) {
	fprintf(fp,"\n");
	fprintf(fp,"      OAT_Wtime = MPI_Wtime()\n");
	fprintf(fp,"\n");
	}else {
	 */
	fprintf(fp, "      real*8 omp_get_wtime\n");
	fprintf(fp, "\n");
	fprintf(fp, "      OAT_Wtime = omp_get_wtime()\n");
	fprintf(fp, "\n");
	// }
	if (MainF->SrcCodeType == MainF->sctFortran90) {
		fprintf(fp, "      end function OAT_Wtime\n");
		fprintf(fp, "\n");
		fprintf(fp, "      end module ppohAT_ControlRoutines\n");
	}
	else {
		fprintf(fp, "      end\n");
	}
	fprintf(fp, "\n");
	//
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// OAT_(元のソースファイル名).c を作成する。
//
// Region（一番外側）を SetParam() call Sub() に置き換える。
// また、NestしたRegion内は、コメントとして、 TokenEndPosまでSkipする。
// NestRegion内の sct_Commandも、Nest先のRegionでの処理となる。
//
// ・sct_command の変換
// ・sct_install ... のブロックをコメントアウト（コメントを残す）
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::MakeSrcCode_C() {
	int i;
	TToken *Token;
	TScript *Script;
	string s;
	TTuneRegion *TuneRegion;
	int SetDefinePos = 0;
	bool IncludeReqF = true;
	int cp;
	int SrcFileIdx;
	string SrcFileName;
	int StartTokenPos, EndTokenPos;
	FILE *fpOut;

	for (SrcFileIdx = 0; SrcFileIdx < MainF->SrcFileNameList.Count;
		SrcFileIdx++) {
		fpOut = (FILE*)fpOutList.Items[SrcFileIdx];
		SrcFileName = MainF->SrcFileNameList.Strings[SrcFileIdx];
		StartTokenPos = (long)MainF->SrcFileNameList.Objects[SrcFileIdx];
		if (SrcFileIdx + 1 < MainF->SrcFileNameList.Count) {
			EndTokenPos = (long)MainF->SrcFileNameList.Objects[SrcFileIdx + 1];
		}
		else {
			EndTokenPos = TokenList->Count;
		}
		//
		// 最初のモジュールの前にグローバル定義を挿入する。
		// #とコメント以外の直前に挿入する。
		//
		for (i = StartTokenPos; i < EndTokenPos; i++) {
			Token = (TToken*)TokenList->Items[i];
			if ((Token->TokId >= tid_SharpStart) &&
				(Token->TokId <= tid_SharpEnd)) {
				for (; i < TokenList->Count; i++) {
					Token = (TToken*)TokenList->Items[i];
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
			}
			if (Token->TokId >= tid_KeyWordStart) {
				SetDefinePos = i;
				break;
			}
		}
		for (i = StartTokenPos; i < EndTokenPos; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (i == SetDefinePos) {
				fprintf(fpOut, "#include \"OAT_Routines.h\"\n");
				fprintf(fpOut, "\n");
				for (int kk = 0; kk < TuneRegionList->Count; kk++) {
					TuneRegion = (TTuneRegion*)TuneRegionList->Items[kk];
					/** *************************************** */
					//
					// Kogakuin Irie
					// 変数の二重宣言を回避するため修正
					// 既存コードはコメントアウト
					//
					// s += "int OAT_iusw1_"+TuneRegion->Name+";";
					if (SrcFileIdx == 0) {
						s = "int OAT_iusw1_" + TuneRegion->Name + " = 0;";
					}
					else {
						s = "extern int OAT_iusw1_" + TuneRegion->Name + ";";
					}
					//
					// ここまで
					//
					/** *************************************** */
					fprintf(fpOut, "%s\n", s.c_str());
					if (SrcFileIdx == 0) {
						s = "int OAT_iusw1_" + TuneRegion->Name + "_flag = 0;";
					}
					else {
						s = "extern int OAT_iusw1_" + TuneRegion->Name +
						"_flag;";
					}
					//
					// ここまで
					//
					/** *************************************** */
					fprintf(fpOut, "%s\n", s.c_str());
				}
				fprintf(fpOut, "\n");
			}
			if (Token->Script != NULL) {
				Script = (TScript*)Token->Script;
				if (Script->ScRegion == scr_start) {
					// Codeの置き換え
					TuneRegion = (TTuneRegion*)Script->TuneRegion;
					if (TuneRegion->TokenEndPos > i) {
						if (!TuneRegion->UsedDynPefThis) {
							TuneRegion->OutputSrcCode(fpOut, "", 0);
							// 置き換えコードを出力
						}
						else {
							TuneRegion->OutputSrcCode(fpOut, "", 3);
							// 置き換えコードを出力
						}
						i = TuneRegion->TokenEndPos - 1; // Top Regionの中をスキップする。
						continue;
					}
				}
				else if (Script->ScType == sct_DynPefThis) {
					//
					// Dynamicの実行 SetParam() を指定
					//
					TScValData *ScValData = NULL;
					int kk;

					if (Script->ScValDataList->Count > 0) {
						ScValData = (TScValData*)Script->ScValDataList->Items
						[0];
						s = ScValData->Str; // ＴＲ名 ""付き
						while (s.find("\"") != string::npos) {
							s.erase(s.find("\""), 1);
						};
						while (s.find("\'") != string::npos) {
							s.erase(s.find("\'"), 1);
						};
						for (kk = 0; kk < TuneRegionList->Count; kk++) {
							TuneRegion = (TTuneRegion*)
							TuneRegionList->Items[kk];
							if (s == TuneRegion->Name) {
								TuneRegion->UsedDynPefThis = true;
								TuneRegion->OutputSrcCode(fpOut, "", 2);
								// 置き換えコードを出力
							}
						}
					}
				}
				else if (Script->ScType == sct_SetParm) {
					//
					// call SetParm 指定があれば、そこに 各TuneRegionのSetParmを展開する。
					//
					for (int kk = 0; kk < TuneRegionList->Count; kk++) {
						TuneRegion = (TTuneRegion*)TuneRegionList->Items[kk];
						TuneRegion->OutputSetParmSrcCode(fpOut, "");
					}
				}
				else if (Script->ScType == sct_ATExec) { // ATExec()
					// Script ATExec()部分は、引数追加に置き換える。
					// 引数を自動で追加。先頭２つの引数の後は、ArfStrの引数を追加する。
#if 1
					string TokStr;
					int ArgCount = 0;
					int BrLevel = 0;

					s = "";
					for (int j = 1; j < Script->TokStrList->Count; j++) {
						Token = (TToken*)Script->TokStrList->Objects[j];
						if (Token == NULL) {
							continue;
						}
						TokStr = Token->Str;
						if (TokStr == ",") {
							if (BrLevel == 1) {
								ArgCount++;
							}
						}
						if (TokStr == "(") {
							BrLevel++;
						}
						if (TokStr == ")") {
							BrLevel--;
							if (BrLevel == 0) {
								ArgCount++;
								break;
							}
						}
						s += Token->OrgStr;
					}
					if (MainF->Call_ATExec_Script != NULL) {
						// ArgStrから残りの数の引数を追加する。すでに必要数だけセット済の場合はそのまま。
						string argStr;

						// s += MainF->Call_ATExec_Script->GetATExecArgStr(false,"", "", ArgCount - 2);
						argStr = MainF->Call_ATExec_Script->GetATExecArgStr
						(false, "", "", ArgCount - 2);
						argStr = MainF->Call_ATExec_Script->ReplaseBindValName
						(argStr);
						s += argStr;
					}
					s += ")";
					fprintf(fpOut, "\t%s;\n", s.c_str());
#else
					s = "";
					for (int j = 1; j < Script->TokStrList->Count; j++) {
						Token = (TToken*)Script->TokStrList->Objects[j];
						s += Token->OrgStr;
					}
					fprintf(fpOut, "\t%s;\n", s.c_str());
#endif
				}
				else if (Script->ScType == sct_None) {

				}
				else if (Script->ScType == sct_command) {
					//
					// コマンドを出力する。
					// ここで、OAT_xxx = の値の所得も行う。
					// 現状では、定数のみが使用可能。（変数や式はエラー）
					// callが含まれている場合は、その部分はＣＵＴする。
					// Fortranとの互換性のため。本来は不要。
					//

					s = Script->TokStrList->Strings[0];
					cp = s.find("call ");
					if (cp >= 0) {
						s.erase(cp, strlen("call "));
					}
					for (int j = 0; j < Token->Indent; j++) {
						s = "  " + s;
					}
					// 最後が カンマの場合は、次があるとしてセミコロンなし。2012/02/28
					if (s[s.length() - 1] == ',') {
						fprintf(fpOut, "%s\n", s.c_str());
					}
					else {
						fprintf(fpOut, "%s;\n", s.c_str());
					}

				}
				else {
					//
					// その他の#Pragma
					//
					s = "";
					for (; i < EndTokenPos; i++) {
						Token = (TToken*)TokenList->Items[i];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					fprintf(fpOut, "%s", s.c_str());
				}
				//
				// スクリプトの最後までＳＫＩＰ
				//
				for (; i < EndTokenPos; i++) {
					Token = (TToken*)TokenList->Items[i];
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
			}
			else {
				fprintf(fpOut, "%s", Token->OrgStr.c_str());
			}
		}
		//
		// グローバル変数定義の関係で #includeを最後に移動。 2011/09/08
		//
		if (IncludeReqF) {
			IncludeReqF = false;
			fprintf(fpOut, "\n");
			fprintf(fpOut, "#include \"OAT_ControlRoutines.c\"\n");
			fprintf(fpOut, "#include \"OAT_DynamicRoutines.c\"\n");
			fprintf(fpOut, "#include \"OAT_InstallRoutines.c\"\n");
			fprintf(fpOut, "#include \"OAT_StaticRoutines.c\"\n");
			fprintf(fpOut, "\n");
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// OAT_(元のソースファイル名).f90 を作成する。
//
// Region（一番外側）を SetParam() call Sub() に置き換える。
// また、NestしたRegion内は、コメントとして、 TokenEndPosまでSkipする。
// NestRegion内の sct_Commandも、Nest先のRegionでの処理となる。
//
// ・sct_command の変換
// ・sct_install ... のブロックをコメントアウト（コメントを残す）
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::MakeSrcCode_Fortran(FILE *fpAdd, int tokSPos, int tokEPos) {
	int i;
	TToken *Token;
	TToken *Token2;
	TScript *Script;
	string s;
	string LineStr;
	TTuneRegion *TuneRegion;
	bool IncludeOATF = false;
	int LastLineNo = -1;
	int SrcFileIdx;
	string SrcFileName, OutputFileName;
	int StartTokenPos, EndTokenPos;
	FILE *fpOut;
	TList *InsertIncludePosList;
	TList *InsertUsePosList;
	bool IncludeInContainsInModouleF;
	int ret;
	int InsertModuleHeaderPos;
	int cp;

	InsertIncludePosList = new TList;
	ret = AddIncludePosList(InsertIncludePosList); // Include配置位置を求める。
	if (InsertIncludePosList->Count == 0) {
		MainF->IncludeCodeInContainsF = false;
		InsertIncludePosList->Add((void*)(long)(TokenList->Count - 1));
	}
	else {
		MainF->IncludeCodeInContainsF = true;
	}
	IncludeInContainsInModouleF = (ret == 1);

	InsertUsePosList = new TList;
	AddUsePosList(InsertUsePosList); // Use配置位置を求める。
	// リージョン内でのみ使用されるローカル変数定義の削除リストを作成する。
	CheckAndDeleteLocalValDefine();

	for (SrcFileIdx = 0; SrcFileIdx < MainF->SrcFileNameList.Count;
		SrcFileIdx++) {
		if (fpAdd != NULL) { // 一部をInstallRoutinesに複写する場合に使用。
			fpOut = fpAdd;
			SrcFileName = MainF->SrcFileNameList.Strings[SrcFileIdx];
			StartTokenPos = tokSPos;
			EndTokenPos = tokEPos;
			InsertIncludePosList->Clear(); // includeなし
		}
		else {
			fpOut = (FILE*)fpOutList.Items[SrcFileIdx];
			SrcFileName = MainF->SrcFileNameList.Strings[SrcFileIdx];
			OutputFileName = "OAT_" + SrcFileName.substr
			(SrcFileName.rfind("/") + 1);
			StartTokenPos = (long)MainF->SrcFileNameList.Objects[SrcFileIdx];
			if (SrcFileIdx + 1 < MainF->SrcFileNameList.Count) {
				EndTokenPos = (long)MainF->SrcFileNameList.Objects
				[SrcFileIdx + 1];
			}
			else {
				EndTokenPos = TokenList->Count;
			}
		}
		//
		// insertModeuleHeader指定と同じファイル名かどうかを調べて、挿入位置を探す。
		// 最初のmoduleの後のコメントとuse以外の行の前に挿入する。
		//
		InsertModuleHeaderPos = -1;
		LastLineNo = -1;
		for (i = 0; i < MainF->InsertModulseHeadFileNameList.Count; i++) {
			s = MainF->InsertModulseHeadFileNameList.Strings[i];
			cp = SrcFileName.find(s);
			if (cp != -1) {
				break;
			}
		}
		if (i < MainF->InsertModulseHeadFileNameList.Count) {
			int ModuleF = false;

			for (i = StartTokenPos; i < EndTokenPos; i++) {
				Token = (TToken*)TokenList->Items[i];
				if (Token->LineNo != LastLineNo) {
					LastLineNo = Token->LineNo;
					if (Token->TokId == tid_MODULE) {
						ModuleF = true;
						continue;
					}
					if (Token->TokId == tid_USE) {
						continue;
					}
					if (Token->TokId == tid_Comment) {
						continue;
					}
					if (ModuleF) {
						InsertModuleHeaderPos = i;
						break;
					}
				}
			}
		}
		//
		// 最初のモジュールの前にグローバル定義を挿入する。
		// #とコメント以外の直前に挿入する。
		//
		LineStr = "";
		for (i = StartTokenPos; i < EndTokenPos; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token->LineNo != LastLineNo) {
				LastLineNo = Token->LineNo;
				if (LineStr != "") {
					s = SepLongStr(LineStr);
					fprintf(fpOut, "%s", s.c_str());
				}
				LineStr = "";
			}
			//
			// 一部をInstallRoutinesに複写する場合の処理を行う
			//
			if (fpAdd != NULL) {
				int Idx;
				string SubroutineName;
				TToken *Token2;
				int k;

				//
				// SOUBROUTINEの次のトークンが置換え用の名前と一致すれば xxx_OATに置き換える。
				//
				if ((Token->TokId == tid_SUBROUTINE) && (i < EndTokenPos - 1)) {
					Token2 = (TToken*)TokenList->Items[i + 1];
					SubroutineName = Token2->Str;
					Idx = -1;
					for (k = 0; k < Call_SubroutineNameInRegionList->Count;
						k++) {
#ifdef _WIN32
						if
						(stricmp(Call_SubroutineNameInRegionList->Strings[k]
								.c_str(),
#else
						if
						(strcasecmp(Call_SubroutineNameInRegionList->Strings[k]
								.c_str(),
#endif
								SubroutineName.c_str()) == 0) {
							Idx = k;
						}
					}
					if (Idx != -1) { // 一致
						LineStr += Token->OrgStr + Token2->OrgStr + "_OAT";
						i++;
						continue;
					}
				}
				/*
				//
				//	CALLの次のトークンがOAT_SetParanと一致すれば xxx_OATに置き換える。
				//
				if((Token->TokId == tid_CALL)&&(i < EndTokenPos-1)){
				Token2 = (TToken *)TokenList->Items[i+1];
				SubroutineName = Token2->Str;
				Idx = -1;
				for(k = 0 ; k < Call_SubroutineNameInRegionList->Count ; k++){
				if(stricmp("OAT_SetParam",SubroutineName.c_str()) == 0){
				Idx = k;
				}
				}
				if(Idx != -1){	// 一致
				LineStr += Token->OrgStr + Token2->OrgStr + "_OAT";
				i++;
				continue;
				}
				}
				 */
			}
			if (Token->Script != NULL) {
				Script = (TScript*)Token->Script;
				if (Script->ScRegion == scr_start) {
					// Codeの置き換え
					TuneRegion = (TTuneRegion*)Script->TuneRegion;
					if (TuneRegion->TokenEndPos > i) {
						if (fpAdd != NULL) {
							if (!TuneRegion->UsedDynPefThis) {
								TuneRegion->OutputSrcCode(fpOut, "", 0, 1);
								// 置き換えコードを出力
							}
							else {
								TuneRegion->OutputSrcCode(fpOut, "", 3, 1);
								// 置き換えコードを出力
							}
						}
						else {
							if (!TuneRegion->UsedDynPefThis) {
								TuneRegion->OutputSrcCode(fpOut, "", 0);
								// 置き換えコードを出力
							}
							else {
								TuneRegion->OutputSrcCode(fpOut, "", 3);
								// 置き換えコードを出力
							}
						}
						i = TuneRegion->TokenEndPos; // Top Regionの中をスキップする。
						continue;
					}
				}
				else if (Script->ScType == sct_DynPefThis) {
					//
					// Dynamicの実行 SetParam() を指定
					//
					TScValData *ScValData = NULL;
					int kk;

					if (Script->ScValDataList->Count > 0) {
						ScValData = (TScValData*)Script->ScValDataList->Items
						[0];
						s = ScValData->Str; // ＴＲ名 ""付き
						while (s.find("\"") != string::npos) {
							s.erase(s.find("\""), 1);
						};
						while (s.find("\'") != string::npos) {
							s.erase(s.find("\'"), 1);
						};
						for (kk = 0; kk < TuneRegionList->Count; kk++) {
							TuneRegion = (TTuneRegion*)
							TuneRegionList->Items[kk];
							if (LowerCase(s) == LowerCase(TuneRegion->Name)) {
								// if(CompareText(s,TuneRegion->Name) == 0){
								TuneRegion->UsedDynPefThis = true;
								if (fpAdd != NULL) {
									TuneRegion->OutputSrcCode(fpOut, "", 2, 1);
									// 置き換えコードを出力
								}
								else {
									TuneRegion->OutputSrcCode(fpOut, "", 2);
									// 置き換えコードを出力
								}
							}
						}
					}
				}
				else if (Script->ScType == sct_ATExec) { // ATExec()
					// Script ATExec()部分は、引数追加に置き換える。
					// 引数を自動で追加。先頭２つの引数の後は、ArfStrの引数を追加する。
					string TokStr;
					int ArgCount = 0;
					int BrLevel = 0;

					s = "      call ";
					for (int j = 1; j < Script->TokStrList->Count; j++) {
						Token = (TToken*)Script->TokStrList->Objects[j];
						if (Token == NULL) {
							continue;
						}
						TokStr = Token->Str;
						if (TokStr == ",") {
							if (BrLevel == 1) {
								ArgCount++;
							}
						}
						if (TokStr == "(") {
							BrLevel++;
						}
						if (TokStr == ")") {
							BrLevel--;
							if (BrLevel == 0) {
								ArgCount++;
								break;
							}
						}
						s += Trim(Token->OrgStr);
					}
					if (MainF->Call_ATExec_Script != NULL) {
						// s += MainF->Call_ATExec_Script->GetATExecArgStr(false,
						// "", "", ArgCount - 2);
						string argStr;

						argStr = MainF->Call_ATExec_Script->GetATExecArgStr
						(false, "", "", ArgCount - 2);
						argStr = MainF->Call_ATExec_Script->ReplaseBindValName
						(argStr);
						s += argStr;
					}
					s += ")";
					s = SepLongStr(s);
					fprintf(fpOut, "%s\n", s.c_str());
				}
				else if (Script->ScType == sct_command) {
					//
					// FORTRAN 90 の式（コマンド）そのまま出力される。
					//
					// ここで、OAT_xxx = の値の所得も行う。
					// 現状では、定数のみが使用可能。（変数や式はエラー）
					//
					s = Script->TokStrList->Strings[0];
					for (int j = 0; j < Token->Indent; j++) {
						s = "  " + s;
					}
					s = SepLongStr(s);
					fprintf(fpOut, "%s\n", s.c_str());
					// }else if(Script->ScType == sct_None){
					// }else if(Script->ScType == sct_BPset){
				}
				else {
					// 展開しないスクリプトはそのまま出力
					if (MainF->SrcCodeType == MainF->sctFortran77) {
						s = "c";
					}
					else {
						s = "!";
					}
					for (; i < EndTokenPos; i++) {
						Token = (TToken*)TokenList->Items[i];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					s = SepLongStr(s);
					fprintf(fpOut, "%s", s.c_str());
				}
				//
				// スクリプトの最後までＳＫＩＰ
				//
				for (; i < EndTokenPos; i++) {
					Token = (TToken*)TokenList->Items[i];
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
			}
			else if ((Token->TokId == tid_CALL) && (MainF->NoMPIF)) {
				//
				// MPI未使用の場合は、 Callの次が　MPIxxx　であれば
				// tid_LineEndまでコメントにする。 2009/03/10
				//
				int cc = 0;
				bool NewLineF = true;
				string FuncName, s, s1, s2;

				if (i < -1) {
					FuncName = LowerCase
					(Trim(((TToken*)TokenList->Items[i + 1])->OrgStr));
					cc = FuncName.find("mpi");
				}
				if (cc == 1) {
					for (; i < EndTokenPos; i++) {
						Token = (TToken*)TokenList->Items[i];
						if (NewLineF) {
							NewLineF = false;
							if (MainF->SrcCodeType == MainF->sctFortran77) {
								s = "c" + Token->OrgStr;
							}
							else {
								s = "!" + Token->OrgStr;
							}
						}
						else {
							s = Token->OrgStr;
						}
						cc = s.find("\n");
						if (cc < 0) {
							fprintf(fpOut, "%s", SepLongStr(s).c_str());
						}
						else {
							s1 = s.substr(0, cc);
							fprintf(fpOut, "%s", SepLongStr(s1).c_str());
							s2 = s.substr(cc, s.length());
							if (s2 != "") {
								fprintf(fpOut, "c%s", SepLongStr(s2).c_str());
							}
						}
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
				}
				else {
					s = SepLongStr(Token->OrgStr);
					fprintf(fpOut, "%s", s.c_str());
				}
			}
			else {
				LineStr += Token->OrgStr;
			}
			//
			// USE文の挿入位置の場合はUSE文を出力する。
			//
			if ((InsertUsePosList->Count > 0) && (i == (long)
					InsertUsePosList->Items[0])) {
				if (LineStr != "") {
					s = SepLongStr(LineStr);
					fprintf(fpOut, "%s", s.c_str());
				}
				LineStr = "";
				if (MainF->SrcCodeType == MainF->sctFortran90) {
					/** ******************************************************************************* */
					//
					// Kogakuin Irie
					// use omp_lib を挿入する
					// 既に宣言している場合は何もしない
					//
					for (int kk = 0; kk < TuneRegionList->Count; kk++) {
						TuneRegion = (TTuneRegion*)TuneRegionList->Items[kk];

						if (TuneRegion->TuneGroup == tgDynamic &&
							TuneRegion->FittingDspline != 0) {
							if (s.find("omp_lib", 0) == string::npos) {
								fprintf(fpOut, "      use omp_lib\n");
							}
						}
					}
					//
					// ここまで
					//
					/** ******************************************************************************* */
					fprintf(fpOut, "      use ppohAT_ControlRoutines\n");
					fprintf(fpOut, "      use ppohAT_InstallRoutines\n");
					fprintf(fpOut, "      use ppohAT_StaticRoutines\n");
					fprintf(fpOut, "      use ppohAT_DynamicRoutines\n");
				}
				InsertUsePosList->Delete(0);
			}
			//
			// OAT.H のインクルードの後に変数定義を追加。
			// integer iusw1_チューニング名　定義がないと implicit none の場合にエラーになる。
			// use文使用に変えたために不要になった。2013/07/30
			// 一時変数ctmpの定義を追加 2013/08/12
			//
#if 1
			if ((MainF->SrcCodeType == MainF->sctFortran90) &&
				(Token->TokId == tid_INCLUDE) && (!IncludeOATF)) {
				if (i + 1 < EndTokenPos) {
					Token2 = (TToken*)TokenList->Items[i + 1];
					if ((Token2->TokId == tid_ConstStr) &&
						(LowerCase(Token2->Str) == "oat.h")) {
						//
						// 行の最後まで出力。
						//
						i++;
						for (; i < EndTokenPos; i++) {
							Token = (TToken*)TokenList->Items[i];
							LineStr += Token->OrgStr;
							if (Token->TokId == tid_LineEnd) {
								break;
							}
						}
						if (LineStr != "") {
							s = SepLongStr(LineStr);
							fprintf(fpOut, "%s", s.c_str());
						}
						LineStr = "";
						/*
						for(int kk = 0 ; kk < TuneRegionList->Count ; kk++){
						TuneRegion = (TTuneRegion *)TuneRegionList->Items[kk];
						s = "      integer iusw1_"+TuneRegion->Name;
						fprintf(fpOut,"%s\n",s.c_str());
						}
						 */
						// fprintf(fpOut, "      character*100 ctmp\n");
						fprintf(fpOut, "      character*%d ctmp\n",
							MainF->CharMaxLen);
						/** *************************************************************************************************************************** */
						//
						// Kogakuin Irie
						// Fortran90用のd-spline用追加成分
						// 取得した時間を格納する変数を宣言
						// この変数は，d-Splineにより推定された値を用いた処理の時間を格納する
						// そのため，d-Splineがある手続き内（チューニング領域内）で宣言しなければならない
						//
						for (int kk = 0; kk < TuneRegionList->Count; kk++) {
							TuneRegion = (TTuneRegion*)
							TuneRegionList->Items[kk];

							if (TuneRegion->TuneGroup == tgDynamic) {
								int j;
								TToken *Token3;

								// 現在位置から最後までのトークンを取得していく
								for (j = i + 1; j < EndTokenPos; j++) {
									Token3 = (TToken*)TokenList->Items[j];

									if
									(Token3->Str == "end" ||
										Token3->Str == "region") {
										// 手続き終了より前に region を見つけたら，変数宣言
										if (Token3->Str == "region") {
										fprintf(fpOut,
										"      double precision t1, t2\n");
										kk = TuneRegionList->Count;
										break;
										}
										// チューニング領域を見つけることなく手続きが終了したら，宣言位置としてふさわしくないため終了
										else {
										Token3 = (TToken*)TokenList->Items[++j];

										if
										(Token3->Str == "main" ||
										Token3->Str == "subroutine" ||
										Token3->Str == "function") {
										kk = TuneRegionList->Count;
										break;
										}
										}
									}
								}
							}
						}
						//
						// ここまで
						//
						/** *************************************************************************************************************************** */
						fprintf(fpOut, "\n");
					}
				}

			}
#endif
			if (InsertIncludePosList->Count != 0) { // Include配置位置に配置
				if (i >= (long)InsertIncludePosList->Items[0]) {
					if (IncludeInContainsInModouleF) {
						MainF->print
						("Output \"include OAT_Routins\" to " +
							OutputFileName +
							" (contains in module).");
					}
					else if (MainF->IncludeCodeInContainsF) {
						MainF->print
						("Output \"include OAT_Routins\" to " +
							OutputFileName + " (contains).");
					}
					else {
						MainF->print
						("Output \"include OAT_Routins\" to " +
							OutputFileName + ".");
					}
					fprintf(fpOut, "\n");
					if ((InsertIncludePosList->Count >= 2) &&
						(InsertIncludePosList->Items[0]
							== InsertIncludePosList->Items
							[1])) {
						// 同じデータが２つ連続する場合は、CONTAINS文を入れる。
						InsertIncludePosList->Delete(0);
						fprintf(fpOut, "      contains\n");
					}
					InsertIncludePosList->Delete(0);
					/*
					use を使う形になったため includeはカット。 2013/07/30
					if(MainF->SrcCodeType == MainF->sctFortran77){
					fprintf(fpOut,"      include 'OAT_ControlRoutines.f'\n");
					fprintf(fpOut,"      include 'OAT_DynamicRoutines.f'\n");
					fprintf(fpOut,"      include 'OAT_InstallRoutines.f'\n");
					fprintf(fpOut,"      include 'OAT_StaticRoutines.f'\n");
					}else{
					fprintf(fpOut,"      include 'OAT_ControlRoutines.f90'\n");
					fprintf(fpOut,"      include 'OAT_DynamicRoutines.f90'\n");
					fprintf(fpOut,"      include 'OAT_InstallRoutines.f90'\n");
					fprintf(fpOut,"      include 'OAT_StaticRoutines.f90'\n");
					}
					fprintf(fpOut,"\n");
					 */
				}
			}

		}
		if (fpAdd != NULL) { // 一部をInstallRoutinesに複写する場合
			if (LineStr != "") {
				s = SepLongStr(LineStr);
				fprintf(fpOut, "%s", s.c_str());
			}
			LineStr = "";
			break;
		}
	}
	delete InsertIncludePosList;
	delete InsertUsePosList;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// OAT_(元のソースファイル名).f を作成する。
//
// Region（一番外側）を SetParam() call Sub() に置き換える。
// また、NestしたRegion内は、コメントとして、 TokenEndPosまでSkipする。
// NestRegion内の sct_Commandも、Nest先のRegionでの処理となる。
// Fortran77用
//
// ・sct_command の変換
// ・sct_install ... のブロックをコメントアウト（コメントを残す）
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::MakeSrcCode_Fortran77() {
	int i;
	TToken *Token;
	TScript *Script;
	string s;
	string LineStr;
	TTuneRegion *TuneRegion;
	bool IncludeReqF = true;
	int LastLineNo = -1;
	FILE *fpOut;
	int SrcFileIdx;

	// for (SrcFileIdx = 0; SrcFileIdx < MainF->SrcFileNameList.Count;	SrcFileIdx++) {
	//
	// Fortran77（固定形式）の場合は１つのファイルとして出力する。
	// ファイルは作成するが内容は空になる。
	//
	for (SrcFileIdx = 0;
		(SrcFileIdx < 1) && (SrcFileIdx < MainF->SrcFileNameList.Count);
		SrcFileIdx++) {
		fpOut = (FILE*)fpOutList.Items[SrcFileIdx];
		LineStr = "";
		for (i = 0; i < TokenList->Count; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (Token->LineNo != LastLineNo) {
				LastLineNo = Token->LineNo;
				if (LineStr != "") {
					s = SepLongStr(LineStr);
					fprintf(fpOut, "%s", s.c_str());
				}
				LineStr = "";
			}
			if (Token->Script != NULL) {
				Script = (TScript*)Token->Script;
				if (Script->ScRegion == scr_start) {
					// Codeの置き換え
					TuneRegion = (TTuneRegion*)Script->TuneRegion;
					if (TuneRegion->TokenEndPos > i) {
						if (!TuneRegion->UsedDynPefThis) {
							TuneRegion->OutputSrcCode(fpOut, "", 0);
							// 置き換えコードを出力
						}
						else {
							TuneRegion->OutputSrcCode(fpOut, "", 3);
							// 置き換えコードを出力
						}
						i = TuneRegion->TokenEndPos; // Top Regionの中をスキップする。
						continue;
					}
				}
				else if (Script->ScType == sct_DynPefThis) {
					//
					// Dynamicの実行 SetParam() を指定
					//
					TScValData *ScValData = NULL;
					int kk;

					if (Script->ScValDataList->Count > 0) {
						ScValData = (TScValData*)Script->ScValDataList->Items
						[0];
						s = ScValData->Str; // ＴＲ名 ""付き
						while (s.find("\"") != string::npos) {
							s.erase(s.find("\""), 1);
						};
						while (s.find("\'") != string::npos) {
							s.erase(s.find("\'"), 1);
						};
						for (kk = 0; kk < TuneRegionList->Count; kk++) {
							TuneRegion = (TTuneRegion*)
							TuneRegionList->Items[kk];
							if (LowerCase(s) == LowerCase(TuneRegion->Name)) {
								// if(CompareText(s,TuneRegion->Name) == 0){
								TuneRegion->UsedDynPefThis = true;
								TuneRegion->OutputSrcCode(fpOut, "", 2);
								// 置き換えコードを出力
							}
						}
					}
				}
				else if (Script->ScType == sct_ATExec) { // ATExec()
					/** ***************************************************** */
					//
					// Kogakuin Irie
					// Fortran90と同じように対応させる（引数の自動追加）
					// 追加コードはFortran90の引用
					// 既存コードはコメントアウト
					//
					// s = "      call ";
					// for(int j = 1 ; j < Script->TokStrList->Count ; j++){
					// Token = (TToken *)Script->TokStrList->Objects[j];
					// s += Trim(Token->OrgStr);
					// }
					// s = SepLongStr(s);
					// fprintf(fpOut,"%s\n",s.c_str());
					// Script ATExec()部分は、引数追加に置き換える。
					// 引数を自動で追加。先頭２つの引数の後は、ArfStrの引数を追加する。
					string TokStr;
					int ArgCount = 0;
					int BrLevel = 0;

					s = "      call ";
					for (int j = 1; j < Script->TokStrList->Count; j++) {
						Token = (TToken*)Script->TokStrList->Objects[j];
						if (Token == NULL) {
							continue;
						}
						TokStr = Token->Str;
						if (TokStr == ",") {
							if (BrLevel == 1) {
								ArgCount++;
							}
						}
						if (TokStr == "(") {
							BrLevel++;
						}
						if (TokStr == ")") {
							BrLevel--;
							if (BrLevel == 0) {
								ArgCount++;
								break;
							}
						}
						s += Trim(Token->OrgStr);
					}
					if (MainF->Call_ATExec_Script != NULL) {
						string argStr;

						// s += MainF->Call_ATExec_Script->GetATExecArgStr(false,"", "", ArgCount - 2);
						argStr = MainF->Call_ATExec_Script->GetATExecArgStr
						(false, "", "", ArgCount - 2);
						argStr = MainF->Call_ATExec_Script->ReplaseBindValName
						(argStr);
						s += argStr;
					}
					else {
						string argStr;

						TScript *Script = new TScript(MainF->TokenList, 0,
							NULL, ValDataList);
						// s += Script->GetATExecArgStr(false, "", "",	ArgCount - 2);
						argStr = Script->GetATExecArgStr(false, "", "",
							ArgCount - 2);
						argStr = MainF->Call_ATExec_Script->ReplaseBindValName
						(argStr);
						s += argStr;
						delete Script;
					}
					s += ")";
					s = SepLongStr(s);
					fprintf(fpOut, "%s\n", s.c_str());
					//
					// ここまで
					//
					/** ***************************************************** */
				}
				else if (Script->ScType == sct_None) {

				}
				else if (Script->ScType == sct_command) {
					//
					// FORTRAN 90 の式（コマンド）そのまま出力される。
					//
					// ここで、OAT_xxx = の値の所得も行う。
					// 現状では、定数のみが使用可能。（変数や式はエラー）
					//
					s = Script->TokStrList->Strings[0];
					for (int j = 0; j < Token->Indent; j++) {
						s = "  " + s;
					}
					s = SepLongStr(s);
					fprintf(fpOut, "%s\n", s.c_str());
				}
				else {
					// 展開しないスクリプトはそのまま出力
					s = "c";
					for (; i < TokenList->Count; i++) {
						Token = (TToken*)TokenList->Items[i];
						s += Token->OrgStr;
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
					s = SepLongStr(s);
					fprintf(fpOut, "%s", s.c_str());
					/*
					s = SepLongStr(Token->OrgStr);
					fprintf(fpOut,"%s",s.c_str());
					continue;
					 */
				}
				//
				// スクリプトの最後までＳＫＩＰ
				//
				for (; i < TokenList->Count; i++) {
					Token = (TToken*)TokenList->Items[i];
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
			}
			else if ((Token->TokId == tid_CALL) && (MainF->NoMPIF)) {
				//
				// MPI未使用の場合は、 Callの次が　MPIxxx　であれば
				// tid_LineEndまでコメントにする。 2009/03/10
				//
				int cc = 0;
				bool NewLineF = true;
				string FuncName, s, s1, s2;

				if (i < TokenList->Count - 1) {
					FuncName = LowerCase
					(Trim(((TToken*)TokenList->Items[i + 1])->OrgStr));
					cc = FuncName.find("mpi");
				}
				if (cc == 1) {
					for (; i < TokenList->Count; i++) {
						Token = (TToken*)TokenList->Items[i];
						if (NewLineF) {
							NewLineF = false;
							s = "c" + Token->OrgStr;
						}
						else {
							s = Token->OrgStr;
						}
						cc = s.find("\n");
						if (cc < 0) {
							fprintf(fpOut, "%s", SepLongStr(s).c_str());
						}
						else {
							s1 = s.substr(0, cc);
							fprintf(fpOut, "%s", SepLongStr(s1).c_str());
							s2 = s.substr(cc, s.length());
							if (s2 != "") {
								fprintf(fpOut, "c%s", SepLongStr(s2).c_str());
							}
						}
						if (Token->TokId == tid_LineEnd) {
							break;
						}
					}
				}
				else {
					s = SepLongStr(Token->OrgStr);
					fprintf(fpOut, "%s", s.c_str());
				}
			}
			else {
				LineStr += Token->OrgStr;
			}
			if (Token->TokId == tid_END) { // 最初のEndの後でInclude （元と同じ）
				//
				// 改行コードまで含めて出力　2016/03/02
				//
				for (i = i + 1; i < TokenList->Count; i++) {
					Token = (TToken*)TokenList->Items[i];
					LineStr += Token->OrgStr;
					if (Token->TokId == tid_LineEnd) {
						break;
					}
				}
				if (LineStr != "") {
					s = SepLongStr(LineStr);
					fprintf(fpOut, "%s", s.c_str());
				}
				LineStr = "";
				if (IncludeReqF) {
					IncludeReqF = false;
					fprintf(fpOut, "\n");
					fprintf(fpOut, "\n");
					fprintf(fpOut, "      include 'OAT_ControlRoutines.f'\n");
					fprintf(fpOut, "      include 'OAT_DynamicRoutines.f'\n");
					fprintf(fpOut, "      include 'OAT_InstallRoutines.f'\n");
					fprintf(fpOut, "      include 'OAT_StaticRoutines.f'\n");
				}
			}
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// InsertIncludePosList 求めた配置トークン位置を返すリスト
//
//
// 3.概要
// Includeを配置する位置を求めて返す。
//
// 4.機能説明
//
// リージョン内の変数で未定義がある場合は、containsやuse等を使っている可能性がある。
// 例えば、 use module での変数を使用している場合は、そこの呼び出し部分に組み込む形で
// OATのルーチンをincludeする必要がある。例えば call OAT_Install_xx(Arg1,Arg2)があった場合に
// Arg1の型がモジュールで定義されて不明の場合には、useが有効な部分の最後に
// contains の形での includeによって use へ参照可能な形での includeが必要。
//
// 未定義の変数がない場合は、OATのルーチンは従来と同じで、外部サブルーチンの記述で問題ない。
// 未定義の変数定義を use等が有効になるようにリージョンの後にContainsで追加する。
//
// Containsでは、複数の参照があった場合は、複数のincludeが必要になるが、
// Module 宣言内に includeする場合には、他のプログラムは useでそのモジュールを参照可能なので
// １か所のみにincludeする。他のプログラムへのuseは手動で追加とする（すでにuseしてある場合も多い）
//
// 5.戻り値
// 配置種類：0:通常の配置。1 モジュール内に１つのみ配置。
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
int TPass5::AddIncludePosList(TList *InsertIncludePosList) {
	int i, j;
	TTuneRegion *TuneRegion;
	int UndefindeArgCount;
	int TokenPos;
	int ModuleNest;
	int NestLevel;
	TToken *Token;
	TToken *Token2;
	TScript *Script;
	bool RegionStartInModuleF;
	bool AddIncludeReqF;
	TValData *ValData;
	bool ContainsF;

	UndefindeArgCount = 0;
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		UndefindeArgCount += TuneRegion->UndefinedArgValList->Count;
		//
		// 配列の仮引数で、要素数の指定がない場合 (:)での定義もContainsを使用する。
		// (INTERFACEの定義に関しては、現時点では考慮していない。)
		//
		for (j = 0; j < TuneRegion->ArgValList->Count; j++) {
			ValData = (TValData*)TuneRegion->ArgValList->Objects[j];
			if (ValData == NULL) {
				continue;
			}
			if (ValData->ArrayOrFuncF && (ValData->ArrayLevel >= 1)) {
				if ((ValData->ArrayDefPosS - 1 < 0) ||
					(ValData->ArrayDefPosE + 1 >= TokenList->Count)) {
					continue;
				}
				for (int k = ValData->ArrayDefPosS; k < ValData->ArrayDefPosE;
					k++) {
					Token = (TToken*)TokenList->Items[k];
					if (Token->TokId == tid_Koron) {
						// :の前が ( か , の場合、;の後が )か,の場合は要素数不明
						Token2 = (TToken*)TokenList->Items[k - 1];
						if ((Token2->TokId == tid_Kakko) ||
							(Token2->TokId == tid_Kannma)) {
							UndefindeArgCount++; // 未定義変数扱いとする・
							break;
						}
						Token2 = (TToken*)TokenList->Items[k + 1];
						if ((Token2->TokId == tid_Kakko) ||
							(Token2->TokId == tid_Kannma)) {
							UndefindeArgCount++; // 未定義変数扱いとする・
							break;
						}
					}
				}
			}
		}
	}
	if (UndefindeArgCount == 0) {
		//
		// 未定義の変数がなければ、最後に別関数として追加する。
		//
		return 0;
	}
	//
	// 未定義の変数があった場合は、containsで組み込む。
	// リージョンがModule内にあるかを調べる。もしモジュール内にあれば、
	// そのモジュール内にのみ組み込み、それ以外は use module で参照する形とする。
	//
	ModuleNest = -1;
	NestLevel = 0;
	RegionStartInModuleF = false;
	for (i = 0; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		switch(Token->TokId) {
		case tid_MODULE: // MODULE
			ModuleNest = NestLevel;
			NestLevel++;
			break;
		case tid_PROGRAM: // PROGRAM
		case tid_INTERFACE: // INTERFACE
		case tid_SUBROUTINE: // SUBROUTINE
		case tid_FUNCTION: // FUNCTION
		case tid_TYPE:
			NestLevel++;
			break;
		case tid_END:
			NestLevel--;
			if (ModuleNest == NestLevel) {
				ModuleNest = -1;
			}
			for (; i < TokenList->Count; i++) {
				Token2 = (TToken*)TokenList->Items[i];
				if (Token2->TokId == tid_LineEnd) {
					break; // LineEndまでスキップ END TYPE , END FUNCTION 等の対応
				}
			}
			break;
		case tid_CONTAINS: // CONTAINS
			break;
		default:
			break;
		}
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if (Script->ScRegion == scr_start) { // リージョン開始
				if (ModuleNest != -1) {
					// モジュール内にリージョン開始があった。そのモジュールの最後にのみ追加する。
					RegionStartInModuleF = true;
				}
			}
		}
	}
	//
	// 未定義の変数を持ち、Module内でのリージョン開始でない場合。
	// この場合は、各出現単位ごとにContainsで追加する。
	// Containsを含む場合は、同じ位置を２回セットする。
	//
	ModuleNest = -1;
	NestLevel = 0;
	AddIncludeReqF = false;
	ContainsF = false;
	for (i = 0; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		switch(Token->TokId) {
		case tid_MODULE: // MODULE
			ModuleNest = NestLevel;
			NestLevel++;
			break;
		case tid_PROGRAM: // PROGRAM
		case tid_INTERFACE: // INTERFACE
		case tid_SUBROUTINE: // SUBROUTINE
		case tid_FUNCTION: // FUNCTION
		case tid_TYPE:
			NestLevel++;
			break;
		case tid_END:
			NestLevel--;
			if (NestLevel == 0) {
				if (AddIncludeReqF) {
					AddIncludeReqF = false;
					if (RegionStartInModuleF && (ModuleNest == -1)) {
						// モジュール内にセットの場合、モジュール出現まで待つ
					}
					else {
						TokenPos = i;
						InsertIncludePosList->Add((void*)(long)TokenPos);
						if (!ContainsF) {
							// Containsがなければ追加する。
							InsertIncludePosList->Add((void*)(long)TokenPos);
						}
						if (RegionStartInModuleF) { // モジュール内であれば、１つで終了。
							return 1;
						}
					}
				}
				ContainsF = false;
			}
			for (; i < TokenList->Count; i++) {
				Token2 = (TToken*)TokenList->Items[i];
				if (Token2->TokId == tid_LineEnd) {
					break; // LineEndまでスキップ END TYPE , END FUNCTION 等の対応
				}
			}
			if (ModuleNest == NestLevel) {
				ModuleNest = -1;
			}
			break;
		case tid_CONTAINS: // CONTAINS
			ContainsF = true;
			break;
		default:
			break;
		}
		if (Token->Script != NULL) {
			Script = (TScript*)Token->Script;
			if (Script->ScRegion == scr_start) { // リージョン開始
				AddIncludeReqF = true;
			}
			else if (Script->ScType == sct_ATExec) { // ATExec()
				AddIncludeReqF = true;
			}
		}
	}
	if (AddIncludeReqF) {
		AddIncludeReqF = false;
		TokenPos = TokenList->Count - 1;
		InsertIncludePosList->Add((void*)(long)TokenPos);
	}
	return 0;
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// InsertUsePosList 求めた配置トークン位置を返すリスト
//
// 3.概要
// USE 文配置する位置を求めて返す。
//
// 4.機能説明
//
// モジュール内に!OAT$を含んでいる場合には、USE文を挿入する位置をInsertUsePosListに追加する。
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::AddUsePosList(TList *InsertUsePosList) {
	int i, j;
	int ModuleNest;
	int ModuleIdx = -1;
	int NestLevel;
	TToken *Token;
	TToken *Token2;
	bool ModuleDefF;
	int LastMoudleDefPos;
	int BrLevel;
	bool ContainsF = false;

	//
	// 未定義の変数があった場合は、containsで組み込む。
	// リージョンがModule内にあるかを調べる。もしモジュール内にあれば、
	// そのモジュール内にのみ組み込み、それ以外は use module で参照する形とする。
	//
	ModuleNest = -1;
	NestLevel = 0;
	ModuleDefF = false;
	LastMoudleDefPos = -1;
	for (i = 0; i < TokenList->Count; i++) {
		Token = (TToken*)TokenList->Items[i];
		if (Token->ModuleIdx > ModuleIdx) {
			ModuleDefF = true;
			ModuleIdx = Token->ModuleIdx;
		}
		switch(Token->TokId) {
		case tid_MODULE: // MODULE
			ModuleNest = NestLevel;
			NestLevel++;
			//
			// モジュールの最初の定義行の次の位置を探す。
			//
			if (ModuleDefF) {
				ModuleDefF = false;
				if (true) {
					BrLevel = -1;
					for (j = i; j < TokenList->Count; j++) {
						Token2 = (TToken*)TokenList->Items[j];
						if (Token2->TokId == tid_Kakko) {
							if (BrLevel < 0) {
								BrLevel = 0;
							}
							BrLevel++;
						}
						if (Token2->TokId == tid_Kokka) {
							BrLevel--;
						}
						// if((Token2->TokId == tid_LineEnd)&&(BrLevel == 0)){
						// Program mainに対応。2016/03/04
						if ((Token2->TokId == tid_LineEnd) && (BrLevel <= 0)) {
							LastMoudleDefPos = j;
							break;
						}
						if ((Token2->TokId == tid_LineEnd) && (BrLevel < 0)) {
							BrLevel = 0;
						}
					}
				}
			}
			break;
		case tid_PROGRAM: // PROGRAM
		case tid_INTERFACE: // INTERFACE
		case tid_SUBROUTINE: // SUBROUTINE
		case tid_FUNCTION: // FUNCTION
		case tid_TYPE:
			NestLevel++;
			if (ContainsF) {
				break;
			}
			//
			// モジュールの最初の定義行の次の位置を探す。
			//
			if (ModuleDefF) {
				ModuleDefF = false;
				if (true) {
					BrLevel = -1;
					for (j = i; j < TokenList->Count; j++) {
						Token2 = (TToken*)TokenList->Items[j];
						if (Token2->TokId == tid_Kakko) {
							if (BrLevel < 0) {
								BrLevel = 0;
							}
							BrLevel++;
						}
						if (Token2->TokId == tid_Kokka) {
							BrLevel--;
						}
						// if((Token2->TokId == tid_LineEnd)&&(BrLevel == 0)){
						// Program mainに対応。2016/03/04
						if ((Token2->TokId == tid_LineEnd) && (BrLevel <= 0)) {
							LastMoudleDefPos = j;
							break;
						}
						if ((Token2->TokId == tid_LineEnd) && (BrLevel < 0)) {
							BrLevel = 0;
						}
					}
				}
			}
			break;
		case tid_END:
			NestLevel--;
			if (ModuleNest == NestLevel) {
				ModuleNest = -1;
				ContainsF = false;
			}
			for (; i < TokenList->Count; i++) {
				Token2 = (TToken*)TokenList->Items[i];
				if (Token2->TokId == tid_LineEnd) {
					break; // LineEndまでスキップ END TYPE , END FUNCTION 等の対応
				}
			}
			break;
		case tid_CONTAINS: // CONTAINS
			ContainsF = true;
			break;
		default:
			break;
		}
		if (Token->Script != NULL) { // スクリプトあり。
			if (LastMoudleDefPos != -1) {
				// USE文挿入位置を追加する。
				InsertUsePosList->Add((void*)(long)LastMoudleDefPos);
				LastMoudleDefPos = -1;
			}
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// モジュール内にリージョンを含んでいる場合に、リージョン内以外で使用されていない
// 変数定義を削除する
//
// 4.機能説明
//
// 変数のみ削除する場合は変数トークンを空白にする。
// 変数定義行を全てコメントにする場合は先頭から改行位置までをコメントにする
//
//
// 5.戻り値
// なし
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::CheckAndDeleteLocalValDefine() {
	int i, j, k;
	TTuneRegion *TuneRegion;
	TList *ModuleList;
	string s, s1, s2;
	int TokPos, TokPos2, ModuleIdx;
	TToken *Token;
	TToken *Token2;
	int ModuleSPos, ModuleEPos;

	//
	// リージョンの属するモジュールの先頭と最後を探す。
	// 開始位置から先頭かモジュールが変化するまで検索する。
	//
	ModuleList = new TList;
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		TokPos = TuneRegion->TokenStartPos;
		Token = (TToken*)TokenList->Items[TokPos];
		ModuleIdx = Token->ModuleIdx;
		for (; TokPos >= 0; TokPos--) {
			Token = (TToken*)TokenList->Items[TokPos];
			if (ModuleIdx != Token->ModuleIdx) {
				break;
			}
		}
		ModuleSPos = TokPos + 1;
		TokPos = TuneRegion->TokenEndPos;
		for (; TokPos < TokenList->Count; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			if (ModuleIdx != Token->ModuleIdx) {
				break;
			}
		}
		ModuleEPos = TokPos;
		ModuleList->Add((void*)(long)ModuleSPos);
		ModuleList->Add((void*)(long)ModuleEPos);
	}
	//
	// モジュール内でのリージョン以外での変数の使用状態をチェックする。
	//
	//
	for (i = 0; i < ModuleList->Count; i += 2) {
		//
		// 変数の定義、参照数をカウントする。
		//
		TValData *ValData;
		TValData *ValData2;
		TList *UseValDataList;
		TList *UseValDataCountList;
		TList *DelValDefList;
		int Idx;
		int UseCount;

		UseValDataList = new TList;
		UseValDataCountList = new TList;
		DelValDefList = new TList;

		ModuleSPos = (long)ModuleList->Items[i];
		ModuleEPos = (long)ModuleList->Items[i + 1];
		for (TokPos = ModuleSPos; TokPos < ModuleEPos; TokPos++) {
			Token = (TToken*)TokenList->Items[TokPos];
			if (Token->ValData != NULL) {
				ValData = (TValData*)Token->ValData;
				Idx = UseValDataList->IndexOf(ValData);
				if (Idx == -1) {
					UseValDataList->Add(ValData);
					UseValDataCountList->Add((void*)1);
				}
				else {
					UseCount = (long)UseValDataCountList->Items[Idx];
					UseCount++;
					UseValDataCountList->Items[Idx] = (void*)(long)UseCount;
				}
			}
		}
		//
		// モジュール内のリージョンに関してカウント数から削除する。
		//
		for (j = 0; j < TuneRegionList->Count; j++) {
			TuneRegion = (TTuneRegion*)TuneRegionList->Items[j];
			if (TuneRegion->TokenStartPos < ModuleSPos) {
				continue;
			}
			if (TuneRegion->TokenEndPos > ModuleEPos) {
				continue;
			}
			for (TokPos = TuneRegion->TokenStartPos;
				TokPos < TuneRegion->TokenEndPos; TokPos++) {
				Token = (TToken*)TokenList->Items[TokPos];
				if (Token->ValData != NULL) {
					ValData = (TValData*)Token->ValData;
					Idx = UseValDataList->IndexOf(ValData);
					if (Idx != -1) {
						UseCount = (long)UseValDataCountList->Items[Idx];
						UseCount--;
						UseValDataCountList->Items[Idx] = (void*)(long)UseCount;
					}
				}
			}
		}
		//
		// 定義のみで参照がない変数（リージョン内の参照は除く）を削除する。
		// 引数(ArgF)の変数についてはそのままとする。
		// トークン自体は残す。
		//
		for (j = 0; j < MainF->ValDataList->Count; j++) {
			ValData = (TValData*)MainF->ValDataList->Items[j];
			if (ValData->DefPos < ModuleSPos) {
				continue;
			}
			if (ValData->DefPos > ModuleEPos) {
				continue;
			}
			if (ValData->ArgF) {
				continue;
			}
			Idx = UseValDataList->IndexOf(ValData);
			if (Idx != -1) {
				UseCount = (long)UseValDataCountList->Items[Idx];
			}
			else {
				UseCount = 0;
			}
			if (UseCount != 0) {
				continue;
			}
			DelValDefList->Add(ValData);
		}
		//
		// 使用されない変数を定義から削除する。
		// 変数部分のみ削除の場合と定義文をコメントアウトの２種類がある。
		//
		// 定義行に含まれる変数が全て削除対象の場合はコメントアウトとする。
		//
		for (j = 0; j < DelValDefList->Count; j++) {
			ValData = (TValData*)DelValDefList->Items[j];
			// printf("delete variable define %s\n", ValData->Str.c_str());
			//
			// 変数定義行が他の削除対象以外の変数を含んでいるかをチェックする。
			//
			for (k = 0; k < MainF->ValDataList->Count; k++) {
				ValData2 = (TValData*)MainF->ValDataList->Items[k];
				if (ValData2 == ValData) {
					continue;
				}
				if (ValData->DefPosS != ValData2->DefPosS) {
					continue; // 定義行が異なる変数
				}
				if (DelValDefList->IndexOf(ValData2) != -1) {
					continue; // 削除対象変数。
				}
				break;
			}
			if (k >= MainF->ValDataList->Count) {
				//
				// 定義文自体をコメントにする。
				//
				TokPos = ValData->DefPosS;
				Token = (TToken*)TokenList->Items[TokPos];
				if (Token->TokId != tid_Comment) {
					Token->TokId = tid_Comment;
					Token->Str = "!" + Token->Str;
					/** ****************************************************************** */
					//
					// Kogakuin Irie
					// 未使用でない変数もコメントアウトされる場合もあるため，コメントアウト
					//
					// Token->OrgStr = "!" + Token->OrgStr;
					//
					// ここまで
					//
					/** ****************************************************************** */
				}
			}
			else {
				//
				// 他の変数の定義があるので、変数部分（,を含む)のみ空白にする。
				//
				TokPos = ValData->DefPos;
				Token = (TToken*)TokenList->Items[TokPos];
				Token->TokId = tid_Comment;
				///*********************************************************************/
				//
				// Kogakuin Irie
				// 未使用でない変数もコメントアウトされる場合もあるため，コメントアウト
				//
				// Token->Str = "";
				// Token->OrgStr = "";
				//
				// ここまで
				//
				/** ****************************************************************** */
				//
				// 後ろに カンマがあれば、そこまで空白にする。
				//
				for (TokPos2 = TokPos; TokPos2 < TokenList->Count; TokPos2++) {
					Token2 = (TToken*)TokenList->Items[TokPos2];
					if (Token2->TokId == tid_LineEnd) {
						TokPos2 = -1;
						break;
					}
					if (Token2->TokId == tid_Set) {
						TokPos2 = -1;
						break;
					}
					if (Token2->TokId == tid_Kannma) {
						for (k = TokPos + 1; k <= TokPos2; k++) {
							Token = (TToken*)TokenList->Items[k];
							Token->TokId = tid_Comment;
							///*********************************************************************/
							//
							// Kogakuin Irie
							// 未使用でない変数もコメントアウトされる場合もあるため，コメントアウト
							//
							// Token->Str = "";
							// Token->OrgStr = "";
							//
							// ここまで
							//
							/** ****************************************************************** */
						}
						break;
					}
				}
				if (TokPos2 == -1) { // 後ろにカンマが見つからなかった場合。
					//
					// 前に カンマがあれば、そこまで空白にする。
					//
					for (TokPos2 = TokPos; TokPos2 >= 0; TokPos2--) {
						Token2 = (TToken*)TokenList->Items[TokPos2];
						if (Token2->TokId == tid_LineEnd) {
							TokPos2 = -1;
							break;
						}
						if (Token2->TokId == tid_KoronKoron) {
							TokPos2 = -1;
							break;
						}
						if (Token2->TokId == tid_Kannma) {
							for (k = TokPos2; k < TokPos; k++) {
								Token = (TToken*)TokenList->Items[k];
								Token->TokId = tid_Comment;
								///*********************************************************************/
								//
								// Kogakuin Irie
								// 未使用でない変数もコメントアウトされる場合もあるため，コメントアウト
								//
								// Token->Str = "";
								// Token->OrgStr = "";
								//
								// ここまで
								//
								/** ****************************************************************** */
							}
							break;
						}
					}
				}
			}
		}
		delete UseValDataList;
		delete UseValDataCountList;
		delete DelValDefList;

	} // 対象モジュールに対して繰り返す。

	delete ModuleList;

}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
// fp 出力先ファイル
//
// 3.概要
// Install , static , dynamic　のモジュールの先頭部分を出力する。
//
// 4.機能説明
//
// モジュール名とモジュールが参照するuse文を出力する。
// Use文についてはリージョンが参照しているUse文のリストを作成して出力する。
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::OutputModuleStart(FILE *fp) {
	int i, j;
	TTuneRegion *TuneRegion;
	string s, s1, s2;
	TStringList *UseModuleNameList;
	int TokPos, ModuleIdx;
	TToken *Token;

	UseModuleNameList = new TStringList;
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		//
		// リージョンの属するモジュールの Useを探してリストに追加する。
		// 開始位置から先頭かモジュールが変化するまで検索する。
		//
		TokPos = TuneRegion->TokenStartPos;
		Token = (TToken*)TokenList->Items[TokPos];
		ModuleIdx = Token->ModuleIdx;
		for (; TokPos >= 0; TokPos--) {
			Token = (TToken*)TokenList->Items[TokPos];
			if (ModuleIdx != Token->ModuleIdx) {
				break;
			}
			if (Token->TokId == tid_USE) {
				Token = (TToken*)TokenList->Items[TokPos + 1];
				for (j = 0; j < UseModuleNameList->Count; j++) {
					if (LowerCase(UseModuleNameList->Strings[j]) == LowerCase
						(Token->Str)) {
						break;
					}
				}
				if (j >= UseModuleNameList->Count) {
#if 0
					//
					// ここでのuse追加は循環になるのでなしとした。2016/03/13
					// Useに関しては手動での調整も必要。
					//
					UseModuleNameList->Add(Token->Str);
#endif
				}
			}
		}
		//
		// CONTAINSでのネストがあれば、その上位も対象とする。
		// CONTAINSの出現までスキャンして、その先のモジュールのUSEも対象とする。
		// 2013/0/05
		//
		if (Token->NestLevel >= 1) {
			// NowNestLevel = Token->NestLevel;
			for (; TokPos >= 0; TokPos--) {
				Token = (TToken*)TokenList->Items[TokPos];
				if (Token->NestLevel == 0) {
					break;
				}
				if (Token->TokId == tid_CONTAINS) {
					ModuleIdx = Token->ModuleIdx;
					for (; TokPos >= 0; TokPos--) {
						Token = (TToken*)TokenList->Items[TokPos];
						if (ModuleIdx != Token->ModuleIdx) {
							TokPos++;
							break;
						}
						if (Token->NestLevel == 0) {
							TokPos++;
							break;
						}
						if (Token->TokId == tid_USE) {
							Token = (TToken*)TokenList->Items[TokPos + 1];
							for (j = 0; j < UseModuleNameList->Count; j++) {
								if (LowerCase(UseModuleNameList->Strings[j])
									== LowerCase(Token->Str)) {
									break;
								}
							}
							if (j >= UseModuleNameList->Count) {
								UseModuleNameList->Add(Token->Str);
							}
						}
					}
				}
			}
		}
	}
	for (i = 0; i < UseModuleNameList->Count; i++) {
		fprintf(fp, "      use %s\n", UseModuleNameList->Strings[i].c_str());
	}
	fprintf(fp, "\n");
	// implicit noneによってprefetchサンプル等の元にimplicit noneがない場合に
	// がエラーなる場合があるのでCutした 2016/03/06
	// fprintf(fp,"   implicit none\n");
	fprintf(fp, "      public\n");
	fprintf(fp, "\n");
	fprintf(fp, "      contains\n");
	fprintf(fp, "\n");
	delete UseModuleNameList;

}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// Fortranでリージョン内でのCall対応のため、Call_SubroutineNameInRegionListを作成する。
//
// 4.機能説明
//
// TRの中にcallが含まれていれば、そのcall先にTRが含まれていないかチェックして
// 含まれていれば、サブルーチンの複写と呼び先の _OATへの変更を行う。
// 検索結果は、CallInRegionDataとしてCallInRegionDataListに追加される。
// 必要に応じて参照され、コードに複写と_OATへの変更が行われる。
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::Make_Call_SubroutineNameInRegionList() {
	int i, j, k;
	TTuneRegion *TuneRegion;
	TToken *Token;
	string SubroutinName;
	int Idx;
	int tokPos;

	//
	// TRの中にcallが含まれているかチェックする。
	// トークンを先頭から調べて、リージョン内で、callがあるかで調べる。
	//
	for (i = 0; i < TuneRegionList->Count; i++) {
		TuneRegion = (TTuneRegion*)TuneRegionList->Items[i];
		for (j = TuneRegion->TokenStartPos; j < TuneRegion->TokenEndPos; j++) {
			Token = (TToken*)TokenList->Items[j];
			if (Token->TokId != tid_CALL) { // リージョン内のCallが対象
				continue;
			}
			Token = (TToken*)TokenList->Items[j + 1];
			SubroutinName = Token->Str;
			Idx = -1;
			for (k = 0; k < Call_SubroutineNameInRegionList->Count; k++) {
#ifdef _WIN32
				if (stricmp(Call_SubroutineNameInRegionList->Strings[k].c_str(),
#else
				if (strcasecmp(Call_SubroutineNameInRegionList->Strings[k].c_str
						(),
#endif
						SubroutinName.c_str()) == 0) {
					Idx = k;
				}
			}
			if (Idx != -1) {
				continue; // すでに追加済み
			}
			//
			// リージョン内から呼ばれている Subroutine 内に リージョンがあるか調べる。
			// もしあれば、サブルーチン名と開始トークン位置を変換対象としてリストに追加する。
			//
			Idx = -1;
			for (tokPos = 0; tokPos < TokenList->Count; tokPos++) {
				Token = (TToken*)TokenList->Items[tokPos];
				if (Token->TokId == tid_SUBROUTINE) { // サブルーチンを検索
					Token = (TToken*)TokenList->Items[tokPos + 1];
#ifdef _WIN32
					if (stricmp(SubroutinName.c_str(),
							Token->Str.c_str()) == 0) {
#else
					if (strcasecmp(SubroutinName.c_str(),
							Token->Str.c_str()) == 0) {
#endif
						// 一致するサブルーチンが見つかった。
						Idx = tokPos;
						break;
					}
				}
			}
			if (Idx == -1) {
				continue; // サブルーチンが見つからない場合はなにもしない。
			}
			//
			// リージョンを含む、リージョンからcallされたサブルーチン名と位置をリストに追加。
			//
			if (TuneRegion->TuneGroup == tgInstall) {
				Idx += 0 * 1000000;
			}
			else if (TuneRegion->TuneGroup == tgStatic) {
				Idx += 1 * 1000000;
			}
			else if (TuneRegion->TuneGroup == tgDynamic) {
				Idx += 2 * 1000000;
			}
			Call_SubroutineNameInRegionList->AddObject(SubroutinName,
				(void*)((long)Idx));
		}
	}
}

/* ---------------------------------------------------------------------------- */
//
// 1.日本語名
//
// 2.パラメタ説明
//
// 3.概要
// リージョン内call用の関数があればInstallRoutines の先頭に複写して追加。
// setparam_OAT()等についても追加
//
// 4.機能説明
//
// 5.戻り値
//
// 6.備考
//
/* ---------------------------------------------------------------------------- */
void TPass5::Output_Call_SubroutineNameInRegionList() {
	FILE *fpAdd = NULL;
	int subIdx;
	string SubroutinName;
	int tokSPos, tokEPos;
	int i;
	TToken *Token;
	int ModuleIdx;
	string s;
	int Idx;

	// fprintf(fpAdd,"      character*100 ctmp\n");
	for (subIdx = 0; subIdx < Call_SubroutineNameInRegionList->Count; subIdx++)
	{
		SubroutinName = Call_SubroutineNameInRegionList->Strings[subIdx];
		Idx = (long)Call_SubroutineNameInRegionList->Objects[subIdx];
		tokSPos = Idx % 1000000;
		if ((Idx / 1000000) == 0) { // Install Region
			fpAdd = fpOutInstall; // インストール時用出力ファイル
		}
		else if ((Idx / 1000000) == 1) { // Static Region
			fpAdd = fpOutStatic; // 実行前時用出力ファイル
		}
		else if ((Idx / 1000000) == 2) { // Dynamic Region
			fpAdd = fpOutDynamic; // 実行時用出力ファイル
		}
		else {
			continue;
		}
		//
		// サブルーチンを複写で出力する。ここでの出力は変換後のSrcになる。
		// OATにあわせたコメント処理を行ったソースが必要
		//
		Token = (TToken*)TokenList->Items[tokSPos];
		ModuleIdx = Token->ModuleIdx;
		tokEPos = tokSPos;
		// fprintf(fpAdd,"!===== SUBROUTINE COPY START\n");
		fprintf(fpAdd, "\n");
		for (i = tokSPos; i < TokenList->Count; i++) {
			Token = (TToken*)TokenList->Items[i];
			if (ModuleIdx != Token->ModuleIdx) {
				tokEPos = i;
				break;
			}
			s = Token->OrgStr;
			// fprintf(fpAdd,"%s",s.c_str());
		}
		// 元ソースを変更したファイルを生成。（F90のソース)
		MakeSrcCode_Fortran(fpAdd, tokSPos, tokEPos);
		// fprintf(fpAdd,"!===== SUBROUTINE COPY END\n");
		fprintf(fpAdd, "\n");
		/*
		}else if(MainF->SrcCodeType == MainF->sctFortran77){
		MakeFunctionCode_Fortran77();	// OAT/OAT_(元のソースファイル名).f 作成
		}else{
		MakeFunctionCode_C();	// OAT/OAT_(元のソースファイル名).c 作成
		}
		 */
	}
	//
	// コントロールから SetParam()等を複写して OAT_SetParam_OAT()に変える。
	//
	MakeControlCode_Fortran(fpAdd);
}

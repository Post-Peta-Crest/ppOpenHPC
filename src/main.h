/*=====================================================================*
 *                                                                     *
 *   Software Name : ppOpen-AT                                         *
 *         Version : 1.0.0                                             *
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
 *=====================================================================*/
 #ifndef mainH
#define mainH

#include <stdio.h>
#include <string.h>
#include "VCL_Lib.h"

#include "pass1.h"
#include "pass2.h"
#include "pass3.h"
#include "pass4.h"
#include "pass5.h"

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    メインフォームクラス
//
//  3.機能説明
//    プログラムのメイン。ソースファイル名の所得やメッセージの表示、各パスの呼び
//    出しを行う。
//
//  4.備考
//
/*----------------------------------------------------------------------------*/

class TMainF
{
private:	// ユーザー宣言
	bool IsF90SrcFile(string SrcFileName);
public:		// ユーザー宣言

	enum TSrcCodeType { 	// 対象ソースコード種類。全体で１つ。
		sctNone = 0,
		sctFortran77 = 1, // フォートラン77相当（固定桁)
		sctFortran90 = 2, // フォートラン90
		sctC = 3,		// C
	} SrcCodeType;

	TStringList SrcFileNameList;		// 対象ソースファイル名リスト。
	TStringList InsertModulseHeadFileNameList;
	FILE *LogFp;	// ログ出力用

	int ErrCount;
	bool ErrF;
	bool NotSkipByErrF;					// エラーがあっても実行継続
	bool CloseReqF;
	bool EndF;
	bool VisualF;						// Viusalzationを行う（HTML+データ出力)
										// かのフラグ
	bool DebugF;						// Debug出力ON/OFF (OFF時は、コメントに
										// してSpeedUp)
	bool NoMPIF;						// -NoMPI指定（MPI関連のコードをSkip）
	bool MPIF;							// -mpi指定
	bool OMP_OuterF;					// -omp otr -omp_outer 指定
	bool OMP_InnerF;					// -omp_inner 指定
	string TimeFunc;					// -timeで指定する時刻関数
										// MPI_Wtime() OAT_Wtime() MyTime()
	string my_timer_start;				// -stime_and_etime で指定する測定関数名
	string my_timer_stop;				// -stime_and_etime で指定する測定関数名

	bool EECntlF;						// -eectrlのフラグ(ON時は、コントロール部分を置き換え）

	string cc_option_str;				// -cc=で指定するコンパイルオプション

	TScript *Call_SetParam_Script;	   	// #pragma call OAT_SetParam() のスクリプト
										// 未設定の場合はNULLとする。
	TScript *Call_ATExec_Script;	   	// #pragma call OAT_ATexe() のスクリプト
										// 未設定の場合はNULLとする。
	TStringList *Call_ATExec_ArgList;  	// ATExec()呼び出し時の引数リスト
										// ValName,ValDataのリスト。自動追加される。

	TList *TokenList;					// トークンリスト (Pass1で生成）
	TList *ValDataList;					// 変数リスト(Pass2で生成）
	TList *TuneRegionList;				// チューニングリージョンリスト(Pass4で
										// 生成)
	int CharMaxLen;						// 格納文字列の最大長さ(TRの名前を全て入
										// れた長さ）

	TStringList *TokStrList;
	bool IncludeCodeInContainsF;    // Include code in Contains

	void *Pass5;

	TMainF();
	~TMainF();
	void GetCommandLineArg(int argc,char* argv[]);
	void CodeGen();
	void print(string s);
	void Err(string s);
	void ErrMessage(int Pos,string s);
	void DispTokenList();
};

extern TMainF *MainF;

void DP(string s);	// Debug用

#endif

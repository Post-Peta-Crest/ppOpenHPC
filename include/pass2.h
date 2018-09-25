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
 /*----------------------------------------------------------------------------*/
//
//  概要
//    パス２　ヘッダ
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#ifndef pass2H
#define pass2H
//---------------------------------------------------------------------------

#include "pass1.h"

void Pass2(TList *TokenList,TList *ValDataList);
int Pass2_Fortran(TList *TokenList,int sPos,TList *ValDataList);
int GetValIdx(TList *ValDataLlst,string ValName,int ModuleIdx);

void AddDataDef_Fortran(TList *TokenList,int sPos,int ePos,TList *ValDataList);
void AddDataDef_C(TList *TokenList,int sPos,int ePos,TList *ValDataList);

#define ARRAY_MAX	8	// 最大８次元までの配列に対応する。

//
// 変数データ種類
//
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    変数のデータ種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum Enum_DataType{
	dt_Int,
	dt_Real,
	dt_Char,
	dt_Logic,
	dt_Complex,
	dt_Float,	// Add 2011/07/27
	dt_Void,   	// Add 2012/02/23

	dt_IntFunc,
	dt_RealFunc,
	dt_CharFunc,
	dt_LogicFunc,
	dt_ComplexFunc,
	dt_FloatFunc,	// Add 2011/07/27
	dt_VoidFunc,	// Add 2012/02/23
};
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    Fortran変数クラス
//    新しい変数がソース内に見つかるごと作成される。同じモジュールの同じ変数は同
//    じ扱いとなる。
//    変数データ内には、ＤＯ変数かどうかや、参照回数などの情報も格納される。
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TValData{

public:
	string Str;						// 変数名

	bool ParamValF;					// 定数 PARAMETER の定義変数であることを
									// 示す。
									// ただし、int,real 以外は、定数としては
									// 扱われない。

	bool ArrayOrFuncF;				// 変数名の直後に（があった場合にセット
									// される。
	bool ArgF;						// SUBRUTION,FUNCTIONの引数として使用さ
									// れている。

	int ModuleIdx;					// モジュールＩｄｘ (endで++)　スコープ
									// 範囲
	int BrNestLevel;				// {} () のレベルグローバルかどうかの判定に使用。

	Enum_DataType DataType;			// int,realなどのデータ種別
	int DataLength;					// real*8 などのデータ長を示す。別定義時
									// に必要

	string DefStr1;					// "double **" 等の double 定義部分の文字列。
	string DefStr2;					// "double **" 等の ** 定義部分の文字列。

	int DefPosS;					// real*8 などの 定義部分のTokenPosなど
									// の範囲
	int DefPosE;					// real*8 などの 定義部分のTokenPosなど
									// の範囲

	int DefPos;						// 最初に定義で出現したPos
	int ArrayDefPosS;				// Array定義時のTokenPos (N,N) などの範
									// 囲
	int ArrayDefPosE;				// Array定義時のTokenPos (N,N) などの範
									// 囲
	int RefCount;					// 変数参照Count
	int SetCount;					// 変数設定Count

	double dData;					// 定数の場合の値
	int ArrayLevel ;				// 配列次元数

	int ArrayStart[ARRAY_MAX];		// 配列要素開始番号リスト -1は未定
	int ArrayCount[ARRAY_MAX];		// 配列要素数リスト -1は未定

	// UnRoll時に一時的に使用される。
	// 複数のDo(DoValCount)に対応するために、bitに修正 2004/3/13
	// RefDoValBitsは、複数のBitが１になる場合もある。
	//
	unsigned int DoValBits;			// Do変数自身かどうか。
	unsigned int RefDoValBits;		// Do変数依存かどうか。
	unsigned int DoEndValBits;		// UnRollのDoの終わり値の変数（式はＮＧ）

	TValData(string aStr);			// 生成
	~TValData();					// 破棄

	string ToString();		  		// 文字列で情報を所得
	string GetDefStr_C();
	string GetDefStr_Fortran();

};

#endif

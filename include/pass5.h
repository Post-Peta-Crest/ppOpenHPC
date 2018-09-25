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
//    パス５　ヘッダ
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#ifndef pass5H
#define pass5H
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    パス５クラス
//    解析結果からソースコードを生成する。TuneRegionクラスなどを参照して生成を行
//    う。
//
//  3.機能説明
//
//
//  4.備考
//
/*----------------------------------------------------------------------------*/

#include "main.h"

class TPass5{
private:
	string SrcFname;

	TList *TokenList;
    TList *ValDataList;
	TList *TuneRegionList;

  	TList fpOutList;	  				// 出力ファイルリスト（複数に対応)
  	FILE *fpOutInstall;					// インストール時用出力ファイル
  	FILE *fpOutStatic;					// 実行前時用出力ファイル
  	FILE *fpOutDynamic;					// 実行時用出力ファイル
	FILE *fpOutControl;					// コントロール用出力ファイル
	FILE *fpOutHeader;					// 関数プロトタイプヘッダ用出力ファイル

	FILE *fpInOATHeader; 				// ヘッダ元ファイル（OAT_base.h）入力用
	FILE *fpOutOATHeader;             	// ヘッダ(OAT.h)出力用

	bool FittingF;						// Fittingが使用されているかどうか？
										// 必要な変数定義などが追加される.(SetPa
										// ram()など）
	char Comment;

	void MakeControlCode_Fortran();
	void MakeControlCode_C();
	void MakeFunctionCode_Fortran();
	void MakeFunctionCode_Fortran77();
	void MakeFunctionCode_C();
	int AddIncludePosList(TList *InsertIncludePosList);
	void AddUsePosList(TList *InsertUsePosList);
	void OutputModuleStart(FILE *fp,string ModuleName);
	void CheckAndDeleteLocalValDefine();

public:
	TPass5(TList *aTokenList,TList *aValDataList,TList *aTureRegionList);
	~TPass5();

	void Exec();

};

#endif

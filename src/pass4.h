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
//    パス４　ヘッダ
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "main.h"

#ifndef pass4H
#define pass4H
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    パス４クラス
//    スクリプトから TuneRegionの生成を行う
//    Pass4自体をTuneRegionからアクセス可能として、現在の状態を参照可能とする。
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/

class TPass4{
private:

  	FILE *fpOut;						// 出力ファイル
  	FILE *fpOutInstall;					// インストール時用出力ファイル
  	FILE *fpOutStatic;					// 実行前時用出力ファイル
  	FILE *fpOutDynamic;					// 実行時用出力ファイル
  	FILE *fpOutControl;					// コントロール用出力ファイル


public:
	TList *TokenList;
    TList *ValDataList;
    TStringList *OAT_ValList;
	TList *TuneRegionList;

	TPass4(TList *TokenList,TList *ValDataList,TList *TuneRegionList);
	~TPass4();
	void Exec();

};

#endif

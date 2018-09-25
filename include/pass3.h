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
//    パス３　ヘッダ
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "main.h"

#ifndef pass3H
#define pass3H

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    スクリプト種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/

void Pass3(TList *TokenList,TList *ValDataList);
string SepLongStr(string s);

enum Enum_ScType  // 自動チューニング種類
{
	sct_install,						// インストール自動チューニングの指定
	sct_static,							// 実行軌道前自動チューニングの指定
	sct_dynamic,						// 実行時自動チューニングの指定
	sct_command,						// CやFORTRAN 90 の式（コマンド）
										// そのまま出力される。
										// !ABCLib$の後の文字列をそのまま出力
	// 補助指定子
	sct_name,
	sct_parameter,
	sct_select,
	sct_according,
	sct_varied,
	sct_fitting,
	sct_prepro,
	sct_postpro,

	sct_debug,
	sct_DynPefThis,	// Add 2004/09/15
	sct_Number,
	sct_BPset,		// Add 2009/03/05
	sct_Allocate,	// Add 2010/12/28 GPU用 allocate ()
	sct_pre_BPset,	// Add 2011/09/05 GPUベンチプログラム用での仕様追加
	sct_post_BPset,

	sct_SetParm, 	// call SerParam での位置指定用。 Add 2012/02/28
	sct_ATExec, 	// call AtExec
	sct_None,	 	// 継続行等で前のスクリプトに追加した場合 Add 2012/02/28

	// Split用 Add 2012/03/05
	sct_SplitPoint,				//
	sct_SplitPointCopyDef,		// SplitPointCopyDef region start ... end
	sct_SplitPointCopyInsert,	// SplitPointCopyInsert region start ... end

	// RotaionOrader 用 Add 2013/03/10
	sct_RotationOrder,			// RotateionOrder sub region start ... end

};

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    スクリプト機能、Install,Static,Dynamic の後に付く。
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum Enum_ScAction  // 機能
{
	sca_none,
	sca_define,							// パラメタを設定する処理であるという指
										// 定
	sca_variable,						// 変動するパラメタであるという指定
	sca_select,							// 複数の手続きから選択する処理であると
										// いう指定
	sca_unroll,							// 以下の処理をループアンローリングする
										// という指定

										// Fusion,Split用 Add 2012/03/05
	sca_loopfusion,						// 以下をループフィージョンする。
	sca_loopsplit,
	sca_loopfusionsplit,
	sca_rotationorder,
};

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    スクリプトリージョン指定種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum Enum_ScRegion  // Region指定子
{
 	scr_none,
 	scr_start,
	scr_end,
	scr_substart,
 	scr_subend
};

//
// スクリプトで使用される変数
// Fortranの変数とは別。
//
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    スクリプト変数クラス
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TScValData{
public:
	int RefCount;						// 参照カウント
	string Str;						// 変数名
	double dData;						// 変数の値（確定していない場合もあり）
    double sData;						// 範囲を持っている場合の開始・終了値
										// varied () from 1 to 8 などで上書きさ
										// れる。
    double eData;						// 範囲を持っている場合の開始・終了値
										// varied () from 1 to 8 などで上書きさ
										// れる。
    					// varied () from 1 to 8 などで上書きされる。

	TScValData(string ValName);
};

//
// スクリプト単位で生成されるクラス
// 実際の実行時は、スクリプト実行エンジンが、解釈を行いながら実行する形とする。
// 例えば unroll (i) があった場合に、実行エンジンのunroll Reqがセット
// 内部状態を変更しながら実行する形となる。
//

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    スクリプトクラス
//    １つのABCScript文に対応したスクリプトデータのクラス
//
//  3.機能説明
//    スクリプトクラスは、各スクリプトの構文解析した結果が入る。これには、スクリ
//    プトの種類は対象変数、範囲の数値などが格納される。１文のスクリプトが１デー
//    タに対応し、TuneRegionクラスがて複数のスクリプトクラスも参照によって生成さ
//    れる形となる。
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TScript{

public:
	string Str;							// スクリプト自体の文字列
	int TokPos;							// スクリプトが置かれているToken位置
	int LineEndTokPos;					// スクリプトの最終(改行)トークン位置
	void *TuneRegion;
	TScript *PreviousScript;
	TScript *NextScript;

    Enum_ScType ScType;					// 自動チューニング種類 (static , instal
										// l ,,,)
	Enum_ScAction ScAction;				// 機能 (select , unroll,,,)
	Enum_ScRegion ScRegion;				// Region指定 (region start...)
    TList *ScValDataList;				// 対象変数リスト　ScValDataへのリスト
										// Fortranの変数とは別でスクリプト内での
										// 変数
										// Region Start-Endの間で有効となる。
	TStringList *TokStrList;

    //
    // Fitting関連のデータ
    //

//d-spline用追加部分
	int FittingDspline;
	int FittingDynamic;
//d-spline用追加部分 ここまで

	int FittingType;					// Type = 0:なし、1:least-square,2:user_
										// defined
    int FittingDegree;					// 多項式の次数
    TList *SampledList;					// intの形で Sampledの値がセット

	TScript(TList *TokenList,int sPos,TScript *LastScript,TList *ValDataList); // 生成
    ~TScript(); // 破棄
	void GetFittingParam(int sPos);
	string GetATExecArgStr(bool DefineF,string ValName1,string ValName2,int SkipArgCount=0);
	int CheckBrBalance();
	bool IsATExecArg(void *ValData);

};

#endif

/*=====================================================================*
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
 *=====================================================================*/
 /*----------------------------------------------------------------------------*/
//
//  概要
//    チューニングリージョンクラス　ヘッダ
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "main.h"

#ifndef TuneRegionH
#define TuneRegionH

//---------------------------------------------------------------------------
//
// 各チューニングリージョンごとに生成されるクラス
// これを元に Pass5でソースコードが生成される形式に変更
// TuneRegionListに追加される。
//
// 生成は、Pass4でのソースをスキャンする形で行われる。
//
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    チューニンググループ種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum TTuneGroup {
	tgInstall,
	tgStatic,
	tgDynamic,
};

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    チューニング種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum TTuneKind {
	tkDefine,
	tkVariable,
	tkSelect,
	tkUnroll,
	tkLoopFusionSplit,
	tkLoopFusion,
	tkLoopSplit,
	tkRotationOrder,

	tkList,				// Add 2012/02/26
	tkVariableD,
	tkListD,
	tkReplace,
	tkGWV,
	tkList_End,			// List種類の終わりを示す識別用。使用はしない。
};
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    サブリージョン種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum TSubRegionKind {
	srSelect							// サブリージョン種類
}; 	// サブリージョン種類

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    GPU種類
//
//  3.機能説明
//	  #Pragma ABCLib allocate で指定される GPUオプション
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum TGPUOption {
	gpu_option_None,	// GPUオプションなし
	gpu_option_CPU,		// GPUオプション CPU
	gpu_option_GPU,		// GPUオプション GPU
	gpu_option_auto,	// GPUオプション auto
};
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    チューニングリージョンクラス
//    各チューニングリージョンごとに生成され、解析に応じてデータが設定される。
//    ソース生成時には、このクラスをアクセスして出力コードが生成される。
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TTuneRegion{
private:
	TPass4 *Pass4;

    // UnRollで共通利用される変数
    int CaseIdx;
    unsigned int UnRollDoRefValBits;			// Unroll対象のDO変数Bit CaseIdxごとに変
										// 化
    int UnrollCount[32];
	int DoTokPos[32];
	int EndDoTokPos[32];
	string DoValStr[32];
	string DoVal_m[32];
	string DoVal_i[32];
	string DoVal_l[32];
	string DoValName[32];
	int DoValIdx;
    int DoValCount;
    char Comment;

	void OutputLoopFusionExecCode_Fortran(FILE *fp);
	void OutputUnrollExecCode_Fortran(FILE *fp);
	void OutputLoopFusionExecCode_C(FILE *fp,FILE *fpOutHeader);
	void OutputUnrollExecCode_C(FILE *fp,FILE *fpOutHeader);

	string GetNewValName(string BaseName,int Idx,TStringList *ArgValList);
	string GetNewValName(string BaseName,string AddStr,TStringList *ArgValList);
	string GetNewValNameForValData(TValData *ValData,
			unsigned int UnRollDoRefValBits,int Idx,TStringList *ArgValList);
	int GetBitIdx(unsigned int Bits);
	int GetValCountOfBit(unsigned int Bits,int *Count);
	string ChangeFromOrgStr(string OrgStr,string Str);
	int GetDoValAddValue(int kk,unsigned int UseValBits,int DoValIdx,int *Count);
//    int TTuneRegion::GetValAddValue(int kk,DWORD UseValBits,DWORD UseDoValBits,int DoValIdx,int *Count);
	int GetValAddValue(int RefValIdx,unsigned int UseDoValBits,int DoValIdx,int *Count);
	int GetValAddValue2(int RefValIdx,unsigned int RefDoValBits,
		unsigned int UnRollDoRefValBits,int DoValIdx,int *Count);

//	string SepLongStr(string s);
	void MakeArgValList();
	int GetNextSemikoronPos(int sPos);
	int FindEndDo2(TList *TokenList,int sPos);
	int FindEndIf(TList *TokenList,int sPos);
	string GetPrePostSubregionStr(int Mode);

public:		// ユーザー宣言
	TList *TokenList;
    TList *ValDataList;
	int TokenStartPos;					// 開始トークン位置（Pass4内参照)
	int TokenEndPos;					// 終了トークン位置

	string Name;					// 名前 name で指定される。
	string FuncName;				// 呼び出し関数・サブルーチン名
	string RefValStr;				// デバッグ用（変数参照文字列リスト）
	TTuneGroup TuneGroup;
	string TuneGroupName;			// Install,Static.Dynamic
 	TTuneKind TuneKind;

	bool UseAccordingF;					// Sub Regionを含めて According指定あり
										// 。
	string AccordingStr;			// Accordingで指定された　数式
	bool UsedDynPefThis;				// DynPefThisスクリプトが使用された。
										// Dynamicで呼び出し時の setParam()をスキッ
										// プ
	int Number;							// スクリプトNumberでの指定

	TStringList *ArgValList;			// サブルーチンにする場合の引数リスト
	TList *UndefinedArgValList;			// 定義が見つからない引数データ
	int CaseCount;						// リージョン内の自動チューニングが取りうる種類数
	TList *ValBitsList;					// ValData->Bits保持用
	TList *SubRegionList;				// サブリージョンリスト TSubRegion クラス
										// Select sub region など

	string OffsetStr;

	TStringList *BaseValList;			// 基本パラメータ名の変数リスト Add 2009/03/05
										// BPset(N) や BPset(N,NX)などで設定される。
										// 変数名+ValDataへのポインタ
										// リージョンデータ作成時にセットされる。
	TStringList *ParamValList; 			// パラメータ文の変数リスト

	//
	// varid (xx[,xx]) from X to Y の指定情報 (varied指定時に有効)
	//
	int variedCount;
	string variedValName[32];		// 変数名 （必須）
/**************************************************************************/
//
//Kogakuin Irie
//実数対応のための変更
//既存コードはコメントアウト
//
//	int variedFromValue[32];			// 開始値
//	int variedToValue[32];				// 終了値
	float variedFromValue[32];			// 開始値
	float variedToValue[32];			// 終了値
	float variedStepValue[32];			// 増分（間隔）値
//
//変更ここまで
//
/**************************************************************************/
	void *variedValData[32];			// 変数へのポインタ


//
//	variedD用変数 Add 2016/03/01
//
	int variedDCount;
	string variedDValName[32];		// 変数名 （必須）
	float variedDFromValue[32];			// 開始値
	float variedDToValue[32];			// 終了値
	float variedDStepValue[32];			// 増分（間隔）値
	void *variedDValData[32];			// 変数へのポインタ

	//
	// Fittingのためのデータ
	//
//d-spline用追加部分
	int FittingDspline;
	int FittingDynamic;
//d-spline用追加部分ここまで
	int FittingType;					// Type = 0:なし、1:least-square,2:user_
										// defined
	int FittingDegree;					// 多項式の次数
	TList *SampledList;					// intの形で Sampledの値がセット（実体は
										// 、Scriptのデータ）

	//
	//	GPUのためのデータ 2010/12/28 追加
	//
	TGPUOption GPUOption;

	//
	//	LoopFusionSplitのためのデータ 2012/03/07
	//
	int SplitPointCopyDef_StartPos;
	int SplitPointCopyDef_EndPos;

	//
	//	Option指定文字列
	//
	string OptionStr;

	//
	// Selectの解析を行う
	//
	void MakeSelectData_Old(int sPos);	// 以前のバージョン 2016/02/26
	void MakeSelectData(int sPos);		// Replace等に対応
	//
	//
	// UnRollの場合の解析結果
	// DoのNestは、最大32までを対象とする。(元はPass4で解析していた)
	//
	void MakeUnrollData(int sPos);

	TToken *DoToken[32];				// Unrollの一番外側のDoトークン
	TToken *DoValToken[32];				// Do変数
	int DoStartSPos[32];				// Doの初期値の式
	int DoStartEPos[32];
	int DoEndSPos[32];					// Doの終わり値の式
	int DoEndEPos[32];
	int DoStepSPos[32];					// Doの増分値の式
	int DoStepEPos[32];

	//
	//  LoopFusionの解析結果
	//
	int FusionCount;

	void MakeLoopFusionSplitData(int sPos);
	TList *MakeRotaionOrderSrcTokenList();

	TList *RotaionOrderList;	// RotationOrderのリージョントークン位置
								// 先頭と最後が交互に入る。

	//
	//	Replase等の解析結果。
	//
	TStringList *ReplaceStrList;		// replace指定子で定義された文字列リスト
	TStringList *TargetStrList;			// targer指定子で定義された文字列リスト。最後の()を含む場合あり。
	TStringList *ListSrcStrList; 		// list指定子で定義された置換え元変数名リスト
	TStringList *ListReplaceStrList;	// list指定子で定義された置換えリスト ListSrcList*N個の要素
	TStringList *ListDSrcStrList; 		// listD指定子で定義された置換え元変数名リスト
	TStringList *ListDReplaceStrList;	// listD指定子で定義された置換えリスト ListSrcList*N個の要素
	TList *GWV_ListList;				// GWV変換用のリストへのリスト
	TStringList *GWV_TargetStrList;		// targer指定子で定義された文字列リスト。最後の()を含む場合あり。

	//
	// Selectの場合の解析結果
	//

	int GetNextKanmaPos(int sPos);
	void ChackRefDoValF(int sPos,int ePos);
	void AddArgValListToATExecArgList();
	void SaveAndResetValBits(int sPos,int ePos);
	void RestoreValBits(int sPos,int ePos);

	TTuneRegion(TPass4 *aPass4,int Pos); 		// 生成
	~TTuneRegion();			// 破棄

	string GetInfo();   // 情報を改行入り文字列で所得（デバッグ用）

	void OutputSetParamCode_Fortran(FILE *fp,int TempLineNo,FILE *fpAdd);
	void OutputSetParamCode_C(FILE *fp,int TempLineNo);
	void OutputAutoExecCode_Fortran(FILE *fp);
	void OutputAutoExecCode_C(FILE *fp,FILE *fpOutHeader);
	void OutputExecCode_Fortran(FILE *fp);
	void OutputExecCode_C(FILE *fp,FILE *fpOutHeader);
//	void TTuneRegion::OutputSrcCode(FILE *fp,AnsiString IndentStr,bool InCaseF);
	void OutputSrcCode(FILE *fp, string IndentStr, int OutMode,int Use_OAT_functuonNameF = 0);
	void OutputSelectCaseExecCode_Fortran(FILE *fp,int iusw1);
	void OutputSelectCaseExecCode_C(FILE *fp,int iusw1);

	void OutputReplaceSrc(FILE *fp,void *TargetSubRegion,int iusw1,bool Skip_Pragma_ACCF);

//	int OutputUnroll_DoBlock_Fortran(FILE *fp,int TokPos,unsigned int UseDoRefValBits,int RefValIdx);
	int OutputUnroll_DoBlock_Fortran(FILE *fp, int TokPos,
		unsigned int UsedDoRefValBits, int RefValIdx,string CommentStrBeforeDo = "");
	int OutputUnroll_DoBlock_C(FILE *fp,int TokPos,
		unsigned int UseDoRefValBits,int RefValIdx,string CommentStrBeforeDo = "");
	int OutputUnroll_IfBlock_Fortran(FILE *fp,int TokPos,unsigned int UsedDoRefValBits,int RefValIdx);
	int OutputUnroll_IfBlock_C(FILE *fp,int TokPos,unsigned int UsedDoRefValBits,int RefValIdx);
	int OutputUnroll_Line_Fortran(FILE *fp,int TokPos,unsigned int UseDoRefValBits,int RefValIdx);
	int OutputUnroll_Line_C(FILE *fp,int TokPos,unsigned int UseDoRefValBits,int RefValIdx);

	int CalNewRefValIdx(int RefValIdx,unsigned int UseDoRefValBits,
			int AndRefValIdx,unsigned int AndUseDoRefValBits,int *UnrollCount);

//    AnsiString TTuneRegion::GetPrePostSubregionStr(int Mode);
	string GetPrePostSubregionStr(int Mode,int InControlCode);
	string GetPrePostBPSetSubregionStr(int Mode);
	string Merge_Unroll_Line_C(string *BaseOutputStr,
		string LastOutputStr,string *CurLineStr,int AddOprStrPos);

//	void OutputSetParmSrcCode(FILE *fp,string IndentStr);
	void OutputSetParmSrcCode(FILE *fp, string IndentStr,int Use_OAT_functuonNameF = 0);
	string ChangePragmaStr(string OrgStr,int TokPos,int EndTokPos,
			TStringList *NewValStrList);

	bool IsATExecArg(void *ValData);
	void OutputFusionDo_Fortran(string FusionValName,FILE *fp,int DoNest,int FusionIdx);
	void OutputFusionDo_C(string FusionValName,FILE *fp,int DoNest,int FusionIdx);

};
//
// SubReguon Class
//
/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    サブリージョンクラス
//    スクリプトで定義されるサブリージョンのデータが格納される。
//	  スプリットの場合にも使用される。
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TSubRegion{
public:
	Enum_ScType ScType;					// 種類 sub region start での先頭キーワード
	int TokenStartPos;
	int TokenEndPos;
	string AccordingStr;			// Accordingで指定された　数式

	//
	// varid (xx[,xx]) from X to Y の指定情報 (varied指定時に有効)
	//
	int CaseCount;

	int variedCount;
	string variedValName[32]; 		// 変数名 （必須）
/**************************************************************************/
//
//Kogakuin Irie
//実数対応のための変更
//既存コードはコメントアウト
//
//	int variedFromValue[32];			// 開始値
//	int variedToValue[32];				// 終了値
	float variedFromValue[32];			// 開始値
	float variedToValue[32];			// 終了値
	float variedStepValue[32];			// 増分（間隔）値
//
//変更ここまで
//
/**************************************************************************/
	void *variedValData[32];		// 変数へのポインタ

	unsigned int SplitBitsOfFusionIdx[32];	// FusionIdxによる Split有効無効Bits
									// DoValと同じ並びのBitsになる。
									// ２^Bits数が各FusionIdxにおけるSplit数になる。
	int SplitCaseCountOfFusionIdx[32];		// FusionIdxによる SplitCase数

	TSubRegion();

};

#endif

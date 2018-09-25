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
//    パス１　ヘッダ
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "main.h"

#ifndef pass1H
#define pass1H

void Pass1(string SrcFileName,TList *TokenList);

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    トークン種類
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum TTokId{

	tid_null,
	tid_LineEnd,			// 行の終わり

	tid_Comment,		 	// コメント　/* //
//
//	プリプロセッサ
//
	tid_SharpStart,
	tid_SharpInclude,
	tid_SharpDefine,
	tid_SharpUnDef,
	tid_SharpIf,
	tid_SharpElse,
	tid_SharpElIf,
	tid_SharpEndIf,
	tid_SharpIfDef,
	tid_SharpIfnDef,
	tid_SharpPragma,
	tid_SharpEnd,

//
//	キーワード
//
	tid_KeyWordStart,   //  キーワード開始
	tid_auto,
	tid_break,
	tid_case,
	tid_char,
	tid_const,
	tid_continue,
	tid_default,
	tid_do,
	tid_double,
	tid_else,
	tid_enum,
	tid_extern,
	tid_float,
	tid_for,
	tid_goto,
	tid_if,
	tid_inline,
	tid_int,
	tid_long,
	tid_register,
	tid_restrict,
	tid_return,
	tid_short,
	tid_signed,
	tid_sizeof,
	tid_static,
	tid_struct,
	tid_switch,
	tid_typedef,
	tid_union,
	tid_unsigned,
	tid_void,
	tid_volatile,
	tid_while,
	tid__Bool,
	tid__Complex,
	tid__Imaginary,

	// キーワード（文字）
	tid_Kannma,
	tid_Semikoron,
	tid_Semikoron2,	// for(;;) 用
	tid_Koron,

	//
	// 演算子 解析を行うならば、区別と演算優先順位が必要。
	// 最初は単語区切りで tid_Oprとする。
	//
	tid_Set,
	tid_Kakko,
	tid_Kokka,
	tid_KagiKakko,
	tid_KagiKokka,
	tid_DaiKakko,
	tid_DaiKokka,
	tid_Dot,
	tid_Set2,	// += 等

	// 演算子
	tid_KoronKoron,						// ::

	tid_Opr,							// 演算子 （単項、２項、ロジックなど全て）
	tid_Val,							// 変数名
	tid_Func,							// 関数名  Func(xxx) の形となる。

	tid_DataType,						// int,double等以外の型指定

	//
	//	この下は、主にFortran用
	//
	tid_ConstNum,						// 数値定数

	//
	// 字句解析結果からのトークン種別
	// 一部の文字列は、関数としてまとめて扱われる。
	//
	tid_ConstStr,						// 文字列定数
	tid_ConstInt,						// 整数定数
	tid_ConstReal,						// 実数定数
	tid_Label,							// 文番号、またはラベル

	tid_ContinuedLineStr,				// 継続行用 &\n    &

	//
	// Pass1_5()で生成
	//

	// Program関連
	tid_PROGRAM,						// PROGRAM
	tid_SUBROUTINE,						// SUBROUTINE
	tid_FUNCTION,						// FUNCTION
 	tid_MODULE,							// MODULE
	tid_INTERFACE,						// INTERFACE
	tid_CONTAINS,						// CONTAINS
	tid_USE,							// USE
	tid_INCLUDE,						// INCLUDE

	// データ宣言
	tid_COMMON,
	tid_INTEGER,
	tid_REAL,
	tid_DOUBLEPRECISION,
	tid_CHARACTER,
	tid_LOGICAL,
	tid_COMPLEX,
	tid_IMPLICIT,
	tid_TYPE,
	tid_TYPE_REF,
	tid_NAMELIST,
	tid_ALLOCATABLE,
	tid_POINTER,
	tid_TARGET,
	tid_SAVE,
	tid_PRIVATE,
	tid_PUBLIC,
	tid_EXTERNAL,
	tid_INTRINSIC,
	tid_INTENT,
	tid_OPTIONAL,
	tid_WHERE,
	tid_EQUIVALENCE,					// EQUIVALENCE

	//動的記憶割付け
	tid_ALLOCATE,
 	tid_DEALLOCATE,
 	tid_NULLIFY,

    // 制御文
 	tid_IF,
 	tid_THEN,
 	tid_ELSE,
	tid_ELSEIF,
	tid_ENDIF,
	tid_DO,
	tid_ENDDO,
	tid_SELECT,
	tid_CASE,
	tid_GOTO,
	tid_DOWHILE,
	tid_CALL,
	tid_RETURN,
	tid_STOP,
	tid_END,
	tid_END_STR, 	// コメント内のEND resion end 等

    // 入出力
 	tid_READ,
 	tid_WRITE,
 	tid_PRINT,
 	tid_BACKSPACE,
 	tid_INQUIRE,
 	tid_REWIND,
 	tid_CLOSE,
 	tid_OPEN,
 	tid_ENDFILE,
 	tid_FORMAT,

    // その他
 	tid_ASSOCIATED,
 	tid_CONTINUE,
 	tid_CYCLE,
 	tid_DATA,
 	tid_DIMENSION,
 	tid_EXIT,
 	tid_PARAMETER,

};

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//   トークン位置での変数参照タイプ
//
//  2.概要
//    変数の参照状態
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
enum Enum_RefType
{
 	vrf_def,							// 定義
	vrf_ref,                           	// 参照
 	vrf_set,							// 設定
 	vrf_refset,							// a = a+ 1 などの設定時は、同時
};

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//    トークンデータ
//
//  2.概要
//    トークンクラス
//    字句解析後のトークン単位のクラス。トークンクラスには、変数としての値や属性
//    なども格納される。
//
//  3.機能説明
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TToken{

public:
	string Str;					// トークン文字列
	TTokId TokId;				// トークン種別番号

	string OrgStr;				// タブやスペースも含めた文字列
	int Idx;					// TokenList上での Idx位置 Pass1_5 終了
								// 後に有効
	int LineNo;					// ソースコードでの行番号

	int ModuleIdx;		 		// 何番目のモジュールかを示す。
	int NestLevel;		 		// Endで-- SUBRUTINE などで++ されるブロ
								// ック深さ
	int BrNestLevel;			// {} と ()の両方のNestLevel
	int Indent;			 		// if -- endif , do -- endif のネスト位置
								//
	int priority;	  			// 演算子 優先順位（演算子の場合のみ有効）
//	int JmpTokenPos;	 		// DO->EndDo+1 などのとび先

	void *ValData;		 		// 変数トークンの場合の変数 (変数以外の
								// 場合はNULL)
	void *Script;
	Enum_RefType RefType;		// 変数の参照か、代入かを示す。
								// サブルーチンCallなどは、IN,OUT,INOUTの指定が出来るので
								// vrf_refsetが設定される。

	double dData;				// 数値データ
								// 数値定数トークンおよび、
								// 変数トークン(vrf_set)で定数代入時 (a=
								// 3など)のみ有効

	TList *ChangedTokList;	 	// Pass5でのコード生成時に置換されたTokenList
								// 置換したリストは、ValData=NULL,Scrip=
								// NULLで
								// 使用のみとすること。

	bool LineEndF;	// 行の終わり

	TToken(string aStr,int Id);
	~TToken();	// 破棄
	string GetTokIdStr();

};

#endif


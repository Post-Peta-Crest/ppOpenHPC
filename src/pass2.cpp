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
//    パス２
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "pass2.h"
#include "math.h"

//---------------------------------------------------------------------------

void ResetModule();
void Pass2_Fortran(TList *TokenList,int sPos,int ePos,TList *ValDataList);
int Pass2_C(TList *TokenList,int sPos,TList *ValDataList);
int Eval(TList *TokenList,int sPos,int ePos,TList *ValDataList,double *dData);
void IMPLICIT(TList *TokenList,int sPos,int ePos);
void AddSubFuncDef_Fortran(TList *TokenList,int sPos,int ePos,TList *ValDataList);
void AddSubFuncDef_C(TList *TokenList,int sPos,int ePos,TList *ValDataList);

static Enum_DataType ImplType[256];		// 暗黙の宣言かどうか
bool EvalConstF = false;

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    ValDataList 変数リスト
//
//  3.概要
//    構文解析と変数リストを作成し、変数の型などを設定する。
//
//    　１）モジュール構造識別（ModukeIdxを割り振って、異なるモジュールの区別)
//    　２）｛｝のネストをNestLevelにセットする。
//    　３）for , if の終了先をセットする
//    　４）変数定義 (int , double ) などの情報から、変数一覧(ValDataList)を作成し
//    　　　リンクを設定する。（モジュールが異なる変数は、別物とする）
//    　　　変数の型、バイトサイズも保持すること。（一時変数作成の場合に必要）
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void Pass2(TList *TokenList,TList *ValDataList)
{
	int i,j;
	TToken *Token;
	TToken *Token2;
	int ModuleIdx = 0; 		// 何番目のモジュールを処理中か
	int NestLevel = 0; 		// プログラム構造ネストの状態
	int BrNestLevel = 0;  	// 括弧のネストの状態

	ModuleIdx = 0;
	NestLevel = 0;
	BrNestLevel = 0;
	ResetModule();
	for(i = 0 ; i < TokenList->Count ;i++){
		Token = (TToken *)TokenList->Items[i];
		Token->ModuleIdx = ModuleIdx;   // トークンにモジュールやネスト番号をセット
		Token->NestLevel = NestLevel;
		Token->BrNestLevel = BrNestLevel;
		switch(Token->TokId){
		case tid_END:
			// Fortran モジュールを終了。（次行から次モジュールとなる）
			// Endの後の名前は、Skipする。
			// スクリプトのEndの場合があるので、前が注釈でない（改行まで）かチェック
			for(j = i-1; j >= 0 ; j--){
				Token2 = (TToken *)TokenList->Items[j];
				if((Token2->TokId == tid_LineEnd)||(j == 0)){
					NestLevel--; // 文法上のEND
					break;
				}
				if(Token2->TokId == tid_Comment){
					Token->TokId = tid_END_STR;	// コメントの後の end (  region end )
					break;
				}
			}
			if(Token->TokId == tid_END){
				//
				//  CONTAINS に対応するため、NestLevelが0でなくても別モジュール。
				//  サブルーチンの記述の中のサブルーチンの記述への対応。
				//
#if 0  	// end soubroutin 識別用に修正 2016/02/25
				if((i+1 >= TokenList->Count)||
					(((TToken *)TokenList->Items[i+1])->TokId != tid_TYPE)){
					ModuleIdx++;
					ResetModule();
				}
				for(; i < TokenList->Count ;i++){
					Token2 = (TToken *)TokenList->Items[i];
					Token2->ModuleIdx = ModuleIdx;   // トークンにモジュールやネスト番号をセット
					Token2->NestLevel = NestLevel;
					Token2->BrNestLevel = BrNestLevel;
					if(Token2->TokId == tid_LineEnd){
						break; // LineEndまでスキップ END TYPE , END FUNCTION 等の対応
					}
				}
#else
				for(; i < TokenList->Count ;i++){
					Token2 = (TToken *)TokenList->Items[i];
					Token2->ModuleIdx = ModuleIdx;   // トークンにモジュールやネスト番号をセット
					Token2->NestLevel = NestLevel;
					Token2->BrNestLevel = BrNestLevel;
					if(Token2->TokId == tid_LineEnd){
						break; // LineEndまでスキップ END TYPE , END FUNCTION 等の対応
					}
				}
				if((i+1 >= TokenList->Count)||
					(((TToken *)TokenList->Items[i+1])->TokId != tid_TYPE)){
					ModuleIdx++;
					ResetModule();
				}
#endif
			}
			break;
		// Fortran Program関連
		case tid_CONTAINS:		// CONTAINS
			ModuleIdx++;
			ResetModule();
			break;
		case tid_PROGRAM:		// PROGRAM
		case tid_MODULE:		// MODULE
		case tid_INTERFACE:		// INTERFACE
		case tid_SUBROUTINE:	// SUBROUTINE
		case tid_FUNCTION:		// FUNCTION
			NestLevel++;
			break;
		case tid_TYPE:
			// TYPE（）以外の TYPE 構造体名の場合のみ ネストが有効。
			if((i+1 < TokenList->Count)&&(((TToken *)TokenList->Items[i+1])->TokId != tid_Kakko)){
				// TYPE 構造名 による定義。 END TYPEを持つ。
				NestLevel++;
			}else{
				Token->TokId = tid_TYPE_REF;  // 定義でないType。END TYPEを持たない。
			}
			break;
		case tid_DaiKakko:	// C {
			NestLevel++;
			BrNestLevel++;
			break;
		case tid_DaiKokka:	// C }
			NestLevel--;
			BrNestLevel--;
			if(NestLevel == 0){ // Ｃの場合は、一番外側の}で次モジュールとする。
				ModuleIdx++;
			}
			break;
		case tid_Kakko: // (
		case tid_KagiKakko: // [
			BrNestLevel++;
			break;
		case tid_Kokka: // )
		case tid_KagiKokka: // ]
			BrNestLevel--;
			break;
		default:
            break;
		}
	}
	//
	//	各トークンに合わせた処理。
	//
	for(i = 0 ; i < TokenList->Count ;){
		Token = (TToken *)TokenList->Items[i];
		switch(Token->TokId){
		case tid_null:
		case tid_Semikoron:	// ; セミコロン
			i++;
			continue;
		case tid_Comment:
			if(MainF->SrcCodeType != MainF->sctC){
				if((int)Token->Str.find("!") == 0){
					for( ; i < TokenList->Count ; i++){
						Token2 = (TToken *)TokenList->Items[i];
//						if(Token->LineNo != Token2->LineNo){
						if((Token2->TokId == tid_LineEnd)||(Token2->TokId == tid_Semikoron)){
							i++;
							break; // 行末トークンまでを行として扱う。継続行の場合は最後にセットされる。
						}
					}
				}else{
					i++;
				}
			}else{
				i++;
			}
			continue;
		case tid_SharpStart:
		case tid_SharpInclude:
		case tid_SharpDefine:
		case tid_SharpUnDef:
		case tid_SharpIf:
		case tid_SharpElse:
		case tid_SharpElIf:
		case tid_SharpEndIf:
		case tid_SharpIfDef:
		case tid_SharpIfnDef:
		case tid_SharpPragma:
		case tid_SharpEnd:
			for( ; i < TokenList->Count ; i++){
				Token2 = (TToken *)TokenList->Items[i];
				if((Token2->TokId == tid_LineEnd)||(Token2->TokId == tid_Semikoron)){
					i++;
					break; // 行末トークンまでを行として扱う。継続行の場合は最後にセットされる。
				}
			}
			continue;
		default:
			break;
		}
		//
		//	文を解析し、トークンを進める。
		//	解析結果は、変数の参照や関数の引数状態等としてトークンデータに
		//	格納され、以後の処理で参照される。
		//
		if(MainF->SrcCodeType != MainF->sctC){
			int ePos;

//			for(ePos = i+1 ; ePos < TokenList->Count ; ePos++){
			for(ePos = i ; ePos < TokenList->Count ; ePos++){
				Token2 = (TToken *)TokenList->Items[ePos];
//				if(Token->LineNo != Token2->LineNo){
				if((Token2->TokId == tid_LineEnd)||(Token2->TokId == tid_Semikoron)){
					ePos++;
					break; // 行末トークンまでを行として扱う。継続行の場合は最後にセットされる。
				}
			}
#if 0
			string ss = "";
			for(int j = i ; j < ePos ; j++){
				Token2 = (TToken *)TokenList->Items[j];
				ss += Token2->Str+" ";
			}
			MainF->print("["+ss+"]");
#endif
			Pass2_Fortran(TokenList,i,ePos,ValDataList);
			i = ePos;
		}else{
			i += Pass2_C(TokenList,i,ValDataList);
		}
	}
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    sPos    開始位置
//    ePos    終了位置
//    ValDataList 変数リスト
//
//  3.概要
//    構文解析を１行ごとに行う
//    sPosからePosのトークンを全て処理すること。(ePosは、LineEndを示す)
//    いくつかのまとまった処理は、サブルーチンとすること。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void Pass2_Fortran(TList *TokenList,int sPos,int ePos,TList *ValDataList)
//void Pass2_Line(TList *TokenList,int sPos,int ePos,TList *ValDataList)
{
	TToken *Token,*ValToken;
	int TokId;
	int UseTokLen;
	double dData;
	string TokStr;
	int ValIdx;
	TValData *ValData = NULL;
	int TmpRefCount;

	Token = (TToken *)TokenList->Items[sPos];
	TokId = Token->TokId;
	//
	//	行の先頭の行番号とラベルの処理を行う。
	//
	if(TokId == tid_Label){ // ラベルId自体がPass1で設定された場合。
		// ラベルを除いて、もう一度呼び出す。 (文でも可能なため)
		sPos++;
		Token = (TToken *)TokenList->Items[sPos];
		TokId = Token->TokId;
	}
	else if(TokId == tid_ConstInt){ // 行の先頭の数値は行番号
		Token->TokId = tid_Label;
		sPos++;
		Token = (TToken *)TokenList->Items[sPos];
		TokId = Token->TokId;
	}
	else if((TokId == tid_Val)&&(sPos+1 < ePos)&&
		(((TToken *)TokenList->Items[sPos+1])->TokId == tid_Koron)){
		// 行の先頭の 名前: でラベルになる。
		Token->TokId = tid_Label;
		sPos+=2;
		Token = (TToken *)TokenList->Items[sPos];
		TokId = Token->TokId;
	}

	switch(TokId){
//	case -1:
	case tid_Val:
		//
		// 代入文　先頭が未定義トークン (変数名）
		// キーワードと一致する DO などの変数名はサポートしない。
		// 行の先頭は、常に変数名となる。 名前の次が(なら配列。
		//
		ValToken = Token; // 変数トークン
		TokStr = Token->Str;
		//
		// 変数を vrf_setで追加
		//
		for(ValIdx = 0 ; ValIdx < ValDataList->Count ; ValIdx++){
			ValData = (TValData *)ValDataList->Items[ValIdx];
			if((LowerCase(ValData->Str) == LowerCase(TokStr))&&
				(ValData->ModuleIdx == Token->ModuleIdx)){
				Token->ValData = ValData; // トークンに変数を関連付ける
				ValData->SetCount++;
                ValIdx = ValDataList->Count+10;
				break;
			}
		}
		if(ValIdx != ValDataList->Count+10){
			// 最初に出現した変数
			ValData = new TValData(TokStr);
			ValDataList->Add((void *)ValData);
			ValData->ModuleIdx = Token->ModuleIdx;
			ValData->DataType = ImplType[(int)TokStr[0]]; // 暗黙の宣言
			Token->ValData = ValData; // トークンに変数を関連付ける
			ValData->SetCount++;
		}
		TmpRefCount = ValData->RefCount;
		Token = (TToken *)TokenList->Items[++sPos]; // 次のトークンを所得
		if(Token->TokId == tid_Kakko){
			// 配列　a(i,j) = など
			// 配列の添え字部分の解析を行ない TokenとValListをセット
			//
			ValData->ArrayOrFuncF = true;
			sPos++;	// skip "("
			do{
				UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
//				if(UseTokLen == -1){
				if(UseTokLen <= 0){
					break;
                }
				sPos += UseTokLen;
				Token = (TToken *)TokenList->Items[sPos]; // 次のトークンを所得
				if(Token->TokId == tid_Kannma){ // ","の場合は、次へ
					Token = (TToken *)TokenList->Items[++sPos];
                }
            }while(Token->TokId != tid_Kokka);
			sPos++; // Skip ")"
			Token = (TToken *)TokenList->Items[sPos];
		}
		if(Token->TokId != tid_Set){
			MainF->ErrMessage(sPos,"式に'='がありません。");
		}
		sPos++;	// Skip "="
		// 右辺部分の評価とトークンの処理を行う。
		UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
		if(TmpRefCount != ValData->RefCount){
			//
			// Val = の右辺で Valの参照があった場合は、
			// Val = Val + ... として扱っている。
			// if(TmpRefCount < ValData->RefCount) と同等
			//
			ValToken->RefType = vrf_refset;
		}else{
			ValToken->RefType = vrf_set;
		}
    	break;
    case tid_LineEnd:	// 行の終わり
		break;
 	case tid_null:
	case tid_Comment:	// コメント
	case tid_ConstStr:	// 文字列定数
	case tid_Label:		// 文番号、またはラベル (必要に応じて検索)
    	break;
	case tid_END:
		// モジュールを終了。（次行から次モジュールとなる）
		// Endの後の名前は、Skipする。
		if(Token->NestLevel == 0){
			ResetModule();
		}
		break;
	// Program関連
	case tid_PROGRAM:		// PROGRAM
	case tid_MODULE:		// MODULE
	case tid_INTERFACE:		// INTERFACE
		// とりあえずなし。現状では、サブの呼び出しは対象外
		break;
	case tid_SUBROUTINE:	// SUBROUTINE
	case tid_FUNCTION:		// FUNCTION
		AddSubFuncDef_Fortran(TokenList,sPos,ePos,ValDataList);
		break;
	case tid_CONTAINS:		// CONTAINS
 	case tid_USE:			// USE
		// とりあえずなし。現状では、サブの呼び出しは対象外
    	break;
    // データ宣言関連、変数(定数）定義をValDataListに追加する。
	case tid_INTEGER:
	case tid_REAL:
 	case tid_DOUBLEPRECISION:
 	case tid_CHARACTER:
	case tid_LOGICAL:
 	case tid_COMPLEX:
	case tid_DIMENSION:
 	case tid_PARAMETER:
    	//
	    // データ宣言関連、変数(定数）定義をValDataListに追加する。
    	// 変数のデータ型が必要となるため必須
	    // PARAMETERの定数処理も、式の評価を行って計算する。（可能な限り）
        //  Exp. REAL,PARAMETER ABC = 12*4 ; PARAMETER (N=3)
		//
		AddDataDef_Fortran(TokenList,sPos,ePos,ValDataList);
    	break;
 	case tid_IMPLICIT:  	// 暗黙の型宣言変更
		IMPLICIT(TokenList,sPos+1,ePos);
		break;
	case tid_TYPE:
		break;
	case tid_NAMELIST: // NameListは、READ,WRITEで使われる.
	case tid_ALLOCATABLE:
	case tid_POINTER:
	case tid_TARGET:
	case tid_SAVE:
	case tid_PRIVATE:
	case tid_PUBLIC:
	case tid_EXTERNAL:
	case tid_INTRINSIC:
	case tid_INTENT:
	case tid_OPTIONAL:
 	case tid_WHERE:
	//動的記憶割付け
 	case tid_ALLOCATE:
 	case tid_DEALLOCATE:
 	case tid_NULLIFY:
    	// すべて、読み飛ばす。（当面ポインタ、構造体は対象外とする）
        // ポインタ、構造体を含む部分は、スクリプトで変換不可とする。
    	break;
 	case tid_CALL:
    	// Callは、その次のサブルーチン名をSkipしてから
        // 後の変数を RWの属性をつけて設定する。（型の一致チェックなどは行わない）
#if 1   // Callの変数
		sPos++; // Skip サブルーチン名

		Token = (TToken *)TokenList->Items[++sPos]; // 次のトークンを所得
		if(Token->TokId == tid_Kakko){
        	// 配列　a(i,j) = など
            // 配列の添え字部分の解析を行ない TokenとValListをセット
            //
            sPos++;	// skip "("
            do{
				UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
//                if(UseTokLen == -1){
                if(UseTokLen <= 0){
                 	break;
                }
				sPos += UseTokLen;
				Token = (TToken *)TokenList->Items[sPos]; // 次のトークンを所得
				if(Token->TokId == tid_Kannma){ // ","の場合は、次へ
					Token = (TToken *)TokenList->Items[++sPos];
                }
            }while(Token->TokId != tid_Kokka);
            sPos++; // Skip ")"
			Token = (TToken *)TokenList->Items[sPos];
        }
#endif
    	break;
 	case tid_DO: // DO文
//		Token->JmpTokenPos = FindEndDo(TokenList,sPos);
		Token = (TToken *)TokenList->Items[++sPos]; // 次のトークンを所得
		if(Token->TokId == tid_ConstInt){ // 行番号使用のDO文 DO 800 I = 1,10 等
			Token = (TToken *)TokenList->Items[++sPos]; // 次のトークンを所得
		}
		if(Token->TokId != tid_Val){
			MainF->ErrMessage(sPos,"DO文の変数が見つかりません。");
		}
		ValToken = Token; // 変数トークン

		TokStr = Token->Str;
		//
		// 変数を vrf_setで追加
		//
		for(ValIdx = 0 ; ValIdx < ValDataList->Count ; ValIdx++){
			ValData = (TValData *)ValDataList->Items[ValIdx];
//			if((AnsiCompareText(ValData->Str,TokStr) == 0)&&
			if((LowerCase(ValData->Str) == LowerCase(TokStr))&&
				(ValData->ModuleIdx == Token->ModuleIdx)){
				Token->ValData = ValData; // トークンに変数を関連付ける
                ValData->SetCount++;
                ValIdx = ValDataList->Count+10;
				break;
            }
        }
		if(ValIdx != ValDataList->Count+10){
           	// 最初に出現した変数
        	ValData = new TValData(TokStr);
			ValDataList->Add((void *)ValData);
       	    ValData->ModuleIdx = Token->ModuleIdx;
			ValData->DataType = ImplType[(int)TokStr[0]]; // 暗黙の宣言
			Token->ValData = ValData; // トークンに変数を関連付ける
            ValData->SetCount++;
        }
		TmpRefCount = ValData->RefCount;
		Token = (TToken *)TokenList->Items[++sPos]; // 次のトークンを所得
		if(Token->TokId == tid_Kakko){
        	// 配列　a(i,j) = など
            // 配列の添え字部分の解析を行ない TokenとValListをセット
            //
		    ValData->ArrayOrFuncF = true;
            sPos++;	// skip "("
			do{
		       	UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
                if(UseTokLen == -1){
                 	break;
                }
				sPos += UseTokLen;
				Token = (TToken *)TokenList->Items[sPos]; // 次のトークンを所得
				if(Token->TokId == tid_Kannma){ // ","の場合は、次へ
					Token = (TToken *)TokenList->Items[++sPos];
                }
            }while(Token->TokId != tid_Kokka);
			sPos++; // Skip ")"
			Token = (TToken *)TokenList->Items[sPos];
        }
		if(Token->TokId != tid_Set){
			MainF->ErrMessage(sPos,"式に'='がありません。");
		}else{
	        sPos++; // Skip "="
            do{
				UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
                if(UseTokLen <= 0){
                 	break;
				}
				sPos += UseTokLen;
				Token = (TToken *)TokenList->Items[sPos]; // 次のトークンを所得
				if(Token->TokId == tid_Kannma){ // ","の場合は、次へ
					Token = (TToken *)TokenList->Items[++sPos];
                }
            }while(Token->TokId != tid_Kokka);
            sPos++; // Skip ")"
			Token = (TToken *)TokenList->Items[sPos];
        }
        //
        // 式の右辺の解析を行う。
        // 右辺が定数の場合は、Tokenに定数をセット！
        //
       	UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
		if(TmpRefCount != ValData->RefCount){
	        ValToken->RefType = vrf_refset;
        }else{
	        ValToken->RefType = vrf_set;
        }
    	break;

    // 制御文
 	case tid_IF:
    	//
        // IF文は、その後の変数を参照として追加する。
        //
		sPos++; // Skip IF
       	UseTokLen = Eval(TokenList,sPos,ePos,ValDataList,&dData);
       	break;
	case tid_SELECT:
	case tid_DOWHILE:
       	break;
 	case tid_THEN:
 	case tid_ELSE:
 	case tid_ELSEIF:
       	break;
 	case tid_ENDIF:
 	case tid_ENDDO:
       	break;
    case tid_CASE:
 	case tid_GOTO:
 	case tid_RETURN:
 	case tid_STOP:
       	break;
    // 入出力
 	case tid_READ:
 	case tid_WRITE:
	case tid_PRINT:
 	case tid_BACKSPACE:
 	case tid_INQUIRE:
 	case tid_REWIND:
 	case tid_CLOSE:
 	case tid_OPEN:
 	case tid_ENDFILE:
 	case tid_FORMAT:
		// すべて、読み飛ばす。
        // 入出力を含む部分は、スクリプトで変換不可とする。
    	break;
    // その他
 	case tid_ASSOCIATED:
 	case tid_CONTINUE:
 	case tid_CYCLE:
 	case tid_DATA:
 	case tid_EXIT:
    	// すべて、読み飛ばす。
        // 入出力を含む部分は、スクリプトで変換不可とする。
		break;
    }
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    sPos    開始位置
//    ValDataList 変数リスト
//
//  3.概要
//    構文解析を１文ごとに行う
//	  主な目的は、変数についての参照関係を含めた情報収集。
//    sPosからのトークンを処理し、使用したトークン数を返す。
//
//  4.機能説明
//
//  5.戻り値
//    使用したトークン数
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
int Pass2_C(TList *TokenList,int sPos,TList *ValDataList)
{
	TToken *Token;
	TToken *Token1;
	TToken *Token2;
	int TokId,TokId2;
	string TokStr;
	int ValIdx;
	TValData *ValData;
	int Nest;
	int i,j,ePos,StartPos;

	StartPos = sPos;
	//
	// 先頭の改行や区切りをSkipする。
	//
	for( ; sPos < TokenList->Count ; sPos++){
		Token = (TToken *)TokenList->Items[sPos];
		TokId = Token->TokId;
		if(TokId == tid_Semikoron){
			continue;
		}
		if(TokId == tid_Koron){
			continue;
		}
		if(TokId == tid_DaiKakko){
			continue;
		}
		if(TokId == tid_DaiKokka){
			continue;
		}
		if(TokId == tid_LineEnd){
			continue;
		}
		break;
	}

	//
	// 各文について、計算上での分割範囲での分割を行う。
	//	;: か {}までを範囲とする。
	// if(a==0){ c = 3 ; d = 4: } の場合は、
	// if(a==0) と c = 4 と d = 4 の分割となる。
	//
	for(ePos = sPos+1 ; ePos < TokenList->Count ; ePos++){
		Token = (TToken *)TokenList->Items[ePos];
		TokId = Token->TokId;
		if(TokId == tid_Semikoron){
			break;
		}
		if(TokId == tid_Koron){
			break;
		}
		if(TokId == tid_DaiKakko){
			break;
		}
		if(TokId == tid_DaiKokka){
			break;
		}
		if((Token->TokId >= tid_SharpStart)&&(Token->TokId <= tid_SharpEnd)){
			break;
		}
	}
	//
	//	文の先頭のトークンをでの処理を行う。
	//	int ,double 等の定義が先頭の場合は、定義として処理する。
	//	(int)は先頭でないので対象外。
	//
	Token = (TToken *)TokenList->Items[sPos];
	TokId = Token->TokId;
	TokStr = Token->Str;

	//
	// 先頭が#のマクロの処理
	//
	if((Token->TokId >= tid_SharpStart)&&(Token->TokId <= tid_SharpEnd)){
		//
		//	#define N 500 の対応。
		//
		if(Token->TokId == tid_SharpDefine){
			if(sPos+2 < TokenList->Count){
				Token1 = (TToken *)TokenList->Items[sPos+1];
				Token2 = (TToken *)TokenList->Items[sPos+2];
				if((Token1->TokId == tid_Val)&&(Token2->TokId == tid_ConstNum)){;
					TokStr = Token1->Str;
					ValIdx = GetValIdx(ValDataList,TokStr,Token->ModuleIdx);
					if(ValIdx == -1){
						ValData = new TValData(TokStr);
						ValIdx = ValDataList->Add((void *)ValData);
					}else{
						ValData = (TValData *)ValDataList->Items[ValIdx];
					}
					ValData->ModuleIdx = Token->ModuleIdx;
					ValData->BrNestLevel = Token->BrNestLevel;
					ValData->DataType = dt_Int;
					Token->ValData = ValData;
					ValData->ParamValF = true; // PARAMETER相当
					ValData->dData = atof(Token2->Str.c_str());
				}
			}
		}
		for(ePos = sPos+1 ; ePos < TokenList->Count ; ePos++){
			Token = (TToken *)TokenList->Items[ePos];
			TokId = Token->TokId;
			if(TokId == tid_LineEnd){
				break;
			}
		}
//		return ePos - sPos;
		return ePos - StartPos;
	}
	//
	//	関数と変数定義を追加する。
	//
	AddDataDef_C(TokenList,sPos,ePos,ValDataList);
#if 0
	//
	//	NestLevel = 0 での変数定義（グローバル）に対応 2011/07/27
	//
	if((Token->NestLevel == 0)&&(TokId == tid_Func)){
		//
		// 先頭が関数か変数の場合で、Nest=0ならば、int 定義となる。
		//
		AddSubFuncDef_C(TokenList,sPos,ePos,ValDataList);
	}else if(TokId == tid_Val){
		AddDataDef_C(TokenList,sPos,ePos,ValDataList);
	}else if((TokId == tid_int)||(TokId == tid_double)||(TokId == tid_char)||
		(TokId == tid_float)||(TokId == tid_void)||
		(TokId == tid_signed)||(TokId == tid_unsigned)||(TokId == tid_static)){
		//
		//	関数か配列定義。Nestは無関係。
		//
		for(i = sPos ; i < ePos ; i++){
			Token = (TToken *)TokenList->Items[i];
			TokId = Token->TokId;
			TokStr = Token->Str;
			if(TokId == tid_Val){
				AddDataDef_C(TokenList,sPos,ePos,ValDataList);
				break;
			}
			if(TokId == tid_Func){
				AddSubFuncDataDef_C(TokenList,sPos,ePos,ValDataList);
				break;
			}
		}
	}
	//
	//	変数を変数リストに追加する。
	//	配列の場合は、配列の次元も調べてセットする。
	//
	for(i = sPos ; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->TokId == tid_Val){	// 変数出現時は、変数リストに追加する。
			//
			// 名前の次が[なら配列。名前の前が * の場合も配列。
			//
			TokStr = Token->Str;
			//
			// 変数を名前とモジュールIdxで検索。
			// 最初の出現時、変数の後の [] を数えて、配列の次元を求める。
			// 通常は、定義での呼出が先なので、すでに型等は設定済み。。
			//
			ValIdx = GetValIdx(ValDataList,TokStr,Token->ModuleIdx);
			if(ValIdx == -1){	// Cの場合は、最初の出現は定義であること。
				ValData = new TValData(TokStr);
				ValDataList->Add((void *)ValData);
				ValData->ModuleIdx = Token->ModuleIdx;
				ValData->BrNestLevel = Token->BrNestLevel;
				ValData->SetCount = 0;
				Token->ValData = ValData;
				Nest = 0;
				ArrayLevel = 0;
				for(j = i+1 ; j < ePos ; j++){
					Token = (TToken *)TokenList->Items[j];
					TokId = Token->TokId;
					if(TokId == tid_KagiKakko){	// [
						Nest++;
						ArrayLevel++;
					}else if(TokId == tid_KagiKokka){ // ]
						Nest--;
					}else if(Nest == 0){
						break;
					}
				}
				for(j = i-1 ; j > 0 ; j--){
					Token = (TToken *)TokenList->Items[j];
					if(Token->Str == "*"){	// *
						ArrayLevel++;
					}else if(Nest == 0){
						break;
					}
				}
				ValData->ArrayLevel = ArrayLevel; // 配列次元数
			}else{
				ValData = (TValData *)ValDataList->Items[ValIdx];
				Token->ValData = ValData;
			}
			ValData->RefCount++;
		}
	}
#endif
	//
	//	＝、＋＝等を検索して、その右側の変数に参照をセットする。
	//		Add 2010/08/20
	//  = なしで、( が出現した場合も参照とする。
	//
	for(i = sPos ; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		if((TokId == tid_Set)||(TokId == tid_Set2)){
			// = や += などの出現位置から右側の変数に値の参照属性をセットする。
			Nest = 0;
			for(j = i+1; j < ePos ; j++){
				Token2 = (TToken *)TokenList->Items[j];
				TokId2 = Token2->TokId;

				if(TokId2 == tid_Val){
//					MainF->print("Ref["+Token2->Str+"]");
					Token2->RefType = vrf_ref;
					ValData = (TValData *)Token2->ValData;
					ValData->RefCount++;
				}
			}
			break;
		}
	}
	if(i == ePos){	// = , += 等がない場合は、変数はすべて参照で使用される。
		for(j = sPos; j < ePos ; j++){
		Token2 = (TToken *)TokenList->Items[j];
		TokId2 = Token2->TokId;

		if(TokId2 == tid_Val){
//			MainF->print("Ref["+Token2->Str+"]");
			Token2->RefType = vrf_ref;
			ValData = (TValData *)Token2->ValData;
			if(ValData != NULL){
                ValData->RefCount++;
			}
		}
	}

	}
	//
	//	＝、＋＝等を検索して、変数とトークンに設定ありかどうかを追加する。
	//
	for(i = ePos-1 ; i >= sPos ; i--){
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		if((TokId == tid_Set)||(TokId == tid_Set2)){
			// = や += などの出現位置から左側の変数に値の設定の属性をセットする。
			// だだし、[][]内の添え字の変数は対象外とする。
			Nest = 0;
			for(j = sPos ; j < i ; j++){
				Token2 = (TToken *)TokenList->Items[j];
				TokId2 = Token2->TokId;
				if(TokId2 == tid_KagiKakko){	// [
					Nest++;
				}else if(TokId2 == tid_KagiKokka){ // ]
					Nest--;
				}
				if((TokId2 == tid_Val)&&(Nest == 0)){
//					MainF->print("Set["+Token2->Str+"]");
					Token2->RefType = vrf_set;
					ValData = (TValData *)Token2->ValData;
					ValData->SetCount++;
//					if(TokId == tid_Set){
//						ValData->RefCount--;
//					}
				}
			}
			break;
		}
	}
//	return ePos-sPos;
	return ePos-StartPos;
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   処理するトークンリスト
//    sPos    TokenList上での開始位置
//    ePos    TokenList上での終了位置
//    ValDataList 変数データリスト
//
//  3.概要
//    SUBROUTINE , FUNCTIONの文（行）の処理を行う。
//    引数のValDataを生成して、 ValDataに ArgF=trueをセットする。
//
//  4.機能説明
//    SUBROUTINE , FUNCTIONの文（行）の処理を行う。
//    出現した変数名をValDataListに追加する。この時ValDataにArgF=trueをセットし
//    て、サブルーチンやＦｕｎｃｔｉｏｎの引数として変数が使われていることを設定
//    する。
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void AddSubFuncDef_Fortran(TList *TokenList,int sPos,int ePos,TList *ValDataList)
{
	int i,j;
	TToken *Token;
	int TokId;
	string TokStr,ValName;

//	for(i = sPos ; i <= ePos ; i++){
	for(i = sPos ; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
        TokStr = Token->Str;

        if(TokId == tid_LineEnd){	// １行終了
        	break;
        }
//        if(TokId != -1){	// 変数トークンのみが対象
		if(TokId != tid_Val){	// 変数トークンのみが対象
			continue;
        }
        //
        // すでに、同じMoudelIdxで、宣言されている同名変数があるかを調べる。
        // あれば、追加しない。（型の一致かどうかの判定は、コンパパイラまかせ）
        //
		TValData *ValData;
        bool NewValDataF = true;

        ValName = Trim(TokStr);
        for(j = 0 ; j < ValDataList->Count ; j++){
			ValData = (TValData *)ValDataList->Items[j];
//			if((AnsiCompareText(ValData->Str,ValName) == 0)&&
			if((LowerCase(ValData->Str) == LowerCase(ValName))&&
				(ValData->ModuleIdx == Token->ModuleIdx)){
	        	NewValDataF = false;
				break;
            }
		}
		//
        // 変数リストに追加する。
        //
		if(NewValDataF){
			ValData = new TValData(ValName);
			ValDataList->Add((void *)ValData);
			ValData->ModuleIdx = Token->ModuleIdx;
			ValData->DataType = ImplType[(int)ValName[0]]; // 暗黙の宣言
			ValData->DefPos = -1;
			ValData->ArgF = true;	// ArgFをセット
		}else{
			ValData->ArgF = true;	// ArgFをセット
		}
	}
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   処理するトークンリスト
//    sPos    TokenList上での開始位置
//    ePos    TokenList上での終了位置
//    ValDataList 変数データリスト
//
//  3.概要
//    intやdoubleなどのデータ宣言の処理を行ない、変数リストに変数と型を追加す
//    る。変数と関数の両方が対象となる。
//	　定義の順番に沿った解析を行う。元の定義文字列を保持する。
//
//  4.機能説明
//    intやdoubleなどのデータ宣言の処理を行ない、変数リストに変数と型を追加。
//    ValDataには、どのモジュールでの宣言かが、ModuleIdxかかから設定される。
//    変数には、ModuleIdxも保持して、ModuleIdxが異なる場合は別変数とする。
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
#if 1
void AddDataDef_C(TList *TokenList,int sPos,int ePos,TList *ValDataList)
{
	int i,j;
	TToken *Token,*Token2;
	int TokId;
	string TokStr,ValName;
	Enum_DataType DataType;
	int Nest,ArrayLevel;
	int DefPosS;
	string DefBaseStr,DefStr;
	bool SepF = false;
	bool NotValDefF = false;

	DefPosS = sPos;
	DataType = dt_Int;
	DefBaseStr = "";
	DefStr = "";
	Token = (TToken *)TokenList->Items[sPos];
	for(i = sPos ; i < ePos ; i++){
		if(i >= TokenList->Count){ // tid_null
			break;
		}
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		TokStr = Token->Str;

		switch(TokId){
		case tid_Val:
		case tid_Func:
			break;
		case tid_LineEnd:
		case tid_ConstNum:
			break;
		case tid_KagiKakko:
		case tid_KagiKokka:
			break;
		case tid_Kakko:
		case tid_Kokka:
			// Cの場合、（)が出現した場合は、それ以前をクリア。（簡略化）
			// Exp.  void func(int a); の voidはaの定義ではない。
			SepF = true;
			DefStr = "";
			break;
		case tid_Set:
		case tid_Set2:
		case tid_Comment:
			DefBaseStr = "";
			DefStr = "";
			break;
		case tid_Semikoron:	// ;
			NotValDefF = false; // 定義でない式フラグをクリア
			break;
		case tid_Kannma:	// 変数の区切り
			SepF = true;
			DefStr = "";
			break;
		case tid_Opr:
			if(TokStr == "*"){
				DefStr += Token->OrgStr;	// ポインタ
			}else{
				DefBaseStr = "";
				DefStr = "";
			}
			break;
		case tid_static:	// static は定義文字としては含めない。
		case tid_const:		// const は定義文字としては含めない。
			DefBaseStr = "";
			break;
		case tid_void:		// 定義
		case tid_int:
		case tid_double:
		case tid_char:
		case tid_float:
		case tid_struct:
			if(SepF){
				DefBaseStr = "";
			}
			DefBaseStr += Token->OrgStr;
			// 次のトークンが,()の場合は、クリアする。
			// Exp.  printf("%d",c); の "%d"は定義ではない。
			if(i+1 < TokenList->Count){
				Token2 = (TToken *)TokenList->Items[i+1];
				if((Token2->TokId == tid_Kannma)||
					(Token2->TokId == tid_Kakko)||
					(Token2->TokId == tid_Kokka)){
					DefBaseStr = "";
				}
			}
			break;
		default:
			if(SepF){
				DefBaseStr = "";
			}
			// 未定義の単語も定義用として追加。WORD等のマクロへの対抗
			DefBaseStr += Token->OrgStr;
			// 次のトークンが,()の場合は、クリアする。
			// Exp.  printf("%d",c); の "%d"は定義ではない。
			if(i+1 < TokenList->Count){
				Token2 = (TToken *)TokenList->Items[i+1];
				if((Token2->TokId == tid_Kannma)||
					(Token2->TokId == tid_Kakko)||
					(Token2->TokId == tid_Kokka)){
					DefBaseStr = "";
				}
			}
			break;
		}
		switch(TokId){
		case tid_void:
			DataType = dt_Void;
			break;
		case tid_int:
			DataType = dt_Int;
			break;
		case tid_double:
			DataType = dt_Real; // とりあえず、Fortan版に合わせる。
			break;
		case tid_char:
			DataType = dt_Char;
			break;
		case tid_float:
			DataType = dt_Float;
			break;
		case tid_struct:
			DataType = dt_Void;
			break;
		}
		if((TokId != tid_Val)&&(TokId != tid_Func)){
			// 変数と関数トークン以外次へ
			continue;
		}
		if(DefBaseStr == ""){
			// 定義文字なしでの変数や関数の出現は、式になるので、
			// セミコロンまで定義なしになる。
			NotValDefF = true;
		}
		//
		// すでに、同じ名前で、宣言されている関数か変数があるかを調べる。
		// あれば、追加しない。（型の一致かどうかの判定は、コンパイラまかせ）
		//
		TValData *ValData = NULL;
		bool NewValDataF = true;

		ValName = Trim(TokStr);

		if(Token->TokId == tid_Func){
			NewValDataF = false;     // 関数
		}
//		for(j = 0 ; j < ValDataList->Count ; j++){
		for(j = ValDataList->Count-1 ; j >= 0 ; j--){
			ValData = (TValData *)ValDataList->Items[j];
			if(ValData->Str.compare(ValName) == 0){
				if(Token->TokId == tid_Func){
					NewValDataF = false;     // 関数で2回目の出現
					break;
				}else if(Token->BrNestLevel == 0){
					NewValDataF = false;	// ネストなしでの2回目の出現
					break;
				}else if(ValData->ModuleIdx == Token->ModuleIdx){
					// 同じモジュール内での同じ名前の2回目の変数も定義済としている。
					NewValDataF = false;
					break;
				}else if((DefBaseStr == "")&&(ValData->BrNestLevel == 0)){
					// 定義でなく、グローバル変数を参照
					NewValDataF = false;
					break;
				}
			}
		}
#if 0
		if((NotValDefF)&&(Token->TokId != tid_Func)){
			// 定義なしで、変数出現の場合は、定義でない式とする。
			NewValDataF = false;
		}
#endif
		if((NotValDefF)&&(DefBaseStr == "")&&(Token->TokId != tid_Func)){
			// 定義なしで、変数出現の場合は、定義でない式とする。
			NewValDataF = false;
		}
		if(!NewValDataF){
			Token->ValData = ValData;
			continue;
		}
		//
		// 変数リストに追加する。
		//
		ValData = new TValData(ValName);
		Token->ValData = ValData;
		ValDataList->Add((void *)ValData);
		ValData->ModuleIdx = Token->ModuleIdx;
		ValData->BrNestLevel = Token->BrNestLevel;
		ValData->DataType = DataType;
		if(TokId == tid_Func){
			ValData->DataType = (Enum_DataType)(DataType+dt_IntFunc-dt_Int);
//		}else if(ValData->BrNestLevel > FuncBrNestLevel){
		}else if((Token->NestLevel == 0)&&(Token->BrNestLevel > 0)){
			// 関数定義 Func( xxx ) の引数の場合にArgFをセット
			ValData->ArgF = true;	// 引数であるArgFをセット
		}
		ValData->SetCount = 0;
		ValData->DefPos = i;	// 変数自体の位置
		ValData->DefStr1 = DefBaseStr;
		ValData->DefStr2 = DefStr;
		ValData->DefPosS = DefPosS; 	// 実体とは異なるがセット
		ValData->DefPosE = i;
		ValData->ArrayDefPosS = i+1;
		Token->ValData = ValData;
		Nest = 0;
		ArrayLevel = 0;
		for(j = i-1 ; j >= sPos ; j--){ // 変数の前の * をチェック
			Token = (TToken *)TokenList->Items[j];
			if(Token->Str == "*"){	// *
				ArrayLevel++;
			}else{
				break;
			}
		}
		for(j = i+1 ; j < ePos ; j++){ // 変数の後の [] をチェック
			Token = (TToken *)TokenList->Items[j];
			TokId = Token->TokId;
			if(TokId == tid_KagiKakko){	// [
				Nest++;
				ArrayLevel++;
			}else if(TokId == tid_KagiKokka){ // ]
				Nest--;
			}else if(Nest == 0){
				break;
			}
		}
		ValData->ArrayDefPosE = j;
		ValData->ArrayLevel = ArrayLevel; // 配列次元数
	}
}
#else
void AddDataDef_C(TList *TokenList,int sPos,int ePos,TList *ValDataList)
{
	int i,j;
	TToken *Token;
	int TokId;
	string TokStr,ValName;
	Enum_DataType DataType;
	int Nest,ArrayLevel;
	int DefPosS,DefPosE;
	string DefBaseStr,DefStr;
	int FuncBrNestLevel;
	bool SepF = false;

	DefPosS = sPos;
	DefPosE = sPos;
	DataType = dt_Int;
	DefBaseStr = "";
	DefStr = "";
	Token = (TToken *)TokenList->Items[sPos];
	FuncBrNestLevel = Token->BrNestLevel;
	for(i = sPos ; i < ePos ; i++){
		if(i >= TokenList->Count){ // tid_null
			break;
		}
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		TokStr = Token->Str;

		switch(TokId){
		case tid_Val:
		case tid_Func:
			break;
		case tid_KagiKakko:
		case tid_KagiKokka:
			break;
		case tid_Kakko:
		case tid_Kokka:
			DefBaseStr = "";
			DefStr = "";
			break;
		case tid_Kannma:	// 変数の区切り
			SepF = true;
			DefStr = "";
			break;
		case tid_Opr:
			if(TokStr == "*"){
				DefStr += Token->OrgStr;	// ポインタ
			}
			break;
		case tid_static:	// 定義
		case tid_void:
		case tid_int:
		case tid_double:
		case tid_char:
		case tid_float:
			if(SepF){
				DefBaseStr = "";
			}
			DefBaseStr += Token->OrgStr;
			break;
		default:
			if(SepF){
				DefBaseStr = "";
			}
			DefBaseStr += Token->OrgStr;
			break;
		}
		switch(TokId){
		case tid_void:
			DataType = dt_Void;
			break;
		case tid_int:
			DataType = dt_Int;
			break;
		case tid_double:
			DataType = dt_Real; // とりあえず、Fortan版に合わせる。
			break;
		case tid_char:
			DataType = dt_Char;
			break;
		case tid_float:
			DataType = dt_Float;
			break;
		}
		if((TokId != tid_Val)&&(TokId != tid_Func)){
			// 変数と関数トークン以外次へ
			continue;
		}
		//
		// すでに、同じ名前で、宣言されている関数か変数があるかを調べる。
		// あれば、追加しない。（型の一致かどうかの判定は、コンパイラまかせ）
		//
		TValData *ValData;
		bool NewValDataF = true;

		DefPosE = i;
		ValName = Trim(TokStr);
		for(j = 0 ; j < ValDataList->Count ; j++){
			ValData = (TValData *)ValDataList->Items[j];
			if(ValData->Str.compare(ValName) == 0){
				if(Token->TokId == tid_Func){
					NewValDataF = false;     // 関数で2回目の出現
					break;
				}else if(Token->BrNestLevel == 0){
					NewValDataF = false;	// ネストなしでの2回目の出現
					break;
				}else if(ValData->ModuleIdx == Token->ModuleIdx){
					// 同じモジュール内での同じ名前の2回目の変数も定義済としている。
					NewValDataF = false;
					break;
				}
			}
		}
		if(!NewValDataF){
			Token->ValData = ValData;
			continue;
		}
		//
		// 変数リストに追加する。
		//
		ValData = new TValData(ValName);
		Token->ValData = ValData;
		ValDataList->Add((void *)ValData);
		ValData->ModuleIdx = Token->ModuleIdx;
		ValData->BrNestLevel = Token->BrNestLevel;
		ValData->DataType = DataType;
		if(TokId == tid_Func){
			ValData->DataType = (Enum_DataType)(DataType+dt_IntFunc-dt_Int);
//		}else if(ValData->BrNestLevel > FuncBrNestLevel){
		}else if((Token->NestLevel == 0)&&(Token->BrNestLevel > 0)){
			// 関数定義 Func( xxx ) の引数の場合にArgFをセット
			ValData->ArgF = true;	// 引数であるArgFをセット
		}
		ValData->SetCount = 0;
		ValData->DefPos = i;	// 変数自体の位置
		ValData->DefStr1 = DefBaseStr;
		ValData->DefStr2 = DefStr;
		ValData->DefPosS = DefPosS; 	// 実体とは異なるがセット
		ValData->DefPosE = i;
		ValData->ArrayDefPosS = i+1;
		Token->ValData = ValData;
		Nest = 0;
		ArrayLevel = 0;
		for(j = i-1 ; j >= sPos ; j--){ // 変数の前の * をチェック
			Token = (TToken *)TokenList->Items[j];
			if(Token->Str == "*"){	// *
				ArrayLevel++;
			}else{
				break;
			}
		}
		for(j = i+1 ; j < ePos ; j++){
			Token = (TToken *)TokenList->Items[j];
			TokId = Token->TokId;
			if(TokId == tid_KagiKakko){	// [
				Nest++;
				ArrayLevel++;
			}else if(TokId == tid_KagiKokka){ // ]
				Nest--;
			}else if(Nest == 0){
				break;
			}
		}
		ValData->ArrayDefPosE = j;
		ValData->ArrayLevel = ArrayLevel; // 配列次元数
	}
}
#endif
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   処理するトークンリスト
//    sPos    TokenList上での開始位置
//    ePos    TokenList上での終了位置
//    ValDataList 変数データリスト
//
//  3.概要
//    INTEGERやREALなどのデータ宣言の処理を行ない、変数リストに変数と型を追加す
//    る。
//
//  4.機能説明
//    INTEGERやREALなどのデータ宣言の処理を行ない、変数リストに変数と型を追加す
//    る。
//    ValDataには、どのモジュールでの宣言かが、ModuleIdxから設定される。
//    PARAMETERを含む宣言の場合には、値の計算も行う。
//    変数には、ModuleIdxも保持して、ModuleIdxが異なる場合は別変数とする。
//    パラメータ以外の定数での代入文などは、変数として扱う。（分岐,COMMONなどが
//    ある）
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void AddDataDef_Fortran(TList *TokenList,int sPos,int ePos,TList *ValDataList)
{
	int i,j;
	TToken *Token;
	int TokId;
	string TokStr;
	bool IntF = false;
	bool RealF = false;
	bool CharF = false;
	bool LogicF = false;
	bool ComplexF = false;
	bool ParamaterF = false;
	bool DimensionF = false;
	int UseTokLen;
	double dData;
	int ArrayLevel = 0;
	int ArrayCount[100];
	int ArrayStart[100];
	int DimAttr_ArrayLevel = 0;		// Dimension属性でのArrayLevel(DimensionF=true)
	int DimAttr_ArrayCount[100];
	int DimAttr_ArrayStart[100];
	int DataLength = 0;
	bool ParamValF;
	double ParamData = 0;
	string ValName;
	int DefPos;
	int DefPosS,DefPosE;
	int ArrayDefPosS,ArrayDefPosE;

	ArrayDefPosS = ArrayDefPosE = -1;

	//
	// 宣言部分の処理
	//
	DefPosS = -1;
//	for(i = sPos ; i <= ePos ; i++){
	for(i = sPos ; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		TokStr = Token->Str;

		switch(TokId){	// 定義開始
		case tid_INTEGER:
		case tid_REAL:
		case tid_DOUBLEPRECISION:
		case tid_CHARACTER:
		case tid_LOGICAL:
		case tid_COMPLEX:
			if(DefPosS == -1){
				DefPosS = sPos;
			}
			break;
		case tid_DIMENSION:
			if(DefPosS == -1){
				DefPosS = sPos;
			}
			break;
		}
		switch(TokId){
		case tid_INTEGER:
			IntF = true;
			break;
		case tid_REAL:
			RealF = true;
			break;
		case tid_DOUBLEPRECISION:
			RealF = true;
			break;
		case tid_CHARACTER:
			CharF = true;
			break;
		case tid_LOGICAL:
			LogicF = true;
			break;
		case tid_COMPLEX:
			ComplexF = true;
			break;
		case tid_DIMENSION:
			//
			//	F90に対応するため、次が(の場合には、Dimension属性として扱う。
			//	(でない場合は、DIMENSION文となる。	2007/1/19
			//	また、定数でない場合もOK（デフォルトの値）とする。
			//
			if((i+1 < TokenList->Count)
				&&(((TToken *)TokenList->Items[i+1])->TokId == tid_Kakko)){
				int TokId2;
				string TokStr2;

				DimensionF = true;
				i+=2;
				//
				// )までを対象となり、Dim_ArrayLevelを求める。
				//
				//	Exp: DIMENSION(1:3,2) 等
				//
				while((i < ePos)&&(((TToken *)TokenList->Items[i])->TokId != tid_Kokka)){
					TokId2 = ((TToken *)TokenList->Items[i])->TokId;
					TokStr2 = ((TToken *)TokenList->Items[i])->Str;
					if(TokId2 == tid_Koron){	// 先頭なし　Exp. (:3)
						i++;
					}
					// 数値の処理
					UseTokLen = Eval(TokenList,i,ePos,ValDataList,&dData);
					if(!EvalConstF){
//						ErrMsg(TokenList,i,"宣言部エラー");
						dData = 0;
					}
					i += UseTokLen;
					TokId2 = ((TToken *)TokenList->Items[i])->TokId;
					TokStr2 = ((TToken *)TokenList->Items[i])->Str;
					if(TokId2 == tid_Koron){	// 先頭あり Exp (1:3)
						DimAttr_ArrayStart[DimAttr_ArrayLevel] = (int)dData;
						i++;
						TokId2 = ((TToken *)TokenList->Items[i])->TokId;
						TokStr2 = ((TToken *)TokenList->Items[i])->Str;
						UseTokLen = Eval(TokenList,i,ePos,ValDataList,&dData);
						if(!EvalConstF){
//							ErrMsg(TokenList,i,"宣言部エラー");
							dData = 0;
						}
						i += UseTokLen;
						DimAttr_ArrayCount[DimAttr_ArrayLevel] = (int)dData-
							DimAttr_ArrayStart[DimAttr_ArrayLevel] + 1;
					}else{
						DimAttr_ArrayStart[DimAttr_ArrayLevel] = 1;
						DimAttr_ArrayCount[DimAttr_ArrayLevel] = (int)dData;
					}
					TokId2 = ((TToken *)TokenList->Items[i])->TokId;
					TokStr2 = ((TToken *)TokenList->Items[i])->Str;
					if(TokId2 == tid_Kannma){
						DimAttr_ArrayLevel++;
					}else if(TokId2 == tid_Kokka){
						DimAttr_ArrayLevel++;
						break;
					}
					i++;
				}
			}
			break;
		case tid_PARAMETER:
			ParamaterF = true;
			break;
		case tid_INTENT: // ()をスキップする。 INTENT (OUT) で OUTを変数扱いしないため
			for(; i < ePos ; i++){
				Token = (TToken *)TokenList->Items[i];
				if(Token->TokId == tid_Kokka){ // 次の ) まで進める。
					break;
				}
			}
			break;
		case tid_ALLOCATABLE:	// その他の定義
		case tid_POINTER:
		case tid_TARGET:
		case tid_SAVE:
		case tid_PRIVATE:
		case tid_PUBLIC:
		case tid_EXTERNAL:
		case tid_INTRINSIC:
//		case tid_INTENT:
		case tid_OPTIONAL:
			break;
		case tid_Kannma:	// ,
			// ,は、ArrayLevelを Upする。
			ArrayLevel++;
			break;
		case tid_KoronKoron:	// ::
			// 宣言部分終了
			sPos = i+1;
			i = ePos+1;
			break;
		case tid_Kakko:	// (
			//
			// (Len=xx) や (xx,xx)で配列要素数を指定　')'までの処理を行う。
			// ただし、PARAMETERの直後は、()はパラメータの設定になる。
			//
			if(((TToken *)TokenList->Items[i-1])->TokId == tid_PARAMETER){
				// 宣言部分終了
				sPos = i+1;
				i = ePos+1;
			}else{
				// (Len=xx)か (xx,xx) 注:xxは、PARAMETERでの値も可能
				i++;
//				if((AnsiCompareText(((TToken *)TokenList->Items[i])->Str,"LEN") == 0)
				if((LowerCase(((TToken *)TokenList->Items[i])->Str) == "len")
					&&(((TToken *)TokenList->Items[i+1])->TokId == tid_Set)){
					// Len = の処理
					i++;
				}
//				if((AnsiCompareText(((TToken *)TokenList->Items[i])->Str,"KIND") == 0)
				if((LowerCase(((TToken *)TokenList->Items[i])->Str) == "kind")
					&&(((TToken *)TokenList->Items[i+1])->TokId == tid_Set)){
					// KIND = の処理
					i++;
				}
				// 数値の処理
				UseTokLen = Eval(TokenList,i,ePos,ValDataList,&dData);
				if(!EvalConstF){
//					ErrMsg(TokenList,i,"宣言部エラー");
					dData = 0;
				}
				i += UseTokLen;
				DataLength = (int)dData; // 文字列長さは、DataLength
			}
			break;
		default:
			if(TokStr == "*"){
				// REAL*8 や INTEGER*4 などの処理
				i++;
				Token = (TToken *)TokenList->Items[i];
				if(Token->TokId == tid_ConstInt){
					DataLength = (int)Token->dData; // 文字列長さは、DataLength
				}

			}else{
				// 宣言部分終了
				sPos = i;
				i = ePos+1;
			}
		}
		switch(TokId){	// 定義開始
		case tid_INTEGER:
		case tid_REAL:
		case tid_DOUBLEPRECISION:
		case tid_CHARACTER:
		case tid_LOGICAL:
		case tid_COMPLEX:
			// 宣言の次が ( の場合は、 )までは 種別(KIND)なのでスキップする。
			if((i+1< ePos)&&(((TToken *)TokenList->Items[i+1])->TokId == tid_Kakko)){
				int brLevel = 0;
				for(; i < ePos ; i++){
					Token = (TToken *)TokenList->Items[i];
					if(Token->TokId == tid_Kakko){
						brLevel++;
					}
					if(Token->TokId == tid_Kokka){
						brLevel--;
						if(brLevel == 0){
							break;
						}
					}
				}
			}
		}
	}
	DefPosE = sPos;
	//
	// 変数部分の処理
	// 変数の定義を追加する。宣言（PARAMETERを含む）のみが対象。
	//	もし、変数名のみで、DimensonF=treu(Dimension属性あり）の場合は、
	//	Dim_ArrayLevel等を使用する。 Add 2007/01/18
	//
//	for(i = sPos ; i <= ePos ; i++){
	for(i = sPos ; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		TokStr = Token->Str;

		if(TokId == tid_ContinuedLineStr){
			continue;
		}
		if(TokId != tid_Val){	// 変数トークンのみが対象
			break;
		}
		DefPos = i;
		ValName = TokStr;
		i++;
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		TokStr = Token->Str;
		ArrayLevel = 0;
		if(TokStr == "("){
			//
			// 配列宣言の添え字の処理
			// REAL D(3,4)
			//
			ArrayDefPosS = i;
			i++;
			while(true){
				UseTokLen = Eval(TokenList,i,ePos,ValDataList,&dData);
				if(!EvalConstF){
					i += UseTokLen;
					ArrayStart[ArrayLevel] = 1;
					ArrayCount[ArrayLevel] = 0;
				}else{
					i += UseTokLen;
					ArrayCount[ArrayLevel] = (int)dData;
					ArrayStart[ArrayLevel] = 1;
//					if(((TToken *)TokenList->Items[i+1])->TokId == tid_Koron){
					if(((TToken *)TokenList->Items[i])->TokId == tid_Koron){
						// : を含んだ宣言の処理
						i++;
						ArrayStart[ArrayLevel] = ArrayCount[ArrayLevel];
						UseTokLen = Eval(TokenList,i,ePos,ValDataList,&dData);
						if(!EvalConstF){
							i += UseTokLen;
							ArrayCount[ArrayLevel] = 0;
						}else{
							i += UseTokLen;
							ArrayCount[ArrayLevel] = (int)dData - ArrayStart[ArrayLevel]+1;
						}
					}
				}
				ArrayLevel++;
				Token = (TToken *)TokenList->Items[i];
				TokId = Token->TokId;
				TokStr = Token->Str;
				if(TokId != tid_Kannma){
					break;
				}
				i++;
			}
			ArrayDefPosE = i+1;
		}
		//
		//	もし、配列宣言なしの変数名のみで、Dimension属性ありの場合は、
		//	Dimension属性をセットする。
		//	Add 2007/01/19
		//
		if((ArrayLevel == 0)&&(DimensionF)){
			ArrayLevel = DimAttr_ArrayLevel;
			memcpy(ArrayStart,DimAttr_ArrayStart,sizeof(ArrayStart));
			memcpy(ArrayCount,DimAttr_ArrayCount,sizeof(ArrayCount));
		}
		//
		// パラメータ文の場合は、　xxx = xxx での値を所得する。
		// もしパラメータ文でない場合は、初期値なのでカンマまで読み飛ばす。
		//
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		ParamValF = false;
		if(TokId == tid_Set){
			UseTokLen = 0;
			if(ParamaterF){
				i++;
				UseTokLen = Eval(TokenList,i,ePos,ValDataList,&dData);
				if(EvalConstF){
					ParamValF = true;
					ParamData = dData;
				}
				i += UseTokLen;
			}
			if(UseTokLen <= 0){
//				for( ; i <= ePos ; i++){
				for( ; i < ePos ; i++){
					if(((TToken *)TokenList->Items[i])->TokId == tid_Kannma){
						break;
					}
				}
			}
		}
		//
		// すでに、同じMoudelIdxで、宣言されている同名変数があるかを調べる。
		// あれば、追加しない。（型の一致かどうかの判定は、コンパイラまかせ）
		//
		TValData *ValData;
		bool NewValDataF = true;

		for(j = 0 ; j < ValDataList->Count ; j++){
			ValData = (TValData *)ValDataList->Items[j];
//			if((AnsiCompareText(ValData->Str,ValName) == 0)&&
			if((LowerCase(ValData->Str) == LowerCase(ValName))&&
				(ValData->ModuleIdx == Token->ModuleIdx)){
	        	NewValDataF = false;
				break;
            }
        }
		//
        // 変数リストに追加する。
        // SUBRuntineなどで、先に出現した場合も行う
        //
        if(NewValDataF || (ValData->DefPos == -1)){
			if(NewValDataF){
	        	ValData = new TValData(ValName);
				ValDataList->Add((void *)ValData);
				ValData->ModuleIdx = Token->ModuleIdx;
            }
			ValData->DataType = ImplType[(int)ValName[0]]; // 暗黙の宣言
			ValData->DefPos = DefPos;
            ValData->DefPosS = DefPosS;
            ValData->DefPosE = DefPosE;
            ValData->ArrayDefPosS = ArrayDefPosS;
            ValData->ArrayDefPosE = ArrayDefPosE;
 		    if(RealF){
				ValData->DataType = dt_Real;
            }
			if(IntF){
				ValData->DataType = dt_Int;
            }
			if(CharF){
				ValData->DataType = dt_Char;
            }
			if(LogicF){
				ValData->DataType = dt_Logic;
            }
			if(ComplexF){
				ValData->DataType = dt_Complex;
            }
            ValData->DataLength = DataLength;
    		ValData->ArrayLevel = ArrayLevel;
            if(ArrayLevel != 0){
			    ValData->ArrayOrFuncF = true;
//             	ValData->ArrayStart = new int[ArrayLevel];
//             	ValData->ArrayCount = new int[ArrayLevel];
                for(int k = 0 ; k < ArrayLevel ; k++){
    	         	ValData->ArrayStart[k] = ArrayStart[k];
	             	ValData->ArrayCount[k] = ArrayCount[k];
                }
			}
        }
		if(ParamValF){
            ValData->ParamValF = ParamValF;
            ValData->dData = ParamData;
        }
		//
        // 次の変数へ
        //
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		if(TokId == tid_Kokka){
        	i++;
        }
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		if(TokId != tid_Kannma){
          	break;
        }
	}
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    sPos    開始位置
//
//  3.概要
//    （に対応する )の Posを返す。ネストに関しても処理される。
//    見つからない場合は、-1を返す。
//
//  4.機能説明
//
//  5.戻り値
//    対応する ()の Posを返す。見つからない場合は、-1を返す。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
int FindBrace(TList *TokenList,int sPos)
{
	int i;
    int Level = 0;
    TToken *Token;

    for(i = sPos ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->TokId == tid_Kakko){
			Level++;
		}else if(Token->TokId == tid_Kokka){
			Level--;
			if(Level == 0){
				return i;
			}
		}
	}
	return -1;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    aStr 変数名
//
//  3.概要
//    変数データ生成。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TValData::TValData(string aStr)
{
	Str = aStr;
	DataType = (Enum_DataType)-1;
	DataLength = 0;
	ParamValF = false;
	ArrayOrFuncF = false;
	ArgF = false; 		// SUBRUTION,FUNCTIONの引数として使用されている。
	ArrayLevel = 0;
	RefCount = 0;		// 変数参照Count
	SetCount = 0;		// 変数設定Count
	DefPos = -11;
	DefPosS = DefPosE = -1;    // 定義時のTokenPos A(N,N) などの範囲
	ArrayDefPosS = ArrayDefPosE = -1;    // 定義時のTokenPos A(N,N) などの範囲
	RefDoValBits = 0;
	DoValBits = 0;	// Add 2004/08/26

}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    変数データ破棄
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TValData::~TValData()
{
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    変数の情報を文字列で所得する。
//
//  4.機能説明
//
//  5.戻り値
//    変数情報の文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TValData::ToString()
{
	string s;
	string DataTypeStr[] = {"Int","Real","Char","Logic","Complex","Float","Void",
		"IntFunc","RealFunc","CharFunc","LogicFunc","ComplexFunc","FloatFunc",
		"VoidFunc"	};

	s = " (MIdx="+IntToStr(ModuleIdx)+") ";
	if(DataLength != 0){
		if(DataType == -1){
			s += (string)("??")+"*"+IntToStr(DataLength);
		}else{
			s += (string)(DataTypeStr[DataType])+"*"+IntToStr(DataLength);
		}
	}else{
		if(DataType == -1){
			s += (string)("??");
		}else{
			s += (string)(DataTypeStr[DataType]);
		}
	}
	s += " "+Str;
	for(int i = 0 ; i < ArrayLevel ; i++){
		s += "(";
		if(ArrayStart[i] != 1){
			s += IntToStr(ArrayStart[i])+":"+IntToStr(ArrayStart[i]+ArrayCount[i]);
		}else{
			s += IntToStr(ArrayCount[i]);
		}
		s += ")";
	}
	if(ParamValF){
		s += "="+FloatToStr(dData);
	}
	s += " (RW="+IntToStr(RefCount)+","+IntToStr(SetCount)+")";

	if(ArgF){
		s += "(Arg)";
	}

	return s;
/*
	char *DataTypeStr[] = {"Int","Real","Char","Logic","Complex",
		"IntFunc","RealFunc","CharFunc","LogicFunc","ComplexFunc"};

	s = " (MIdx="+IntToStr(ModuleIdx)+") ";
	s += "(Nest = "+IntToStr(BrNestLevel)+") ";
	s += "(Type = "+IntToStr(DataType)+") ";
	s += " "+Str;
	for(int i = 1 ; i < ArrayLevel ; i++){
		s += "[]";
	}
	if(ParamValF){
		s += "="+FloatToStr(dData);
	}
	s += " (RW="+IntToStr(RefCount)+","+IntToStr(SetCount)+")";

	return s;
*/
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    ValDataList  変数リスト
//    ValName 変数名
//    ModuleIdx モジュールインデックス
//
//  3.概要
//    名前とモジュールIdxに一致する変数番号を返す。
//
//  4.機能説明
//
//  5.戻り値
//    変数番号
//  6.備考
//
/*----------------------------------------------------------------------------*/
int GetValIdx(TList *ValDataList,string ValName,int ModuleIdx)
{
	int ValIdx;
	TValData *ValData;

	//
	//	同じMoudeIdxを探しなければ、グローバルを探す。 2011/07/28
	//
	for(ValIdx = 0 ; ValIdx < ValDataList->Count ; ValIdx++){
		ValData = (TValData *)ValDataList->Items[ValIdx];
		if((ValData->Str == ValName)&&
			(ValData->ModuleIdx == ModuleIdx)){
			return ValIdx;
		}
	}
	for(ValIdx = 0 ; ValIdx < ValDataList->Count ; ValIdx++){
		ValData = (TValData *)ValDataList->Items[ValIdx];
		if((ValData->Str == ValName)&&
			((ValData->ModuleIdx == ModuleIdx)||(ValData->BrNestLevel == 0))){
			return ValIdx;
		}
	}
	return -1;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    Fortran 各モジュールでの定義をリセットする。
//    最初のモジュールと、ＥＮＤの後で呼ばれる。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void ResetModule()
{
	for(int i = 0 ; i < 256 ; i++){
		ImplType[i] = dt_Real;
		if((i >= 'i')&&(i <= 'n')){
			ImplType[i] = dt_Int;
		}
		if((i >= 'I')&&(i <= 'N')){
			ImplType[i] = dt_Int;
		}
	}
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    sPos    開始位置
//    ePos    終了位置
//    ValDataList 変数データリスト
//    dData   結果データ格納先
//
//  3.概要
//    sPosからePosの式の解析と評価を実行する。
//    使用したトークンの数を返す。
//    定数として評価出来た場合（Parameter,Const）は、EvalConstF=trueを設定 。
//    Paramater文の定数の計算に使用される。
//
//    対象トークンは、 tid_Oprと数値（整数、実数）、数値、一部の関数。
//    関数の引数部分の解析も行われる。
//     = を含む代入文に関しての処理は、呼び出し側で行う。（右辺のみが対象）
//
//  4.機能説明
//
//  5.戻り値
//    使用したトークンの数を返す。
//
//  6.備考
//	  現在はFortranのParamater文用
//
/*----------------------------------------------------------------------------*/
int Eval(TList *TokenList,int sPos,int ePos,TList *ValDataList,double *dData)
{
	int i;
	TToken *Token,*LastTokData;
	int TokId,LastTokId;
	string TokStr,LastTokStr;
	int UseToken = 0;
	TList *DestList = new TList;
	TList *OpcList = new TList;
	int brLevel = 0;

	*dData = 0.0;
	LastTokId = -1;
	EvalConstF = false;
//	for(i = sPos ; i <= ePos ; i++){
	for(i = sPos ; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		TokId = Token->TokId;
		TokStr = Token->Str;

		switch(TokId){
		case tid_ContinuedLineStr:
			continue;
		case tid_ConstInt:	// 定数
		case tid_ConstReal:	// 定数
			DestList->Add((void *)Token);
			break;
		case tid_Opr:	// 演算子
		case tid_Koron:	// 部分配列用演算子 f90対応.2013/03/07
			//
			// 演算子の単項かどうかと優先順位を求める。
			// 前が空か、演算子か、（ならば単項となる。
			//
			if((i == sPos)||(LastTokId == tid_Opr)||(LastTokId == tid_Kakko)){
				// 単項演算子
				Token->priority = 10; // 優先度を変更
				OpcList->Add((void *)Token);
				break;
			}
			//
			// ２項演算子の処理（優先順位を求めること）
			//
			if(OpcList->Count > 0){
				LastTokData = (TToken *)OpcList->Items[OpcList->Count-1];
				LastTokStr = LastTokData->Str;
				if(LastTokData->TokId == tid_Kakko){
					OpcList->Add((void *)Token); // スタックに積む
				}else if((Token->priority <= LastTokData->priority)&&(TokStr == "**")){
					// ** 演算子は、右側結合
					OpcList->Add((void *)Token); // スタックに積む
				}else if(Token->priority < LastTokData->priority){
					OpcList->Add((void *)Token); // スタックに積む
				}else{
					while(OpcList->Count > 0){
						//
						//	スタックの内容を取り出す。
						//
						LastTokData = (TToken *)OpcList->Items[OpcList->Count-1];
						LastTokStr = LastTokData->Str;
						if(Token->priority < LastTokData->priority){
							if(Token->priority != 10){
								break;
							}else{
								// 単項演算子どおしの場合は、後方から
								if(Token->priority <= LastTokData->priority){
									break;
								}
							}
						}
						DestList->Add((void *)LastTokData);
						OpcList->Delete(OpcList->Count-1);
					}
					OpcList->Add((void *)Token); // スタックに積む
				}
			}else{
				OpcList->Add((void *)Token); // スタックに積む
			}
			break;
		case tid_Kakko:
			brLevel++;
			break;
		case tid_Kokka:	// Nest = 0の場合は終了となる。
			if(brLevel <= 0){
				// )まで解析があるので、エラーとはしない
				UseToken = i-sPos;
				i = ePos+1;
				break;
			}
			brLevel--;
			while(OpcList->Count > 0){
				LastTokData = (TToken *)OpcList->Items[OpcList->Count-1];
				LastTokStr = LastTokData->Str;
				if(LastTokData ->TokId == tid_Kakko){
					// 最後が（なら削除で終わり
					OpcList->Delete(OpcList->Count-1);
					break;
				}
				DestList->Add((void *)LastTokData);
				OpcList->Delete(OpcList->Count-1);
				if(OpcList->Count == 0){
					// )まで解析があるので、エラーとはしない
//					ErrMsg(TokenList,i,"対応する'('がありません。");
					break;
				}
			}
			break;
		case tid_Kannma:
			if(brLevel == 0){ // ()内以外の","は評価を終了する。
				UseToken = i-sPos;
				i = ePos+1;
			}
			break;
//		case tid_Koron:
		case tid_KoronKoron:
		case tid_Set:
		case tid_LineEnd:
			UseToken = i-sPos;
			i = ePos+1;
			break;
//		case -1:    // 変数・関数トークン
		case tid_Val:    // 変数・関数トークン
			//
			// 未定義トークン、PARAMETER変数でないかをチェックする
			// PARAMETER変数の場合は、定数と同じ扱いとする。
			// それ以外の場合は、終了する。
			// Nestが０の場合は、エラーとはしない。
            // Exp.  12*34*??? はエラー（スタックに残る）
            // Exp.  12*34 ??? はエラー（スタックに残らない）
            //
			int ValIdx;
            TValData *ValData;

		 	Token->RefType = vrf_ref; // 変数参照
            for(ValIdx = 0 ; ValIdx < ValDataList->Count ; ValIdx++){
            	// 有効なPARAMETER変数が見つかった場合には、
				// その値をToken->dDataにセットして抜ける
				ValData = (TValData *)ValDataList->Items[ValIdx];
				if((LowerCase(ValData->Str) == LowerCase(TokStr))&&
					(ValData->ModuleIdx == Token->ModuleIdx)){
                    Token->ValData = ValData; // トークンに変数を関連付ける
                    ValData->RefCount++;
	                Token->dData = ValData->dData;
		            DestList->Add((void *)Token);
                    ValIdx = ValDataList->Count+10;
					break;
                }
            }
			if(ValIdx != ValDataList->Count+10){
				// 最初に出現した変数
                // 本来は、エラー（警告）だが、変数として追加する形としてある。
	        	ValData = new TValData(TokStr);
    	        ValDataList->Add((void *)ValData);
        	    ValData->ModuleIdx = Token->ModuleIdx;
				ValData->DataType = ImplType[(int)TokStr[0]]; // 暗黙の宣言
				Token->ValData = ValData; // トークンに変数を関連付ける
                ValData->RefCount++;
                Token->dData = ValData->dData;
	            DestList->Add((void *)Token);

                // 次が(で 定義が以前にない場合は、関数
				Token = (TToken *)TokenList->Items[i+1];
    	        if(Token->TokId == tid_Kakko){
					ValData->ArrayOrFuncF = true;
					ValData->DataType = (Enum_DataType)(ImplType[(int)TokStr[0]] + (int)dt_IntFunc);
				}
            }
			break;
		default:	// その他のトークン then
			UseToken = i-sPos;
			i = ePos+1;
			break;
        }
		LastTokId = TokId;
    }
    //
    //  スタック上に残ったトークンを掃き出す
    //
	while(OpcList->Count != 0){
        Token = (TToken *)OpcList->Items[OpcList->Count-1];
		TokStr = Token->Str;
		if(Token->TokId == tid_Kakko){
			MainF->ErrMessage(i,"対応する')'がありません。");
		}
        DestList->Add((void *)Token);
        OpcList->Delete(OpcList->Count-1);
    }
    //
    // 実際の演算を実行する。
    // 型としては、 Int,Realの２種類だけをサポートする。
    // 論理型、複素数、文字型は、エラーとして扱う。
	//
	double stack[1024];
	double IntF[1024];
    int sp = 0;

    EvalConstF = true;
	for(i = 0 ; i < DestList->Count ; i++){
	    if(!EvalConstF){
			break;
        }
        Token = (TToken *)DestList->Items[i];
        TokStr = Token->Str;
        switch(Token->TokId){
        case tid_ConstInt:	// 定数
            IntF[sp] = true;
    		stack[sp++] = Token->dData;
			break;
        case tid_ConstReal:	// 定数
            IntF[sp] = false;
    		stack[sp++] = Token->dData;
            break;
        case tid_Opr:
			if(Token->priority == 10){ // 単項演算子
            	if(sp < 1){
                 	UseToken = -1;
                    break;
                }
				if(TokStr == "-"){
					stack[sp-1] = -stack[sp-1];
				}
            	break;
            }
            //
            // ２項演算子
            //
           	if(sp < 2){
              	UseToken = -1;
                break;
            }
			if(TokStr == "+"){
	    		stack[sp-2] = stack[sp-2] + stack[sp-1];
			}else if(TokStr == "-"){
				stack[sp-2] = stack[sp-2] - stack[sp-1];
			}else if(TokStr == "*"){
	    		stack[sp-2] = stack[sp-2] * stack[sp-1];
			}else if(TokStr == "/"){
				if(stack[sp-1] == 0){	// 0除算エラー
			        EvalConstF = false;
                }else{
		    		stack[sp-2] = stack[sp-2] / stack[sp-1];
                }
			}else if(TokStr == "**"){
	    		stack[sp-2] = pow(stack[sp-2],stack[sp-1]);
            }
        	if(IntF[sp-2] && IntF[sp-1]){
				stack[sp-2] = (int)stack[sp-2];
            }else{
				IntF[sp-2] = false;
            }
            sp--;
        	break;
        default:	// パラメータ変数の処理
            if((Token->ValData == NULL)||(!((TValData *)Token->ValData)->ParamValF)){
			    EvalConstF = false;
            }else{
	            if(((TValData *)Token->ValData)->DataType == dt_Int){
		            IntF[sp] = true;
        	    }else{
	        	    IntF[sp] = false;
	            }
            }
            stack[sp++] = Token->dData;
        	break;
        }
    }
	if(sp != 1){
	    EvalConstF = false;
	}
	*dData = stack[0]; // 最後のスタックが値となる。

	delete DestList;
	delete OpcList;
    return UseToken;
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    sPos    開始位置
//    ePos    終了位置
//
//  3.概要
//    暗黙の型宣言の変更
//    暗黙の型宣言の変更に伴って、暗黙の型宣言テーブルの内容を変更する。
//
//     　IMPLICIT NONE、
//     　IMPLICIT 型指定子(x1,x2-x3,…)
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void IMPLICIT(TList *TokenList,int sPos,int ePos)
{
	TToken *Token;
	int i,j;
	int Nest;
	char c,c2;
	Enum_DataType Type;

	Nest = 0;
	c = 0;
	Type = dt_Real;
//	for(i = sPos; i <= ePos ; i++){
	for(i = sPos; i < ePos ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->TokId == tid_INTEGER){
			Type = dt_Int;
		}else if(Token->TokId == tid_REAL){
			Type = dt_Real;
		}else if(Token->TokId == tid_DOUBLEPRECISION){
			Type = dt_Real;
		}else if(Token->TokId == tid_CHARACTER){
			Type = dt_Char;
		}else if(Token->TokId == tid_LOGICAL){
			Type = dt_Logic;
		}else if(Token->TokId == tid_COMPLEX){
			Type = dt_Complex;
		}
		if(Token->TokId == tid_Kakko){
			Nest++;
        }else if(Token->TokId == tid_Kokka){
         	Nest--;
        }else if(Token->Str == "-"){
         	Nest--;
			Token = (TToken *)TokenList->Items[++i];
			c2 = LowerCase(Token->Str)[0];
			if((c2 < 'a')||(c2 > 'z')){
				c2 = 'z';
            }
           	for(j = c ; j <= c2 ; j++){
				ImplType[j] = Type;
				ImplType[j+'A'-'a'] = Type;
			}
		}else if(LowerCase(Token->Str) == "none"){
			break;
		}else if(Token->Str == ","){
			c = 0;
		}else{
			c = LowerCase(Token->Str)[0];
			ImplType[(int)c] = Type;
			ImplType[(int)c+'A'-'a'] = Type;
        }
    }
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    変数の定義文字列を返す。Fortran言語用
//
//  4.機能説明
//
//  5.戻り値
//    定義文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TValData::GetDefStr_Fortran()
{
	int j;
	string s;
	TToken *Token;

	s = "";
	if(DefStr1 != ""){
		s = Trim(DefStr1 + DefStr2);
		// 変数が最初に出現する位置 DefPosから前にスキャンして、
		// 変数や関数以外が現れる前に改行があれば、改行＋空白を含んだDefStr1を使用し
		// 元のソースに近い形にする。
		for(j = DefPos-1 ; j >= 0 ; j--){
			Token = (TToken *)MainF->TokenList->Items[j];
			if((Token->TokId == tid_Val)||(Token->TokId == tid_Func)){
				break;
			}
			if(Token->TokId == tid_LineEnd){
				if(DefStr1.find('\n') == string::npos){
					s = "\n"+ DefStr1+Trim(DefStr2);
				}else{
					s = DefStr1+Trim(DefStr2);
				}
			}
		}
		if((s == "")||(s[s.length()-1] != '*')){
			s = s+ " ";
		}
	}else if(DefPosS != -1){
		for(j = DefPosS ; j < DefPosE ; j++){
			Token = (TToken *)MainF->TokenList->Items[j];
			if((Token->TokId == tid_Kannma)&&(j+1 < DefPosE)&&
				(((TToken *)MainF->TokenList->Items[j+1])->TokId == tid_SAVE)){
				// , Save はスキップする。
				j++;
				continue;
			}
			s += Token->OrgStr;
		}
	}else{
		// 定義文字がない場合は、暗黙の了解として作成して返す。
		// 先頭の空白は、６文字（デフォルト）とする。
		switch(DataType){
		case dt_Int:
			s += "integer";
			break;
		case dt_Real:
			s += "real";
			break;
		case dt_Char:
			s += "character";
			break;
		case dt_Logic:
			s += "logical";
			break;
		case dt_Complex:
			s += "complex";
			break;
		default:
			return "";
		}
		s = "      "+s;
	}
	return s;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    変数の定義文字列を返す。C言語用
//
//  4.機能説明
//
//  5.戻り値
//    定義文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TValData::GetDefStr_C()
{
	int j;
	string s,s2;
	TToken *Token;

	s = "";
	if(DefStr1 != ""){
		s = Trim(DefStr1 + DefStr2);
		// 変数が最初に出現する位置 DefPosから前にスキャンして、
		// 変数や関数以外が現れる前に改行があれば、改行＋空白を含んだDefStr1を使用し
		// 元のソースに近い形にする。
		for(j = DefPos-1 ; j >= 0 ; j--){
			Token = (TToken *)MainF->TokenList->Items[j];
			if((Token->TokId == tid_Val)||(Token->TokId == tid_Func)){
				break;
			}
			if(Token->TokId == tid_LineEnd){
				if(DefStr1.find('\n') == string::npos){
					s = "\n"+ DefStr1+Trim(DefStr2);
				}else{
					s = DefStr1+Trim(DefStr2);
				}
			}
		}
		if((s == "")||(s[s.length()-1] != '*')){
			s = s+ " ";
		}
	}else{
		// 定義文字がない場合は作成して返す。一時変数等で使用される。
		switch(DataType){
		case dt_Int:
			s += "int";
			break;
		case dt_Real:
			s += "double";
			break;
		case dt_Char:
			s += "char ";
			break;
		case dt_Logic:
			s += "logical";
			break;
		case dt_Complex:
			s += "complex";
			break;
		case dt_Float:
			s += "float";
			break;
		default:
			return "int";
		}
		s = s+" ";
	}
	return s;
}


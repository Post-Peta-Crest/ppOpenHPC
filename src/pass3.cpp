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
//    パス３
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "pass3.h"

void ParseScript(TList *TokenList,int sPos,string TokStr);
TScValData *GetScValData(TList *TokenList,string ValName);

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    ValDataList 変数リスト
//
//  3.概要
//    スクリプトの解析を行ない、Token->Scriptにデータをセットする。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void Pass3(TList *TokenList,TList *ValDataList)
{
	int i;
	TToken *Token;
	TToken *Token2;
	TScript *Script = NULL;
	TScript *LastScript = NULL;
	string s,s1;
	int pos;

	if(MainF->SrcCodeType != MainF->sctC){
		for(i = 0 ; i < TokenList->Count ; i++){
			Token = (TToken *)TokenList->Items[i];
			if(Token->TokId == tid_Comment){ // Fortran
				s = Trim(Token->Str);
				pos = s.find(' ');
				if(pos >= 0){
					s = Trim(s.substr(0,pos));
				}
				if(LowerCase(s) == "!oat$"){
					Script = new TScript(TokenList,i,LastScript,ValDataList);
					if(Script->ScType != sct_None){
						LastScript = Script;
					}
					Token->Script = Script;
				}
			}else if(Token->TokId != tid_LineEnd){
				LastScript = NULL;
			}
		}
	}else{ // C
		for(i = 0 ; i < TokenList->Count-1 ; i++){
			Token = (TToken *)TokenList->Items[i];
			if(Token->TokId == tid_SharpPragma){
				Token2 = (TToken *)TokenList->Items[i+1];
				if(LowerCase(Token2->Str) == "oat"){
					Script = new TScript(TokenList,i+1,LastScript,ValDataList);
					if(Script->ScType != sct_None){
						LastScript = Script;
					}
					Token->Script = Script;
				}
			}
		}
	}
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    sPos    トークンリスト開始位置
//
//  3.概要
//    スクリプトデータを生成する。スクリプトの種類に応じた処理が行われる。
//    必要な場合は、スクリプト変数生成も行われる。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TScript::TScript(TList *TokenList,int sPos,TScript *LastScript,TList *ValDataList)
{
	string s,s1,s2;
	int i;
	TToken *Token;
	TokStrList = new TStringList;

	PreviousScript = LastScript;
	if(LastScript != NULL){
		LastScript->NextScript = this;
/*
		//	１つ前のスクリプトの継続かどうかをチェックする。
		if(LastScript->LineEndTokPos != sPos-2){ // LineEnd + #Pragma
			LastScript = NULL; // 継続ではない。
		}
*/
	}
/*
	if(MainF->SrcCodeType == MainF->sctFortran){	// Fortranの場合はコメント１行
		i = sPos;
		Token = (TToken *)TokenList->Items[i];
		Str = Token->Str;
		LineEndTokPos = i;	// スクリプトの改行トークン位置
	}else{
*/
	TokPos = sPos;
	Str = ""; // １行の文字列
	for(i = sPos ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->TokId == tid_LineEnd){
			break;
		}
		// スクリプト内のトークンは、コメント扱いとする。
		// OAT以外でのコメントは、Pass1ですでにコメント扱い。
		Token->TokId = tid_Comment;
		Str += Token->OrgStr;
	}
	LineEndTokPos = i;	// スクリプトの改行トークン位置

	Token = (TToken *)TokenList->Items[sPos];

	ScValDataList = NULL;
	TuneRegion = NULL;
	ScType = sct_install;
	ScAction = sca_none;
	ScRegion = scr_none;

	// Fitting関連のデータ
	FittingType = 0;
	FittingDegree = 0;		// 多項式の次数
	SampledList = NULL;		// intの形で Sampledの値がセット

//d-spline用追加部分
	FittingDspline = 0;
	FittingDynamic = 0;
//d-spline用追加部分ここまで

	//
	// トークンに分解する。
	// Scriptでのセパレータは、通常より少ない。
	// これは、least-squares などを１トークンとして扱うため。
	// ただし、一部のScriptについては、元を参照したいために同じToken並びとする。
	//
	s1 = "";
	s = Str;
	for(i = 1 ; i <= (int)s.length() ; i++){
		if((s[i] == ' ')||(s[i] == '(')||(s[i] == ')')||(s[i] == ',')){
			s1 = Trim(s1);
			if(s1 != ""){
				TokStrList->Add(s1);
			}
			s1 = s[i];
			if(s1 != " "){
				TokStrList->Add(s1);
			}
            s1 = "";
        }else{
            s1 += s[i];
        }
	}
	s1 = Trim(s1);
	if(s1 != ""){
		TokStrList->Add(s1);
	}
/*
	for(i = 0 ; i < TokStrList->Count ; i++){
		MainF->print(TokStrList->Strings[i]);
	}

*/
	// スクリプトトークン解析
	i = 0;
	i++; // Skip "!OAT$" or "OAT"
	if(i >= TokStrList->Count){
		return;
	}
	s = LowerCase(TokStrList->Strings[i++]);
	if(i > TokStrList->Count){
		ScType = sct_None;
//		TokStrList->Clear();
		return;
	}
	if(i == TokStrList->Count){
		i--; // もし最後の場合は、１つ戻して参照エラーを回避する。
	}
	if(s == "call"){
		//
		// #pragma ABCLib call ABCLib_BPset("i") を
		// #pragma ABCLib BPset("i") に変更する。 Mod 2011/09/05
		//	非均質計算機対応（平成23/7/20）の対応
		//
		s2 = LowerCase(TokStrList->Strings[i]);
		if(s2 == "oat_bpset"){
			s = "bpset";
			i+=1;
		}else if(s2 == "oat_setparm"){
			s = "setparm";
			i+=1;
		}else if(s2 == "oat_atexec"){	// call OAT_ATexec() 引数がセットされる。
			s = "atexec";
			i+=1;
		}
	}
	if(s == "install"){
		ScType = sct_install;  	// 自動チューニング種類
	}else if(s == "static"){
		ScType = sct_static;  	// 自動チューニング種類
	}else if(s == "dynamic"){
		ScType = sct_dynamic;  	// 自動チューニング種類
	}else if(s == "name"){
		ScType = sct_name;  	// 自動チューニング種類
	}else if(s == "parameter"){
		ScType = sct_parameter;
	}else if(s == "select"){
		ScType = sct_select;
	}else if(s == "according"){
		ScType = sct_according;
	}else if(s == "varied"){
		ScType = sct_varied;
	}else if(s == "fitting"){
		ScType = sct_fitting;
	}else if(s == "pripro"){	// Mod 2011/08/03
		ScType = sct_prepro;
	}else if(s == "prepro"){
		ScType = sct_prepro;
	}else if(s == "postpro"){
		ScType = sct_postpro;
	}else if(s == "debug"){
		ScType = sct_debug;
	}else if(s == "oat_dynpefthis"){
		ScType = sct_DynPefThis;
	}else if(s == "number"){
		ScType = sct_Number;
	}else if(s == "bpset"){
		ScType = sct_BPset;
	}else if(s == "allocate"){
		ScType = sct_Allocate;
	}else if(s == "pre"){
		s2 = LowerCase(TokStrList->Strings[i]);
		if(s2 == "bpset"){
			ScType = sct_pre_BPset;
			i++;
		}
	}else if(s == "post"){
		s2 = LowerCase(TokStrList->Strings[i]);
		if(s2 == "bpset"){
			ScType = sct_post_BPset;
			i++;
		}
	}else if(s == "setparm"){
		ScType = sct_SetParm;
		if(MainF->Call_SetParam_Script == NULL){
			MainF->Call_SetParam_Script = this;
		}
	}else if(s == "splitpoint"){
		ScType = sct_SplitPoint;
	}else if(s == "splitpointcopydef"){
		ScType = sct_SplitPointCopyDef;
	}else if(s == "splitpointcopyinsert"){
		ScType = sct_SplitPointCopyInsert;
	}else if(s == "rotationorder"){
		ScType = sct_RotationOrder;
	}else if(s == "bind"){
		ScType = sct_Bind;
	}else if(s == "list"){	// Add 2016/02/26
		ScType = sct_List;
	}else if(s == "variedd"){
		ScType = sct_variedD;
	}else if(s == "listd"){
		ScType = sct_ListD;
	}else if(s == "replace"){
		ScType = sct_Replace;
	}else if(s == "target"){
		ScType = sct_Target;
	}else if(s == "gwv"){
		ScType = sct_GWV;
	}else if(s == "gwv-list"){
		ScType = sct_GWV_list;
	}else if(s == "gwv-target"){
		ScType = sct_GWV_target;
	}else if(s == "atexec"){
		if(MainF->Call_ATExec_Script == NULL){
			MainF->Call_ATExec_Script = this;
		}
		ScType = sct_ATExec;
		TokStrList->Clear();
		// ATExec()については、引数リスト参照があるので、ValData参照可能なトークンを使用する。
		for(int j = sPos+1 ; j < LineEndTokPos ; j++){
			Token = (TToken *)TokenList->Items[j];
			TokStrList->AddObject(Token->OrgStr,Token);
		}
		//
		//	atexec内のデータについては、変数名を探してValDataを設定する。
		//
		AddDataDef_C(MainF->TokenList,sPos+1,LineEndTokPos,ValDataList);
//	}else if((LastScript != NULL)&&(LastScript->ScType == sct_ATExec)){ // 継続
	}else if((LastScript != NULL)&&(LastScript->CheckBrBalance() != 0)){ // 継続
		// １つ前のスクリプトがATExecで今回が指定なしの場合は、前のトークンに追加する。
		Token = (TToken *)TokenList->Items[LastScript->LineEndTokPos];
		LastScript->TokStrList->AddObject(Token->OrgStr,Token);
//			MainF->print("["+Token->Str+"]");
		for(int j = sPos+1 ; j < LineEndTokPos ; j++){
			Token = (TToken *)TokenList->Items[j];
//			MainF->print("["+Token->Str+"]");
			LastScript->TokStrList->AddObject(Token->OrgStr,Token);
		}
		AddDataDef_C(MainF->TokenList,sPos+1,LineEndTokPos,ValDataList);
		ScType = sct_None;
		TokStrList->Clear();
	}else if(s == "option"){	// スクリプト Option Add 2016/02/29
		ScType = sct_Option;
	}else{ // 式
		ScType = sct_command;  	// 自動チューニング種類
		//
		// コマンドの場合は、TokStrListを１つだけに修正する。
		// インテントを有効にするために OrgStr側を参照。
		//
		s = "";
		for(i = TokPos+1 ; i < LineEndTokPos ; i++){
			Token = (TToken *)TokenList->Items[i];
			s += Token->OrgStr;
		}
		TokStrList->Clear();
		s = "      "+s; // Fortanと合わせて、空白を前に入れる。
		TokStrList->Add(s);
		return;
/*
		s = Str;
		if(MainF->SrcCodeType != MainF->sctC){
			s.erase(0,6);	// Delete "!OAT$ "
		}else{
			s.erase(0,4);	// Delete "OAT "
		}
		s = "      "+Trim(s); // Fortanと合わせて、空白を前に入れる。
*/
	}
	//
	//	ListからList_Endまでのスクリプトについては、元のTokenListと同じ参照にする。2016/02/29
	//
	if((ScType >= sct_List)&&(ScType <= sct_List_End)){
		TokStrList->Clear();
		for(int j = sPos+1 ; j <= LineEndTokPos ; j++){
			Token = (TToken *)TokenList->Items[j];
			TokStrList->AddObject(Token->OrgStr,Token);
		}
	}
	// 機能をチェック
	if(i >= TokStrList->Count){
		return;
	}
	if(ScType == sct_fitting){
		GetFittingParam(i);
		return;
	}
	s = LowerCase(TokStrList->Strings[i++]);
	if(s == "define"){
		ScAction = sca_define;
	}else if(s == "variable"){
		ScAction = sca_variable;
	}else if(s == "select"){
		ScAction = sca_select;
	}else if(s == "unroll"){
		ScAction = sca_unroll;
	}else if(s == "loopfusion"){
		ScAction = sca_loopfusion;
	}else if(s == "loopsplit"){
		ScAction = sca_loopsplit;
	}else if(s == "loopfusionsplit"){
		ScAction = sca_loopfusionsplit;
	}else if(s == "rotationorder"){
		ScAction = sca_rotationorder;
	}else if((s == "atexec")&&(ScType == sct_Bind)){
		ScAction = sca_none;
		s = LowerCase(TokStrList->Strings[i++]);
		if(s == "arguments"){
			s = LowerCase(TokStrList->Strings[i++]);
			if(s == "start"){
				ScAction = sca_bind_atexec_arg_start;
			}
			else if(s == "end"){
				ScAction = sca_bind_atexec_arg_end;
				GetBindArgument(TokenList,sPos);
			}
		}
	}else if(s == "list"){	// Add 2016/02/26
		ScAction = sca_list;
	}else if(s == "variabled"){
		ScAction = sca_variableD;
	}else if(s == "listd"){
		ScAction = sca_listD;
	}else if(s == "replace"){
		ScAction = sca_replace;
	}else if(s == "target"){
		ScAction = sca_target;
	}else if(s == "gwv"){
		ScAction = sca_GWV;
	}else if(s == "gwv-list"){
		ScAction = sca_GWV_list;
	}else if(s == "gwv-target"){
		ScAction = sca_GWV_target;
	}else{
		ScAction = sca_none;
		i--; // 該当なしの場合は、１つ戻す。
	}
	//
	//　対象変数リストを作成する。 () 内の場合
	//
	TScValData *ScValData;
	if(i >= TokStrList->Count){
		return;
	}
	s = TokStrList->Strings[i++];
	if(s == "("){
		for(; i < TokStrList->Count ; i++){
			s = TokStrList->Strings[i];
			if(s == ")"){
				i++;
				break;
			}
			s = TokStrList->Strings[i];
			if(s == ","){
				continue;
			}
			// 変数を追加する。（同名の変数があれば、共有する)
			ScValData = GetScValData(TokenList,s);
			ScValData->RefCount++;
			if(ScValDataList == NULL){
				ScValDataList = new TList;
			}
			ScValDataList->Add((void *)ScValData);
		}
	}else{
		i--;
	}
	//
	// 変数の後の region start , region end をチェックして ScRegionにセット
	// sub region start , sub region end も対象となる。
	//
	if(i >= TokStrList->Count){
		return;
	}
	s = LowerCase(TokStrList->Strings[i++]);
	if(s == "region"){
		if(i >= TokStrList->Count){
			return;
		}
		s = LowerCase(TokStrList->Strings[i++]);
		if(s == "start"){
			ScRegion = scr_start;
			if((ScType == sct_pre_BPset)||(ScType == sct_post_BPset)||
				(ScType == sct_SplitPointCopyDef)){
				ScRegion = scr_substart;
			}
		}else if(s == "end"){
			ScRegion = scr_end;
			if((ScType == sct_pre_BPset)||(ScType == sct_post_BPset)||
				(ScType == sct_SplitPointCopyDef)){
				ScRegion = scr_subend;
			}
		}
	}else if(s == "sub"){
		s = LowerCase(TokStrList->Strings[i++]);
		if(s == "region"){
			if(i >= TokStrList->Count){
				return;
			}
			s = TokStrList->Strings[i++];
			if(s == "start"){
				ScRegion = scr_substart;
			}else if(s == "end"){
				ScRegion = scr_subend;
			}
		}
	}

}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    sPos    トークンリスト開始位置
//
//  3.概要
//    fitting Scriptを解析してScriptデータに入れる。
//    SampledList->Count = 0 の場合は、デフォルトの値を設定する。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TScript::GetFittingParam(int sPos)
{
	int i = sPos;
	string s;
    int sIdx,eIdx,mIdx;

    FittingType = 0;
	SampledList = new TList;
//d-spline用追加部分
	FittingDspline = 0;
	FittingDynamic = 0;
//d-spline用追加部分　ここまで
	s = LowerCase(TokStrList->Strings[i++]);
	if(s == "least-squares"){
		FittingType = 1;
	}
//d-spline用追加部分

	/***********************************************************/
	//
	//Kogakuin Irie
	//pragma指示文の変更，および2次元への対応
	//実行時とその他によるd-Splineの区別は，
	//region start 文に記述したチューニングタイミングにより判断
	//
	//既存コードはコメントアウト
	//
	//if(
//	//s = LowerCase(TokStrList->Strings[i++]);
//	else if(s == "dspline"){
//		FittingDspline = 1;
//		printf("fittingdspline ON%d\n",FittingDspline);
//		goto DSP1;
//	}
//
//	else if(s == "dynamicdspline"){
//		FittingDynamic = 1;
//		printf("fittingdynamic ON%d\n",FittingDynamic);
//		goto DSP1;
//	}
	//s = LowerCase(TokStrList->Strings[i++]);
	else if(s == "dspline"){
		FittingDspline = 1;
		printf("fittingdspline ON%d\n",FittingDspline);
		goto DSP1;
	}
	else if(s == "dspline2"){
		FittingDspline = 2;
		printf("fittingdspline2 ON%d\n",FittingDspline);
		goto DSP1;
	}
	//
	//ここまで
	//
	/***********************************************************/
//d-spline用追加部分　ここまで
	if(i < TokStrList->Count){
		s = LowerCase(TokStrList->Strings[i++]);
	}
	FittingDegree = atoi(s.c_str());		// 多項式の次数
	if(i < TokStrList->Count){
		s = LowerCase(TokStrList->Strings[i++]);
    }
	if(s == "sampled"){
		for(;i <= TokStrList->Count;i++){
			s = TokStrList->Strings[i];
			s = Trim(s);
			if((s == "(")||(s == ",")){
				continue;
			}else if(s == ")"){
				break;
			}else{
				mIdx = s.find("-");
				if(mIdx >= 0){
					if(mIdx == 0){
						sIdx = 1;
					}else{
						sIdx = atoi(s.c_str());
						s = s.substr(mIdx+1,s.length());
					}
					eIdx = atoi(s.c_str());
				}else{
					sIdx = eIdx = atoi(s.c_str());
				}
				for(int i = sIdx ; i <= eIdx ; i++){
					if(SampledList->IndexOf((void *)(long)i) == -1){
						SampledList->Add((void *)(long)i);
                    }
                }
            }
        }
    }
DSP1:;//d-spline用追加部分
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList   トークンリスト
//    ValName スクリプト変数名
//
//  3.概要
//    ValNameと同じ名前のスクリプト変数を探して、なければ、新規に作成する。
//    検索は、TokListのScriptを順番に検索する。
//
//  4.機能説明
//
//  5.戻り値
//    ValNameと同じ名前のスクリプト変数のscValDataを返す。なければ、新規に作成し
//    てそのscValkDataを返す。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TScValData *GetScValData(TList *TokenList,string ValName)
{
	int i,j;
	TToken *Token;
	TScValData *ScValData;
	TScript *Script;
	int cp;

	// ""で囲まれて呼ばれた場合は""を変数名から削除する。
	cp = ValName.find("\"");
	if(cp >= 0){
		ValName.erase(cp,1);
	}
	cp = ValName.find("\"");
	if(cp >= 0){
		ValName.erase(cp,1);
	}
	for(i = 0 ;i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->Script == NULL){
			continue;
		}
		Script = (TScript *)Token->Script;
		if(Script->ScValDataList == NULL){
			continue;
		}
		for(j = 0 ; j < Script->ScValDataList->Count ; j++){
			ScValData = (TScValData *)Script->ScValDataList->Items[j];
			if(ScValData->Str == ValName){
				return ScValData;
			}
		}
	}
	ScValData = new TScValData(ValName);
	return ScValData;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    DefineF 変数定義入りの引数を返すか、変数名のみを返すかを指定する。
//	  ValName1 名前が一致した場合に置き換える変数
//	  ValName2 置き換える新しい変数名
//	  SkipArgCount すでに設定済としてスキップする引数の個数
//
//  3.概要
//    ATExec()のスクリプトの引数を返す。定義入りの場合は先頭の２引数は対象外とする。
//
//  4.機能説明
//
//  5.戻り値
//    ATExec()の引数の文字列を返す。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TScript::GetATExecArgStr(bool DefineF,string ValName1,string ValName2,int SkipArgCount)
{
	TToken *Token;
	TToken *Token2;
	string rStr = "";
	string s;
	int i,j,k;
	int ArgCount = 0;
	TValData *ValData;
	TStringList *ATExec_ArgValList;
	string ValName;

	for(i = 0 ; i < TokStrList->Count ; i++){
		Token = (TToken *)TokStrList->Objects[i];
		if(Token == NULL){
			continue;
		}
		if(Token->TokId == tid_Kannma){
			ArgCount++;
		}
		if(ArgCount < 2){
			continue;
		}
		if(Token->TokId == tid_Kakko){
			continue;
		}
		if(Token->TokId == tid_Kokka){
			continue;
		}
		if(Token->ValData == NULL){
			if(DefineF){
				rStr += Token->OrgStr;
			}else{
				rStr += Trim(Token->OrgStr);
			}
		}
		if(DefineF && (MainF->SrcCodeType != MainF->sctC)){
			//
			//	Fortranでの定義（１行ごと）
			//
			// 定義入りの文字列を設定する。
			// **の処理と Token->OrgStrのタブは定義の前へ
			ValData = (TValData *)Token->ValData;
			s = Token->OrgStr;
			for(int j = 0 ; j < (int)s.length() ; j++){
				if(s[j] <= ' '){
					rStr += s[j];
				}else{
					break;
				}
			}
			rStr += "      "+Trim(ValData->DefStr1 + ValData->DefStr2); // 並びの宣言なのでタブはなし。
			for(k = ValData->ArrayDefPosS ; k < ValData->ArrayDefPosE ; k++){
				Token2 = (TToken *)MainF->TokenList->Items[k];
				rStr += Token2->OrgStr;
			}
			rStr += "\n";

		}else if(DefineF){
			// 定義入りの文字列を設定する。
			// **の処理と Token->OrgStrのタブは定義の前へ
			ValData = (TValData *)Token->ValData;
			s = Token->OrgStr;
			for(int j = 0 ; j < (int)s.length() ; j++){
				if(s[j] <= ' '){
					rStr += s[j];
				}else{
					break;
				}
			}
			rStr += Trim(ValData->DefStr1 + ValData->DefStr2); // 並びの宣言なのでタブはなし。
			if(rStr[rStr.length()-1] == '*'){
				rStr += Token->Str;
			}else{
				rStr += " "+Token->Str;
			}
			for(k = ValData->ArrayDefPosS ; k < ValData->ArrayDefPosE ; k++){
				Token2 = (TToken *)MainF->TokenList->Items[k];
				rStr += Token2->OrgStr;
			}
		}else{
			if(Token->Str == ValName1){
				rStr += Trim(ValName2); // 定義でない場合は空白を詰める。
			}else{
				rStr += Trim(Token->Str); // 定義でない場合は空白を詰める。
			}
		}
	}
	//
	//	自動追加された変数があれば追加する。
	//
	ATExec_ArgValList = MainF->Call_ATExec_ArgList;
//	for(j = 0 ; j < ATExec_ArgValList->Count ; j++){
	for(j = SkipArgCount ; j < ATExec_ArgValList->Count ; j++){
		ValName = ATExec_ArgValList->Strings[j];
		for(i = 0 ; i < TokStrList->Count ; i++){
			Token = (TToken *)TokStrList->Objects[i];
			if(Token == NULL){
				continue;
			}
			if(Token->ValData != NULL){
				ValData = (TValData *)Token->ValData;
				if(ValName == ValData->Str){
					break;
				}
			}
		}
		if(i == TokStrList->Count){
			ValData = (TValData *)ATExec_ArgValList->Objects[j];
			if(DefineF && (MainF->SrcCodeType != MainF->sctC)){
				rStr += ValData->GetDefStr_Fortran()+" "+ValData->Str;
				for(k = ValData->ArrayDefPosS ; k < ValData->ArrayDefPosE ; k++){
					Token2 = (TToken *)MainF->TokenList->Items[k];
					rStr += Token2->OrgStr;
				}
				rStr += "\n";
			}else if(DefineF){
				rStr += ","+ValData->GetDefStr_C()+ValData->Str;
				for(k = ValData->ArrayDefPosS ; k < ValData->ArrayDefPosE ; k++){
					Token2 = (TToken *)MainF->TokenList->Items[k];
					rStr += Token2->OrgStr;
				}
			}else{
				s = ValData->GetDefStr_C();	// 定義部分の改行とスペースを継承する。
				for(int k = 0 ; k < (int)s.length(); k++){
					if(s[k] > ' '){
						s.erase(k,s.length());
						break;
					}
				}
				rStr += ","+s+ValData->Str;
			}
		}
	}
	//
	//	行が長い場合は、カンマ部分の後で分割する。
	//
	int LineLen = 0;
	int cp;

	s = rStr;
	rStr = "";
	for(i = 0 ; i < (int)s.length() ; i++){
		rStr += s[i];
		LineLen++;
		if(s[i] == '\n'){
			LineLen = 0;
		}
		if(LineLen >= 50){
			if(s[i] == ','){
				cp = s.find('\n',i);
				if(cp > i+10){
					rStr += "\n\t";
					LineLen = 0;
				}
			}
		}
	}
	return rStr;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    s   元文字列
//
//  3.概要
//    スクリプト種類と()の対応をチェックして返す。
//    次の行が継続行かどうかの判定に使用する。
//
//  4.機能説明
//
//  5.戻り値
//    （と）の数の差を返す。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
int TScript::CheckBrBalance()
{
	int i;
	TToken *Token;
	int BrCount = 0;

	if(ScType != sct_ATExec){
		return 0; // 対象種類以外はチェックなしで０を返す。
	}
	for(i = 0 ; i < TokStrList->Count ; i++){
		Token = (TToken *)TokStrList->Objects[i];
		if(Token == NULL){
			continue;
		}
		if(Token->TokId == tid_Kakko){
			BrCount++;
		}
		if(Token->TokId == tid_Kokka){
			BrCount--;
		}
	}
	return BrCount;

}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    スクリプトデータ破棄
//    ScValDataList内のデータの破棄。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TScript::~TScript()
{
	int i;
	TScValData *ScValData;

	if(ScValDataList != NULL){
		for(i = 0 ; i < ScValDataList->Count ; i++){
			ScValData = (TScValData *)ScValDataList->Items[i];
			ScValData->RefCount--;
			if(ScValData->RefCount == 0){
             	delete ScValData;
            }
		}
    	delete ScValDataList;
    }
    if(SampledList != NULL){
    	delete SampledList;
    }
    delete TokStrList;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    ValName スクリプト変数名
//
//  3.概要
//    スクリプト変数の生成
//    Fortranの変数と別途にスクリプト内での変数として生成される。
//    また、データの初期化も行われる。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TScValData::TScValData(string ValName)
{
	Str = ValName;
	RefCount = 0;
	dData = 0;
	sData = 0;
	eData = 0;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    s   元文字列
//
//  3.概要
//    長い文字列を72桁で分割して、&をセットする。
//	  Ｃ言語の場合は、不要。現在は元の文字列をそのまま戻している。
//	  Fortran90の場合もコメント等をそのままにしたいため分割しない。
//
//  4.機能説明
//
//  5.戻り値
//    元の文字列をそのまま戻している。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string SepLongStr(string s)
{
	int cp;
	string s2,s3;
	string LineStr;
	string rStr;

	if(MainF->SrcCodeType == MainF->sctC){
		return s;
	}
	rStr = "";
	if(MainF->SrcCodeType == MainF->sctFortran77){
		//
		//	Fortrn77の場合
		//	改行までの文字列長さが72を超える場合に、継続行にする。
		//  途中に\nがあれば、そこで改行になっているのでそのままとする。
		//  先頭にスペースを入れたい場合もあるので72文字でなく60文字までに変更。
		//
		while(s != ""){
			cp = s.find('\n');
			if(cp == -1){
				LineStr = s;
				s = "";
			}else{
				LineStr = s.substr(0,cp+1);
				s.erase(0,cp+1);
			}
			if(LineStr.length() <= 72){
				rStr += LineStr;
			}else{
				// LineStrが１行の長さを超えているので行を複数の行に分割する。
				while(LineStr.length() > 72){
					rStr += LineStr.substr(0,72)+"\n";
					LineStr.erase(0,72);
					LineStr =  "     &"+LineStr;
				}
				rStr += LineStr;
			}
		}
	}else{
		//
		//	Fortrn90の場合
		//	改行までの文字列長さが132を超える場合に、継続行にする。
		//  途中に\nがあれば、そこで改行になっているのでそのままとする。
		//  先頭にスペースを入れたい場合もあるので132文字でなく120文字までに変更。
		//
		while(s != ""){
			cp = s.find('\n');
			if(cp == -1){
				LineStr = s;
				s = "";
			}else{
				LineStr = s.substr(0,cp+1);
				s.erase(0,cp+1);
			}
//			if(LineStr.length() <= 132){
			if(LineStr.length() <= 120){
				rStr += LineStr;
			}else{
				// LineStrが１行の長さを超えているので行を複数の行に分割する。
//				while(LineStr.length() > 132){
//					rStr += LineStr.substr(0,131)+"&\n";
//					LineStr.erase(0,131);
				while(LineStr.length() > 120){
					rStr += LineStr.substr(0,119)+"&\n";
					LineStr.erase(0,119);
					LineStr =  "     &"+LineStr;
				}
				rStr += LineStr;
			}
		}
	}
	return rStr;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    sPos    トークンリスト開始位置
//
//  3.概要
//    !OAT$ bind ATexec arguments end 部分で呼ばれて
//	  argumentsd start までを解析して変数置換えリストをScriptデータに入れる。
//    また、置換として参照したScriptの種類を変更する。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TScript::GetBindArgument(TList *TokenList,int argumentEndPos)
{
	int i;
	TScript *Script;
	TToken *Token;
	int argumentStartPos = -1;
	int len;
	string valName1;
	string valName2;

	//
	// arguments start を探す。見つからない場合はエラーとなる。
	//
	for(i = argumentEndPos-1 ; i >= 0 ; i--){
		Token = (TToken *)TokenList->Items[i];
		if(Token == NULL){
			continue;
		}
		Script = (TScript *)Token->Script;
		if(Script == NULL){
			continue;
		}
		if(Script->ScAction == sca_bind_atexec_arg_end){
			break;
		}
		if(Script->ScAction == sca_bind_atexec_arg_start){
			argumentStartPos = i;
			break;
		}
	}
	if(argumentStartPos == -1){
		MainF->ErrMessage(argumentEndPos,"bind ATexec arguments start が見つかりません。");
		return;
	}

	//
	// arguments start から現在位置までを置換リストに追加し
	// そのScript種類を変更する。置換え実行時は arguments end のリストが参照される。
	//
	if(bindValNameList != NULL){
		delete bindValNameList;
	}
	bindValNameList = new TStringList;
	for(i = argumentStartPos ; i < argumentEndPos ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token == NULL){
			continue;
		}
		Script = (TScript *)Token->Script;
		if(Script == NULL){
			continue;
		}
		if(Script->ScType != sct_command){
			continue;
		}
		Script->ScType = sct_Bind;
		len = Script->LineEndTokPos - Script->TokPos;
		if(len != 4){
			MainF->ErrMessage(i,"bind ATexec arguments 書式エラー");
			return;
		}
		Token = (TToken *)TokenList->Items[Script->TokPos+1];
		valName1 = Token->Str;
		Token = (TToken *)TokenList->Items[Script->TokPos+2];
		if(Token->Str != "="){
			MainF->ErrMessage(i,"bind ATexec arguments 書式エラー");
			return;
		}
		Token = (TToken *)TokenList->Items[Script->TokPos+3];
		valName2 = Token->Str;
		bindValNameList->Add(valName1); // 文字列での置換になる。
		bindValNameList->Add(valName2);	// valName1 -> valName2
	}
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    sPos    トークンリスト開始位置
//
//  3.概要
//    !OAT$ bind ATexec arguments を探して見つかれば変数名を置換した引数文字列を返す。
//
//  4.機能説明
//
//  5.戻り値
//	  変数名を置換した引数文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TScript::ReplaseBindValName(string argStr)
{
	//
	// arguments end を探す。見つからない場合は該当なし。
	//
	TList *TokenList = MainF->TokenList;
	int i,j;
	TScript *Script;
	TToken *Token;
	TStringList *NameList;
	string valName;
	string NewArgStr;

	for(i = TokPos ; i >= 0 ; i--){
		Token = (TToken *)TokenList->Items[i];
		if(Token == NULL){
			continue;
		}
		Script = (TScript *)Token->Script;
		if(Script == NULL){
			continue;
		}
		if(Script->ScAction == sca_bind_atexec_arg_end){
			break;
		}
	}
	if(i <= 0){
		return argStr;
	}
	NameList = (TStringList *)Script->bindValNameList;
	if(NameList == NULL){
		return argStr;
	}
	//
	//	argStrをカンマで分割して変数名の置換を行う。
	//
	NewArgStr = "";
	valName = "";
	for(i = 0 ; i < (int)argStr.length() ; i++){
		if(argStr[i] != ','){
			valName += argStr[i];
			if(i != (int)argStr.length()-1){
				continue;
			}
		}
		//
		//	変数名の置換を行う。
		//
		for(j = 0 ; j < NameList->Count ; j+=2){
			if(NameList->Strings[j] == valName){
				valName = NameList->Strings[j+1];
			}
		}
		NewArgStr += valName;
		valName = "";
		if(argStr[i] == ','){
			NewArgStr += argStr[i];
		}
	}
	return NewArgStr;
}


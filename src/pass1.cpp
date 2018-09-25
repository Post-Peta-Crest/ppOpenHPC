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
//    パス１
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "pass1.h"

void Pass1_2(TList *TokenList);
void Pass1_3_Fortran(TList *TokenList);
void MakeTokStrTbl_C(TStringList *TokTbl);
void MakeTokStrTbl_Fortran(TStringList *TokTbl);

//
// トークンセパレータ
//
//static char *SepStr = (char *)" ?\"\';:+-*/%#,!=<>&|^~()[]{}\t\r\n";	// トークン区切り(C+Fortran)
static char *SepStr = (char *)" .?\"\';:+-*/%#,!=<>&|^~()[]{}\t\r\n";	// トークン区切り(C+Fortran)
	// なぜか . がトークン区切りから抜けていたので追加。

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    SrcFileName ソースファイル名
//    TokenList   生成したトークンの格納先
//
//  3.概要
//	  ソース文字列をスキャンし区切り文字列で分割したトークンリストを生成する。
//    生成したトークンは、TokenListに追加される。
//	  CとFortranの両方で使用する。
//
//  4.機能説明
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void Pass1(string SrcFileName,TList *TokenList)
{
	char c,c2;
	string Str;
	TStringList *StrList;
	int cp,cp1,cp2;
	char cData;
	int TextLength;
	string ErrMsg = "";
	TTokId TokId;
	TToken *TokData;
	int LineNo;
	string LineText;
	string s,s1;
	char *lp;
	bool *ContinuedLineF = NULL;
	int ContinuedLineCount = 0;
	string OrgStr;
	bool IsF90SrcFileF;
	bool InFortranCommentF;

	IsF90SrcFileF = (MainF->SrcCodeType == MainF->sctFortran90);

	//
	//	Ｃの場合は、Fortranと違い、行の概念がない.
	//	文字列 "" とコメント /* */ // 以外は、全てトークンとして分析する。
	//
	StrList = new TStringList;
	StrList->LoadFromFile(SrcFileName); // 行ごとに区切って読み込む。
	StrList->Add("\n");

	//
	//	固定形式Fortranの場合は、継続行の処理を行う。
	//	継続行が長い１行（文字列が継続される場合もあり）に変換される。
	//	自由形式 f80の場合は、トークンでの処理となるので文字列としてはそのまま。
	//
	ContinuedLineF = new bool[StrList->Count+1];
	for(LineNo = 0 ; LineNo < StrList->Count+1 ; LineNo++){
		ContinuedLineF[LineNo] = false;
	}
	if(MainF->SrcCodeType == MainF->sctFortran77){
		for(LineNo = 1 ; LineNo < StrList->Count ; LineNo++){
			Str = StrList->Strings[LineNo];
			if((Str.length() >= 6)&&(Str[0] == ' ')&&(Str[1] == ' ')
				&&(Str[2] == ' ')&&(Str[3] == ' ')&&(Str[4] == ' ')
				&&(Str[5] != ' ')&&	(Str[5] != '0')){
				ContinuedLineF[LineNo] = true;
				Str = Str.erase(0,6); 	// 先頭の６文字をカット
				StrList->Strings[LineNo] = Str;
			}
		}
		// 継続行をまとめる。空の行が作成される。これで正しく表示される形に調整する。
		for(int i = StrList->Count-1 ; i > 0 ; i--){
			if(ContinuedLineF[i]){
				s1= StrList->Strings[i-1]+StrList->Strings[i];
				StrList->Strings[i] = "";
				for(int j = s1.length()-1 ; j >= 0 ; j--){
					if(s1[j] == '\r'){
                        s1.erase(j,1);
                    }
				}
			    StrList->Strings[i-1] = s1;
			}
		}
	}
	LineNo = 0;
	LineText = "";
	cp = 0;

	Str = "";
	TextLength = 0;
	InFortranCommentF = false;
	while(LineNo < StrList->Count){
		if(cp >= TextLength){	// 残りの文字がないので行を読み込む。
			if(LineNo != 0){
				TokData = new TToken("\n",tid_LineEnd);
				TokData->OrgStr = "\n";
//				TokData->OrgStr = " [CR]\n"; // テスト用
				TokData->LineNo = LineNo - ContinuedLineCount; // 元のライン番号、Fortranの場合は解析にも使用。
				TokData->LineEndF = true;
				TokenList->Add(TokData);
				InFortranCommentF = false;
			}
			LineText = StrList->Strings[LineNo++];
			if(ContinuedLineF[LineNo-1]){
				ContinuedLineCount++;
//				MainF->print(IntToStr(LineNo)+"> "+ LineText);
			}else{
				ContinuedLineCount=0;
			}
			TextLength = LineText.length();
			cp = 0;
			continue;
		}
		// １単語を追加する。
		cp1 = cp;
		while((cp < TextLength)&&(LineText[cp] <= ' ')){
			cp++;	// 先頭のスペースは追加しない
		}
		cp2 = cp;
		//
		// Strにチェックするトークンを読み込む
		// セパレータが出現するまで読み込むことで、高速化を図る。
		// セパレータが見つかれば、その手前までの文字をStrに入れて検索へ
		//
		for( ; cp < TextLength ; cp++){  // 行末もセパレータ扱い
			cData = LineText[cp];
			if(strchr(SepStr,cData) != NULL){
				if(cp == cp2){
					cp++;
				}
				break;
			}
		}
		Str = LineText.substr(cp2,cp-cp2);   // セパレータ手前までの文字列
		if(Str == ""){
			continue;
		}
		TokId = tid_null;
		c = Str[0];
		if(cp2 + 1 < TextLength){	// /* 等の判定用の次の文字。
			c2 = LineText[cp2+1];
		}else{
			c2 = 0;
		}
		if(MainF->SrcCodeType != MainF->sctC){
			if((c == '"')||(c == '\'')){  // 文字列の処理
				TokData = new TToken(Str,0);
				TokData->TokId = tid_ConstStr; // 文字列
				TokData->LineNo = LineNo - ContinuedLineCount;

				OrgStr = LineText.substr(cp1,cp-cp1);
				Str = LineText[cp++];
				while(cp < TextLength){
					OrgStr += LineText[cp];
					//
					//	継続行の処理を行う。
					//
					if(IsF90SrcFileF && (LineText[cp] == '&')&&
						((cp >= TextLength)||(Trim(LineText.substr(cp+1,TextLength)) == ""))){
						if(LineNo+1 < StrList->Count){
							OrgStr += "\n";
							LineText = StrList->Strings[LineNo++];
							TextLength = LineText.length();
							cp = 0;
							s = Trim(LineText);
							if((s.length() > 0)&&(s[0] == '&')){
								cp = LineText.find('&')+1;	// 先頭の & 位置
								s = LineText.substr(0,cp);
								OrgStr += s;
							}
						}
					}
					if(LineText[cp] == c){
						cp++;
						break;
					}
					Str += LineText[cp++];
				}
				if(cp > TextLength){
					ErrMsg = "String is not closed. ";
					break;
				}
				OrgStr = LineText.substr(cp1,cp-cp1);
				TokData->Str = Str;
				TokData->OrgStr = OrgStr;
				TokenList->Add(TokData);

				continue;  // 継続 LineText[cp]から継続する。
			}else if(((c >= '0')&&(c <= '9'))||(c == '.')){ // 数値
				cp = cp2; // 前に戻ってから再度解析
				TokId = tid_ConstInt;
				while(cp < TextLength){
					c = LineText[cp++];
					if(((c >= '0')&&(c <= '9'))||(c == '.')){
						continue;
					}else if((c == 'd')||(c == 'D')||(c == 'e')||(c == 'E')){
						TokId = tid_ConstReal;
						if(cp < TextLength){
							c = LineText[cp++];
							if((c == '+')||(c == '-')){
								cp++;
							}
						}
						while(cp < TextLength){
							c = LineText[cp++];
							if((c >= '0')&&(c <= '9')){
								continue;
							}
							break;
						}
					}
					cp--;
					break;
				}
				if(c == '_'){	// 定数の種別値の指定あり。 "定数_変数名"
								// セパレータまで進める。
					for( ; cp < TextLength ; cp++){
						cData = LineText[cp];
						if(strchr(SepStr,cData) != NULL){
							if(cp == cp2){
								cp++;
							}
							break;
						}
					}
				}
				Str = LineText.substr(cp2,cp-cp2);
				if(Str.find('.') != string::npos){
					TokId = tid_ConstReal;
				}
			}
			else if(c == '!'){ // ! 以後は行末までコメント // F77等も含む
				TokId = tid_Comment;
				Str = LineText.substr(cp2,TextLength-cp2);
#if 0
				cp = LowerCase(Str).find("!oat$");
				if(cp < 0){
					cp = TextLength;
				}else{
					// ATExecや Call等の実行のために、トークン分解する。
					cp = cp + 5 + cp2;
					Str = LineText.substr(cp2,cp-cp2);
				}
#else
				// VariableDのために、常にトークン分解を行う。コメント中識別を追加。
				cp = cp + 5 + cp2;
				Str = LineText.substr(cp2,cp-cp2);
				InFortranCommentF = true;
#endif
			}
			// 固定形式でのコメント処理
			else if(!IsF90SrcFileF && (cp2 == 0)&&((c == 'c')||(c == 'C')||(c == '*'))){ // １文字目がc,C,*
				cp = TextLength;
				TokId = tid_Comment;
				Str = LineText.substr(cp2,cp-cp2);
				InFortranCommentF = true;
			}
			//
			// 自由形式での継続行の処理。固定形式の継続行は読込時点で文字列として処理済
			// 文字列での継続行処理は、文字列の中で行う。
			// 行の最後の & が見つかれば、　& と場合によっては次の行に&までの文字列を
			// OrgStrとして実体のStr="" の継続行トークンが追加される。
			//
			// 文末の& は、 &の後が空白やコントロールコード !で始まるコメントも含む。
			// コメントを含んだ場合には [& ! Comment \n   &] が継続用文字列となる。
			//
			if(IsF90SrcFileF && (c == '&')){
				int i2;
				char c2;

				for(i2 =cp+1 ; i2 < TextLength ; i2++){
					c2 = LineText[i2];
					if(c2 == '!'){
						i2 = TextLength;
						break;
					}
					if(c2 > ' '){
						break;
					}
				}
				if(i2 >= TextLength){ // 継続行用の&
					//
					//	行末までの文字を入れる。
					//
					Str = "";
					OrgStr = LineText.substr(cp1,TextLength)+"\n";

					TokData = new TToken(Str,0);
					TokData->TokId = tid_ContinuedLineStr;	// 継続行用 &\n    &
					TokData->LineNo = LineNo - ContinuedLineCount;
					//
					//	次の行の先頭の&までが継続行トークンとなる。
					// cpだけでなくLineTextも更新ｓれる。
					//
					if(LineNo+1 < StrList->Count){
						LineText = StrList->Strings[LineNo++];
						TextLength = LineText.length();
						cp = 0;
						s = Trim(LineText);
						if((s.length() > 0)&&(s[0] == '&')){
							cp = LineText.find('&')+1;	// 先頭の & 位置
							s = LineText.substr(0,cp);
							OrgStr += s;	// "   &" を追加。
						}
					}
					TokData->OrgStr = OrgStr;
					TokenList->Add(TokData);

					continue;  // 継続 LineText[cp]から継続する。
				}
			}
		}else{ 	//	C用の解析
			if((c == '"')||(c == '\'')){  // 文字列の処理
				Str = LineText[cp++];
				while(cp < TextLength){
					if(LineText[cp] == c){
						cp++;
						break;
					}
					if((LineText[cp] == '\\')&&(cp < TextLength)){	//  エスケープ文字
						cp++;
						if(LineText[cp] == 'n'){
							Str += "\n";
						}else if(LineText[cp] == 'r'){
							Str += "\r";
						}else if(LineText[cp] == 't'){
							Str += "\t";
						}else if((LineText[cp] == 'x')&&(cp+2 < TextLength)){
							int Data;
							string s2;

							s2 = LineText.substr(cp+1,2);
							Data = strtol(s2.c_str(),NULL,16);
							if(Data != -1){
								Str += (char)Data;
							}
							cp += 2;
						}else{
							Str += LineText[cp];	// \c 文字そのもの。\\,\",\'
						}
						cp++;
					}else{
						Str += LineText[cp++];
					}
					if(cp >= TextLength){
						ErrMsg = "String is not closed. ";
						break;
					}
				}
				TokId = tid_ConstStr; // 文字列
			}else if(((c >= '0')&&(c <= '9'))||(c == '.')){
				//
				// 数値（先頭の+-は別トークン）
				//
				cp = cp2; // 前に戻ってから再度解析
				while(cp < TextLength){
					c = LineText[cp++];
					if(((c >= '0')&&(c <= '9'))||(c == '.')){
						continue;
					}else if((c == 'e')||(c == 'E')){
						if(cp < TextLength){
							c = LineText[cp++];
							if((c == '+')||(c == '-')){
								cp++;
							}
						}
						while(cp < TextLength){
							c = LineText[cp++];
							if((c >= '0')&&(c <= '9')){
								continue;
							}
							break;
						}
					}else if((c == 'X')||(c == 'x')){
						while(cp < TextLength){
							c = LineText[cp++];
							if((c >= '0')&&(c <= '9')){
								continue;
							}
							if((c >= 'a')&&(c <= 'f')){
								continue;
							}
							if((c >= 'A')&&(c <= 'F')){
								continue;
							}
							break;
						}
					}
					cp--;
					break;
				}
				TokId = tid_ConstNum;
				Str = LineText.substr(cp2,cp-cp2);
			}else if((c == '/')&&(c2 == '*')){
				int CommentNest = 1;

				cp++;
				Str = LineText.substr(cp2,cp-cp2);
				OrgStr = LineText.substr(cp1,cp-cp1);
				while(LineNo < StrList->Count){
					if(cp >= TextLength){	// 残りの文字がないので行を読み込む。
						LineText = StrList->Strings[LineNo++];
						TextLength = LineText.length();
						cp = 0;
						OrgStr += "\n";
						continue;
					}
					for(; cp < TextLength ; cp++){
						OrgStr += LineText[cp];
						Str += LineText[cp];
						if(cp == TextLength-1){

						}else if((LineText[cp] == '/')&&(LineText[cp+1] == '*')){
							cp++;
							Str += LineText[cp++];
							if(CommentNest == 0){
								CommentNest++;	// コメントネストはなし。
							}
						}else if((LineText[cp] == '*')&&(LineText[cp+1] == '/')){
							cp++;
							OrgStr += "/";
							Str += LineText[cp++];
							CommentNest--;
						}
						if(CommentNest == 0){
							break;
						}
					}
					if(CommentNest == 0){
						break;
					}
				}
				TokId = tid_Comment;
				TokData = new TToken(Str,0);
				TokData->OrgStr = OrgStr;
				TokData->TokId = TokId; // 文字列、コメント、数値は確定済。
				 // 元のライン番号、Fortranの場合は解析にも使用。
//				TokData->LineNo = LineNo; // 元のライン番号、Fortranの場合は解析にも使用。
				TokData->LineNo = LineNo - ContinuedLineCount;
				TokenList->Add(TokData);
				continue;
//			}else if((c == '/')&&(LineText[cp+1] == '/')){ // 行末までのコメント処理
			}else if((c == '/')&&(c2 == '/')){ // 行末までのコメント処理
				cp = TextLength;
				TokId = tid_Comment;
				Str = LineText.substr(cp2,cp-cp2);
			}
		}
		//
		// トークンとして追加する。この時点では文字列、コメント、数値以外は未定。
		//
		TokData = new TToken(Str,0);
		TokData->OrgStr = LineText.substr(cp1,cp-cp1);
		if(InFortranCommentF){
			TokId = tid_Comment;	// Fortanのコメント解析部分
		}
		TokData->TokId = TokId; // 文字列、コメント、数値は確定済。
		 // 元のライン番号、Fortranの場合は解析にも使用。
//		TokData->LineNo = LineNo; // 元のライン番号、Fortranの場合は解析にも使用。
		TokData->LineNo = LineNo - ContinuedLineCount;
		// 定数の場合は、数値も設定する。
		if(TokId == tid_ConstInt){
			TokData->dData = atoi(TokData->Str.c_str());
		}else if(TokId == tid_ConstReal){
			s = TokData->Str;
			for(int i = 0 ; i < (int)s.length() ; i++){
				if((s[i] == 'D')||(s[i] == 'd')){
					s[i] = 'e';
				}
			}
			TokData->dData = strtod(s.c_str(),&lp);
		}else if(TokId == tid_ConstNum){ // C用の数値。
			s = TokData->Str;
			TokData->dData = strtod(s.c_str(),&lp);
			for(int i = 0 ; i < (int)s.length() ; i++){
				if((s[i] == 'x')||(s[i] == 'X')){
					TokData->dData = strtol(s.c_str(),&lp,16);
					break;
				}
			}
		}
		TokenList->Add(TokData);
	}
	delete StrList;
	//
	//	TokData->Strの文字列から、TokIdをセットする。場合によっては複数トークンを１つにまとめる。
	//
	Pass1_2(TokenList);
#if 0
	MainF->DispTokenList();
	MainF->print("#2---------------------------------\n");
#endif
	if(MainF->SrcCodeType != MainF->sctC){
		Pass1_3_Fortran(TokenList);
	}
	delete[] ContinuedLineF;

}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList 対象トークンリスト
//
//  3.概要
//    TokData->Strの文字列から、TokIdをセットする。
//	  また、複数トークンを１つにまとめる。
//
//  4.機能説明
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void Pass1_2(TList *TokenList)
{
	int i;
	int idx;
	TStringList TokStrList;
	TToken *Token;
	string Str,TokStr;
	bool bRet;
	TToken *Token2;

	if(MainF->SrcCodeType != MainF->sctC){
		MakeTokStrTbl_Fortran(&TokStrList);
	}else{
		MakeTokStrTbl_C(&TokStrList);
	}
	for(i = 0 ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->TokId != tid_null){
			continue;	// 既に確定
		}
		Str = Token->Str;
		if(Str.length() == 0){
			continue;
		}
		if(MainF->SrcCodeType != MainF->sctC){
			bRet = TokStrList.FindIC(Str,idx);	// Fortranは大文字小文字が無関係
		}else{
			bRet = TokStrList.Find(Str,idx);
		}
		if(!bRet){	// 該当トークンなし
			Token->TokId = tid_Val; // 変数とする。
		}else{
			Token->TokId = (TTokId)((long)TokStrList.Objects[idx]);
		}
	}
	//
	// != 等の２つのトークンを合わせる処理を行う。CとFortranで異なる。
	//
	for(i = TokenList->Count-2 ; i >= 0 ; i--){
		Token = (TToken *)TokenList->Items[i];
		Token2 = (TToken *)TokenList->Items[i+1];

		if(Token->TokId == tid_null){	// ソースコードの区切りのnullは残す。2016/03/16
			continue;
		}
		if(MainF->SrcCodeType == MainF->sctC){
			if((Token->TokId == tid_Val)&&(Token2->TokId == tid_Kakko)){
				Token->TokId = tid_Func; // （の前の変数は関数
				continue;
			}
			if((Token->TokId == tid_Val)&&(Token2->TokId == tid_Val)){
				Token->TokId = tid_DataType; // 変数が連続する場合の前の変数は型名
				continue;
			}
		}
		//
		// ２つ目の先頭が空白の場合は１つの空白として検索。"end do"等のため。
		//
		if(Token2->OrgStr[0] == ' '){
			Str = Token->Str + ' ' + Token2->Str;
		}else{
			Str = Token->Str + Token2->Str;
		}
		if(MainF->SrcCodeType != MainF->sctC){
			bRet = TokStrList.FindIC(Str,idx);	// Fortranは大文字小文字が無関係
		}else{
			bRet = TokStrList.Find(Str,idx);
		}
		if(bRet){
			// ２つを合成したトークンあり。 さらに３つも可能 <<= 等
			// トークンとして設定する。ただし、(を含む合成は行わない。
			Token->Str = Str;
			Token->OrgStr = Token->OrgStr + Token2->OrgStr;
			Token->TokId = (TTokId)((long)TokStrList.Objects[idx]);
			delete Token2;
			TokenList->Delete(i+1); // リストから削除
		}
	}
	//
	//	演算子に優先順位を設定する。
	//
	for(i = 0 ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		Token->Idx = i;
		if(Token->TokId == tid_Opr){
			TokStr = Token->Str;
			if(MainF->SrcCodeType != MainF->sctC){
				if(TokStr == "+"){
					Token->priority = 1;
				}else if(TokStr == "-"){
					Token->priority = 1;
				}else if(TokStr == "*"){
					Token->priority = 2;
				}else if(TokStr == "/"){
					Token->priority = 2;
				}else if(TokStr == "**"){
					Token->priority = 3;
				}else if(LowerCase(TokStr) == ".neqv."){
					Token->priority = 0;
				}else if(LowerCase(TokStr) == ".eqv."){
					Token->priority = 0;
				}else{
					Token->priority = 1; // その他の演算子は 優先順位1
				}
			}else{	// C
				if((TokStr == "++")||(TokStr == "--")){
					Token->priority = 11;
				}else if((TokStr == "*")||(TokStr == "/")||(TokStr == "%")){
					Token->priority = 10;
				}else if((TokStr == "+")||(TokStr == "-")){
					Token->priority = 9;
				}else if((TokStr == "<")||(TokStr == ">")||(TokStr == "<=")||(TokStr == ">=")){
					Token->priority = 8;
				}else if((TokStr == "==")||(TokStr == "!=")){
					Token->priority = 7;
				}else if(TokStr == "&"){
					Token->priority = 7;
				}else if(TokStr == "^"){
					Token->priority = 6;
				}else if(TokStr == "|"){
					Token->priority = 4;
				}else if(TokStr == "&&"){
					Token->priority = 3;
				}else if(TokStr == "||"){
					Token->priority = 2;
				}else if(TokStr == ","){
					Token->priority = 1;
				}
			}
		}
	}
#if 0
	for(i = 0 ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		MainF->print(IntToStr(i) + " ["+((TToken *)TokenList->Items[i])->Str+"] Id = "
			+ IntToStr(((TToken *)TokenList->Items[i])->TokId));
	}
#endif
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokenList 対象トークンリスト
//
//  3.概要
//    フォートランの構文に合わせて、Token->TokIdをtid_val(変数）に変更する。
//	  変数名としての end や if を有効とするために構文に合わせて一部を変更。
//
//  4.機能説明
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void Pass1_3_Fortran(TList *TokenList)
{
	int brLevel = 0;
	int LineEndF = true;
	int i;
	int TokId;
	TToken *Token;

	//
	// 行の先頭に来るべきキーワードが途中にある場合に変数扱いとする。
	// 本来は構文解析した結果で判断すべきだが、現状は行の先頭かどうかのみを判定。
	//
	for(i = 0 ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token == NULL){
			continue;	// 既に確定
		}
		TokId = Token->TokId;
		switch(TokId){
		case tid_null:
		case tid_Comment:	// コメント
			break;
		case tid_LineEnd:
		case tid_Semikoron:
			LineEndF = true;
			brLevel = 0;
			break;
		case tid_Kakko:
			brLevel++;
			break;
		case tid_Kokka:
			brLevel--;
			break;
		case tid_ConstInt: // 行番号やラベルの場合は行の先頭状態を保持
		case tid_Val:
		case tid_Koron:
			break;
		case tid_END:	// 行の先頭でない場合は変数に変更。
		case tid_IF:
		case tid_ELSE:
		case tid_ELSEIF:
		case tid_ENDIF:
		case tid_DO:
		case tid_ENDDO:
		case tid_SELECT:
		case tid_CASE:
		case tid_GOTO:
		case tid_DOWHILE:
		case tid_CALL:
		case tid_RETURN:
		case tid_STOP:
			if(LineEndF == false){
				Token->TokId = tid_Val;
			}
			break;
		default:
			LineEndF = false;
		}
	}
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokStrTbl   作成するトークン名とTokIdの対応テーブル格納先。
//
//  3.概要
//    Fortran用トークン名と、TokIdの対応テーブルを作成する。
//    作成したテーブルは、字句解析で使用される。
//
//  4.機能説明
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void MakeTokStrTbl_Fortran(TStringList *TokStrTbl)
{
	// キーワード（単語）

	// プログラム関連
	TokStrTbl->AddObject("PROGRAM",(void *)tid_PROGRAM);
	TokStrTbl->AddObject("SUBROUTINE",(void *)tid_SUBROUTINE);
	TokStrTbl->AddObject("FUNCTION",(void *)tid_FUNCTION);
	TokStrTbl->AddObject("MODULE",(void *)tid_MODULE);
	TokStrTbl->AddObject("INTERFACE",(void *)tid_INTERFACE);
	TokStrTbl->AddObject("CONTAINS",(void *)tid_CONTAINS);
	TokStrTbl->AddObject("USE",(void *)tid_USE);
	TokStrTbl->AddObject("INCLUDE",(void *)tid_INCLUDE);

	// データ宣言
	TokStrTbl->AddObject("COMMON",(void *)tid_COMMON);
	TokStrTbl->AddObject("INTEGER",(void *)tid_INTEGER);
	TokStrTbl->AddObject("REAL",(void *)tid_REAL);
	TokStrTbl->AddObject("DOUBLEPRECISION",(void *)tid_DOUBLEPRECISION);
	TokStrTbl->AddObject("DOUBLE PRECISION",(void *)tid_DOUBLEPRECISION);
	TokStrTbl->AddObject("CHARACTER",(void *)tid_CHARACTER);
	TokStrTbl->AddObject("LOGICAL",(void *)tid_LOGICAL);
	TokStrTbl->AddObject("COMPLEX",(void *)tid_COMPLEX);
	TokStrTbl->AddObject("IMPLICIT",(void *)tid_IMPLICIT);
	TokStrTbl->AddObject("TYPE",(void *)tid_TYPE);
	TokStrTbl->AddObject("NAMELIST",(void *)tid_NAMELIST);
	TokStrTbl->AddObject("ALLOCATABLE",(void *)tid_ALLOCATABLE);
	TokStrTbl->AddObject("POINTER",(void *)tid_POINTER);
	TokStrTbl->AddObject("TARGET",(void *)tid_TARGET);
	TokStrTbl->AddObject("SAVE",(void *)tid_SAVE);
	TokStrTbl->AddObject("PRIVATE",(void *)tid_PRIVATE);
	TokStrTbl->AddObject("PUBLIC",(void *)tid_PUBLIC);
	TokStrTbl->AddObject("EXTERNAL",(void *)tid_EXTERNAL);
	TokStrTbl->AddObject("INTRINSIC",(void *)tid_INTRINSIC);
	TokStrTbl->AddObject("INTENT",(void *)tid_INTENT);
	TokStrTbl->AddObject("OPTIONAL",(void *)tid_OPTIONAL);
	TokStrTbl->AddObject("WHERE",(void *)tid_WHERE);
	TokStrTbl->AddObject("EQUIVALENCE",(void *)tid_EQUIVALENCE);
	// 動的記憶割付け
	TokStrTbl->AddObject("ALLOCATE",(void *)tid_ALLOCATE);
	TokStrTbl->AddObject("DEALLOCATE",(void *)tid_DEALLOCATE);
	TokStrTbl->AddObject("NULLIFY",(void *)tid_NULLIFY);
    // 制御文
	TokStrTbl->AddObject("IF",(void *)tid_IF);
	TokStrTbl->AddObject("THEN",(void *)tid_THEN);
	TokStrTbl->AddObject("ELSE",(void *)tid_ELSE);
	TokStrTbl->AddObject("ELSEIF",(void *)tid_ELSEIF);
	TokStrTbl->AddObject("ELSE IF",(void *)tid_ELSEIF);
	TokStrTbl->AddObject("ENDIF",(void *)tid_ENDIF);
	TokStrTbl->AddObject("END IF",(void *)tid_ENDIF);
	TokStrTbl->AddObject("DO",(void *)tid_DO);
	TokStrTbl->AddObject("ENDDO",(void *)tid_ENDDO);
	TokStrTbl->AddObject("END DO",(void *)tid_ENDDO);
	TokStrTbl->AddObject("SELECT",(void *)tid_SELECT);
	TokStrTbl->AddObject("CASE",(void *)tid_CASE);
	TokStrTbl->AddObject("GOTO",(void *)tid_GOTO);
	TokStrTbl->AddObject("GO TO",(void *)tid_GOTO);
	TokStrTbl->AddObject("DOWHILE",(void *)tid_DOWHILE);
	TokStrTbl->AddObject("DO WHILE",(void *)tid_DOWHILE);
	TokStrTbl->AddObject("CALL",(void *)tid_CALL);
	TokStrTbl->AddObject("RETURN",(void *)tid_RETURN);
	TokStrTbl->AddObject("STOP",(void *)tid_STOP);
	TokStrTbl->AddObject("END",(void *)tid_END);
    // 入出力
	TokStrTbl->AddObject("READ",(void *)tid_READ);
	TokStrTbl->AddObject("WRITE",(void *)tid_WRITE);
	TokStrTbl->AddObject("PRINT",(void *)tid_PRINT);
	TokStrTbl->AddObject("BACKSPACE",(void *)tid_BACKSPACE);
	TokStrTbl->AddObject("INQUIRE",(void *)tid_INQUIRE);
	TokStrTbl->AddObject("REWIND",(void *)tid_REWIND);
	TokStrTbl->AddObject("CLOSE",(void *)tid_CLOSE);
	TokStrTbl->AddObject("OPEN",(void *)tid_OPEN);
	TokStrTbl->AddObject("ENDFILE",(void *)tid_ENDFILE);
	TokStrTbl->AddObject("FORMAT",(void *)tid_FORMAT);

	// その他
	TokStrTbl->AddObject("ASSOCIATED",(void *)tid_ASSOCIATED);
	TokStrTbl->AddObject("CONTINUE",(void *)tid_CONTINUE);
	TokStrTbl->AddObject("CYCLE",(void *)tid_CYCLE);
	TokStrTbl->AddObject("DATA",(void *)tid_DATA);
	TokStrTbl->AddObject("DIMENSION",(void *)tid_DIMENSION);
	TokStrTbl->AddObject("EXIT",(void *)tid_EXIT);
	TokStrTbl->AddObject("PARAMETER",(void *)tid_PARAMETER);

	// キーワード（文字）
	TokStrTbl->AddObject(",",(void *)tid_Kannma);
	TokStrTbl->AddObject(";",(void *)tid_Semikoron);
	TokStrTbl->AddObject(":",(void *)tid_Koron);
	TokStrTbl->AddObject("::",(void *)tid_KoronKoron);

	// 演算子
	TokStrTbl->AddObject("=",(void *)tid_Set);
	TokStrTbl->AddObject("(",(void *)tid_Kakko);
	TokStrTbl->AddObject(")",(void *)tid_Kokka);
	TokStrTbl->AddObject("+",(void *)tid_Opr);
	TokStrTbl->AddObject("-",(void *)tid_Opr);
	TokStrTbl->AddObject("*",(void *)tid_Opr);
	TokStrTbl->AddObject("/",(void *)tid_Opr);
	TokStrTbl->AddObject("**",(void *)tid_Opr);
	TokStrTbl->AddObject("<",(void *)tid_Opr);
	TokStrTbl->AddObject(".LT.",(void *)tid_Opr);
	TokStrTbl->AddObject("<=",(void *)tid_Opr);
	TokStrTbl->AddObject(".LR.",(void *)tid_Opr);
	TokStrTbl->AddObject(">",(void *)tid_Opr);
	TokStrTbl->AddObject(".GT.",(void *)tid_Opr);
	TokStrTbl->AddObject(">=",(void *)tid_Opr);
	TokStrTbl->AddObject(".GE.",(void *)tid_Opr);
	TokStrTbl->AddObject("==",(void *)tid_Opr);
	TokStrTbl->AddObject(".EQ.",(void *)tid_Opr);
	TokStrTbl->AddObject("/=",(void *)tid_Opr);
	TokStrTbl->AddObject(".NE.",(void *)tid_Opr);

	TokStrTbl->AddObject(".NOT.",(void *)tid_Opr);
	TokStrTbl->AddObject(".AND.",(void *)tid_Opr);
	TokStrTbl->AddObject(".OR.",(void *)tid_Opr);
	TokStrTbl->AddObject(".EQV.",(void *)tid_Opr);
	TokStrTbl->AddObject(".NEQV.",(void *)tid_Opr);
	TokStrTbl->AddObject("//",(void *)tid_Opr);

	TokStrTbl->Sort(true); // 大文字小文字なしでの比較を行う。

}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TokStrTbl   作成するトクーン名とTokIdの対応テーブル格納先。
//
//  3.概要
//    C用のトークン名と、TokIdの対応テーブルを作成する。
//    作成したテーブルは、字句解析で使用される。
//
//  4.機能説明
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void MakeTokStrTbl_C(TStringList *TokStrTbl)
{
	TokStrTbl->Clear();
	//
	//	プリプロセッサ
	//
	TokStrTbl->AddObject("#include",(void *)tid_SharpInclude);
	TokStrTbl->AddObject("#define",(void *)tid_SharpDefine);
	TokStrTbl->AddObject("#undef",(void *)tid_SharpUnDef);
	TokStrTbl->AddObject("#if",(void *)tid_SharpIf);
	TokStrTbl->AddObject("#else",(void *)tid_SharpElse);
	TokStrTbl->AddObject("#elif",(void *)tid_SharpElIf);
	TokStrTbl->AddObject("#endif",(void *)tid_SharpEndIf);
	TokStrTbl->AddObject("#ifdef",(void *)tid_SharpIfDef);
	TokStrTbl->AddObject("#ifndef",(void *)tid_SharpIfnDef);
	TokStrTbl->AddObject("#pragma",(void *)tid_SharpPragma);
	//
	// キーワード（ABC順)
	//
	TokStrTbl->AddObject("auto",(void *)tid_auto);
	TokStrTbl->AddObject("break",(void *)tid_break);
	TokStrTbl->AddObject("case",(void *)tid_case);
	TokStrTbl->AddObject("char",(void *)tid_char);
	TokStrTbl->AddObject("const",(void *)tid_const);
	TokStrTbl->AddObject("continue",(void *)tid_continue);
	TokStrTbl->AddObject("default",(void *)tid_default);
	TokStrTbl->AddObject("do",(void *)tid_do);
	TokStrTbl->AddObject("double",(void *)tid_double);
	TokStrTbl->AddObject("else",(void *)tid_else);
	TokStrTbl->AddObject("enum",(void *)tid_enum);
	TokStrTbl->AddObject("extern",(void *)tid_extern);
	TokStrTbl->AddObject("float",(void *)tid_float);
	TokStrTbl->AddObject("for",(void *)tid_for);
	TokStrTbl->AddObject("goto",(void *)tid_goto);
	TokStrTbl->AddObject("if",(void *)tid_if);
	TokStrTbl->AddObject("incline",(void *)tid_inline);
	TokStrTbl->AddObject("int",(void *)tid_int);
	TokStrTbl->AddObject("long",(void *)tid_long);
	TokStrTbl->AddObject("register",(void *)tid_register);
	TokStrTbl->AddObject("restrict",(void *)tid_restrict);
	TokStrTbl->AddObject("return",(void *)tid_return);
	TokStrTbl->AddObject("short",(void *)tid_short);
	TokStrTbl->AddObject("signed",(void *)tid_signed);
	TokStrTbl->AddObject("sizeof",(void *)tid_sizeof);
	TokStrTbl->AddObject("static",(void *)tid_static);
	TokStrTbl->AddObject("struct",(void *)tid_struct);
	TokStrTbl->AddObject("switch",(void *)tid_switch);
	TokStrTbl->AddObject("typedef",(void *)tid_typedef);
	TokStrTbl->AddObject("union",(void *)tid_union);
	TokStrTbl->AddObject("unsigned",(void *)tid_unsigned);
	TokStrTbl->AddObject("void",(void *)tid_void);
	TokStrTbl->AddObject("volatile",(void *)tid_volatile);
	TokStrTbl->AddObject("while",(void *)tid_while);
	TokStrTbl->AddObject("_Bool",(void *)tid__Bool);
	TokStrTbl->AddObject("_Complex",(void *)tid__Complex);
	TokStrTbl->AddObject("_Imaginary",(void *)tid__Imaginary);

	// キーワード（文字）
	TokStrTbl->AddObject(",",(void *)tid_Kannma);
	TokStrTbl->AddObject(";",(void *)tid_Semikoron);
	TokStrTbl->AddObject(":",(void *)tid_Koron);
	TokStrTbl->AddObject("#",(void *)tid_SharpStart);

	// 演算子
	TokStrTbl->AddObject("=",(void *)tid_Set);
	TokStrTbl->AddObject("(",(void *)tid_Kakko);
	TokStrTbl->AddObject(")",(void *)tid_Kokka);
	TokStrTbl->AddObject("[",(void *)tid_KagiKakko);
	TokStrTbl->AddObject("]",(void *)tid_KagiKokka);
	TokStrTbl->AddObject("{",(void *)tid_DaiKakko);
	TokStrTbl->AddObject("}",(void *)tid_DaiKokka);
	TokStrTbl->AddObject(".",(void *)tid_Dot);
	TokStrTbl->AddObject("->",(void *)tid_Opr);
	TokStrTbl->AddObject("++",(void *)tid_Opr);
	TokStrTbl->AddObject("--",(void *)tid_Opr);
	TokStrTbl->AddObject("&",(void *)tid_Opr);
	TokStrTbl->AddObject("*",(void *)tid_Opr);
	TokStrTbl->AddObject("+",(void *)tid_Opr);
	TokStrTbl->AddObject("-",(void *)tid_Opr);
	TokStrTbl->AddObject("~",(void *)tid_Opr);
	TokStrTbl->AddObject("!",(void *)tid_Opr);
	TokStrTbl->AddObject("/",(void *)tid_Opr);
	TokStrTbl->AddObject("%",(void *)tid_Opr);

	TokStrTbl->AddObject("<<.",(void *)tid_Opr);
	TokStrTbl->AddObject(">>.",(void *)tid_Opr);
	TokStrTbl->AddObject("<",(void *)tid_Opr);
	TokStrTbl->AddObject("<=",(void *)tid_Opr);
	TokStrTbl->AddObject(">",(void *)tid_Opr);
	TokStrTbl->AddObject(">=",(void *)tid_Opr);
	TokStrTbl->AddObject("==",(void *)tid_Opr);
	TokStrTbl->AddObject("!=",(void *)tid_Opr);

	TokStrTbl->AddObject("^",(void *)tid_Opr);
	TokStrTbl->AddObject("|",(void *)tid_Opr);
	TokStrTbl->AddObject("&&",(void *)tid_Opr);
	TokStrTbl->AddObject("||",(void *)tid_Opr);

	TokStrTbl->AddObject("?",(void *)tid_Opr);
	TokStrTbl->AddObject(",",(void *)tid_Opr);

	TokStrTbl->AddObject("+=",(void *)tid_Set2);
	TokStrTbl->AddObject("-=",(void *)tid_Set2);
	TokStrTbl->AddObject("*=",(void *)tid_Set2);
	TokStrTbl->AddObject("/=",(void *)tid_Set2);
	TokStrTbl->AddObject("%=",(void *)tid_Set2);
	TokStrTbl->AddObject("<<=",(void *)tid_Set2);
	TokStrTbl->AddObject(">>=",(void *)tid_Set2);
	TokStrTbl->AddObject("&=",(void *)tid_Set2);
	TokStrTbl->AddObject("^=",(void *)tid_Set2);
	TokStrTbl->AddObject("|=",(void *)tid_Set2);

	TokStrTbl->Sort(false);	// 大文字・小文字の区別ありで比較を行う。
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    aStr    トークン文字列
//    Id  トークンＩＤ
//
//  3.概要
//    トークンを生成する。
//    文字列とＩＤを持つトークンが生成され、それ以外のデータが初期化される。他の
//    データはトークン生成後に呼び出し元から設定される。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TToken::TToken(string aStr,int Id)
{
	ChangedTokList = NULL;
	OrgStr = "";
	if(aStr == "\n"){
		Str = aStr;
	}else{
		Str = Trim(aStr);
	}
	TokId = (TTokId)Id;
	Idx = -1;
	LineNo = 0;
	ModuleIdx = 0;	// 何番目のモジュールかを示す。
	NestLevel = 0;	// DOやIFのNest深さ
	Indent = 0;
	ValData = NULL;
	Script = NULL;
	RefType = vrf_def;
	LineEndF = false;	// 行の終わり
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    トークンクラス破棄。作成したトークンを破棄する。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TToken::~TToken()
{
	if(ChangedTokList != NULL){
		for(int i = 0 ; i < ChangedTokList->Count ; i++){
			delete ((TToken *)ChangedTokList->Items[i]);
		}
		delete ChangedTokList;
	}
	if(Script != NULL){
		delete ((TScript *)Script);
	}
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    トークン種類を文字列で返す。
//
//  4.機能説明
//
//  5.戻り値
//    トークン種類の文字列。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TToken::GetTokIdStr()
{
	string s = "?";

	switch(TokId){
	case tid_null:
		s = "tid_ null";
		break;
	case tid_LineEnd:
		s = "tid_LineEnd";
		break;
	case tid_Comment:
		s = "tid_Comment";
		break;
//
//	プリプロセッサ
//
	case tid_SharpStart:
		s = "tid_SharpStart";
		break;
	case tid_SharpInclude:
		s = "tid_SharpInclude";
		break;
	case tid_SharpDefine:
		s = "tid_SharpDefine";
		break;
	case tid_SharpUnDef:
		s = "tid_SharpUnDef";
		break;
	case tid_SharpIf:	
		s = "tid_SharpIf";
		break;
	case tid_SharpElse:
		s = "tid_SharpElse";
		break;
	case tid_SharpElIf:
		s = "tid_SharpElIf";
		break;
	case tid_SharpEndIf:
		s = "tid_SharpEndIf";
		break;
	case tid_SharpIfDef:
		s = "tid_SharpIfDef";
		break;
	case tid_SharpIfnDef:
		s = "tid_SharpIfnDef";
		break;
	case tid_SharpPragma:
		s = "tid_SharpPragma";
		break;
	case tid_SharpEnd:
			s = "tid_SharpEnd";
		break;

//
//	キーワード
//
	case tid_KeyWordStart:
		s = "tid_KeyWordStart";
		break;
	case tid_auto:
		s = "tid_auto";
		break;
	case tid_break:
		s = "tid_break";
		break;
	case tid_case:
		s = "tid_case";
		break;
	case tid_char:
		s = "tid_char";
		break;
	case tid_const:
		s = "tid_const";
		break;
	case tid_continue:
		s = "tid_continue";
		break;
	case tid_default:
		s = "tid_default";
		break;
	case tid_do:
		s = "tid_do";
		break;
	case tid_double:
		s = "tid_double";
		break;
	case tid_else:
		s = "tid_else";
		break;
	case tid_enum:
		s = "tid_enum";
		break;
	case tid_extern:
		s = "tid_extern";
		break;
	case tid_float:
		s = "tid_float";
		break;
	case tid_for:
		s = "tid_for";
		break;
	case tid_goto:
		s = "tid_goto";
		break;
	case tid_if:
		s = "tid_if";
		break;
	case tid_inline:
		s = "tid_inline";
		break;
	case tid_int:
		s = "tid_int";
		break;
	case tid_long:
		s = "tid_long";
		break;
	case tid_register:
		s = "tid_register";
		break;
	case tid_restrict:
		s = "tid_restrict";
		break;
	case tid_return:
		s = "tid_return";
		break;
	case tid_short:
		s = "tid_short";
		break;
	case tid_signed:
		s = "tid_signed";
		break;
	case tid_sizeof:
		s = "tid_sizeof";
		break;
	case tid_static:
		s = "tid_static";
		break;
	case tid_struct:
		s = "tid_struct";
		break;
	case tid_switch:
		s = "tid_switch";
		break;
	case tid_typedef:
		s = "tid_typedef";
		break;
	case tid_union:
		s = "tid_union";
		break;
	case tid_unsigned:
		s = "tid_unsigned";
		break;
	case tid_void:
		s = "tid_void";
		break;
	case tid_volatile:
		s = "tid_volatile";
		break;
	case tid_while:
		s = "tid_while";
		break;
	case tid__Bool:
		s = "tid__Bool";
		break;
	case tid__Complex:
		s = "tid__Complex";
		break;
	case tid__Imaginary:
		s = "tid__Imaginary";
		break;

	// キーワード（文字）
	case tid_Kannma:
		s = "tid_Kannma";
		break;
	case tid_Semikoron:
		s = "tid_Semikoron";
		break;
	case tid_Semikoron2:
		s = "tid_Semikoron2";
		break;
	case tid_Koron:
		s = "tid_Koron";
		break;

	//
	// 演算子 解析を行うならば、区別と演算優先順位が必要。
	// 最初は単語区切りで tid_Oprとする。
	//
	case tid_Set:
		s = "tid_Set";
		break;
	case tid_Kakko:
		s = "tid_Kakko";
		break;
	case tid_Kokka:
		s = "tid_Kokka";
		break;
	case tid_KagiKakko:
		s = "tid_KagiKakko";
		break;
	case tid_KagiKokka:
		s = "tid_KagiKokka";
		break;
	case tid_DaiKakko:
		s = "tid_DaiKakko";
		break;
	case tid_DaiKokka:
		s = "tid_DaiKokka";
		break;
	case tid_Dot:
		s = "tid_Dot";
		break;
	case tid_Set2:
		s = "tid_Set2";
		break;

	// 演算子
	case tid_KoronKoron:
		s = "tid_KoronKoron";
		break;

	case tid_Opr:
		s = "tid_Opr";
		break;
	case tid_Val:
		s = "tid_Val";
		break;
	case tid_Func:
		s = "tid_Func";
		break;

	case tid_DataType:
		s = "tid_DataType";
		break;

	//
	//	この下は、主にFortran用
	//
	case tid_ConstNum:
		s = "tid_ConstNum";
		break;

	//
	// 字句解析結果からのトークン種別
	// 一部の文字列は、関数としてまとめて扱われる。
	//
	case tid_ConstStr:
		s = "tid_ConstStr";
		break;
	case tid_ConstInt:
		s = "tid_ConstInt";
		break;
	case tid_ConstReal:
		s = "tid_ConstReal";
		break;
	case tid_Label:
		s = "tid_Label";
		break;

	case tid_ContinuedLineStr:
		s = "tid_ContinuedLineStr";
		break;

	//
	// Pass1_5()で生成
	//

	// Program関連
	case tid_PROGRAM:
		s = "tid_PROGRAM";
		break;
	case tid_SUBROUTINE:
		s = "tid_SUBROUTINE";
		break;
	case tid_FUNCTION:
		s = "tid_FUNCTION";
		break;
	case tid_MODULE:
		s = "tid_MODULE";
		break;
	case tid_INTERFACE:
		s = "tid_INTERFACE";
		break;
	case tid_CONTAINS:
		s = "tid_CONTAINS";
		break;
	case tid_USE:
		s = "tid_USE";
		break;
	case tid_INCLUDE:
		s = "tid_INCLUDE";
		break;

	// データ宣言
	case tid_COMMON:
		s = "tid_COMMON";
		break;
	case tid_INTEGER:
		s = "tid_INTEGER";
		break;
	case tid_REAL:
		s = "tid_REAL";
		break;
	case tid_DOUBLEPRECISION:
		s = "tid_DOUBLEPRECISION";
		break;
	case tid_CHARACTER:
		s = "tid_CHARACTER";
		break;
	case tid_LOGICAL:
		s = "tid_LOGICAL";
		break;
	case tid_COMPLEX:
		s = "tid_COMPLEX";
		break;
	case tid_IMPLICIT:
		s = "tid_IMPLICIT";
		break;
	case tid_TYPE:
		s = "tid_TYPE";
		break;
	case tid_TYPE_REF:
		s = "tid_TYPE_REF";
		break;
	case tid_NAMELIST:
		s = "tid_NAMELIST";
		break;
	case tid_ALLOCATABLE:
		s = "tid_ALLOCATABLE";
		break;
	case tid_POINTER:
		s = "tid_POINTER";
		break;
	case tid_TARGET:
		s = "tid_TARGET";
		break;
	case tid_SAVE:
		s = "tid_SAVE";
		break;
	case tid_PRIVATE:
		s = "tid_PRIVATE";
		break;
	case tid_PUBLIC:
		s = "tid_PUBLIC";
		break;
	case tid_EXTERNAL:
		s = "tid_EXTERNAL";
		break;
	case tid_INTRINSIC:
		s = "tid_INTRINSIC";
		break;
	case tid_INTENT:
		s = "tid_INTENT";
		break;
	case tid_OPTIONAL:
		s = "tid_OPTIONAL";
		break;
	case tid_WHERE:
		s = "tid_WHERE";
		break;
	case tid_EQUIVALENCE:		s = "tid_EQUIVALENCE";
		break;

	//動的記憶割付け
	case tid_ALLOCATE:
		s = "tid_ALLOCATE";
		break;
	case tid_DEALLOCATE:
		s = "tid_DEALLOCATE";
		break;
	case tid_NULLIFY:
		s = "tid_NULLIFY";
		break;

	// 制御文
	case tid_IF:
		s = "tid_IF";
		break;
	case tid_THEN:
		s = "tid_THEN";
		break;
	case tid_ELSE:
		s = "tid_ELSE";
		break;
	case tid_ELSEIF:
		s = "tid_ELSEIF";
		break;
	case tid_ENDIF:
		s = "tid_ENDIF";
		break;
	case tid_DO:
		s = "tid_DO";
		break;
	case tid_ENDDO:
		s = "tid_ENDDO";
		break;
	case tid_SELECT:
		s = "tid_SELECT";
		break;
	case tid_CASE:
		s = "tid_CASE";
		break;
	case tid_GOTO:
		s = "tid_GOTO";
		break;
	case tid_DOWHILE:
		s = "tid_DOWHILE";
		break;
	case tid_CALL:
		s = "tid_CALL";
		break;
	case tid_RETURN:
		s = "tid_RETURN";
		break;
	case tid_STOP:
		s = "tid_STOP";
		break;
	case tid_END:
		s = "tid_END";
		break;
	case tid_END_STR:
		s = "tid_END_STR";
		break;

	// 入出力
	case tid_READ:
		s = "tid_READ";
		break;
	case tid_WRITE:
		s = "tid_WRITE";
		break;
	case tid_PRINT:
		s = "tid_PRINT";
		break;
	case tid_BACKSPACE:
		s = "tid_BACKSPACE";
		break;
	case tid_INQUIRE:
		s = "tid_INQUIRE";
		break;
	case tid_REWIND:
		s = "tid_REWIND";
		break;
	case tid_CLOSE:
		s = "tid_CLOSE";
		break;
	case tid_OPEN:
		s = "tid_OPEN";
		break;
	case tid_ENDFILE:
		s = "tid_ENDFILE";
		break;
	case tid_FORMAT:
		s = "tid_FORMAT";
		break;

	// その他
	case tid_ASSOCIATED:
		s = "tid_ASSOCIATED";
		break;
	case tid_CONTINUE:		s = "tid_CONTINUE";
		break;
	case tid_CYCLE:			s = "tid_CYCLE";
		break;
	case tid_DATA:			s = "tid_DATA";
		break;
	case tid_DIMENSION:		s = "tid_DIMENSION";
		break;
	case tid_EXIT:			s = "tid_EXIT";
		break;
	case tid_PARAMETER:		s = "tid_PARAMETER";
		break;

	}
	return s;
}


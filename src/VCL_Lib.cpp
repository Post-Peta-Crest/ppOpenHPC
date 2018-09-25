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

 //---------------------------------------------------------------------------

#include "VCL_Lib.h"

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//	  C++Builder,VCL相当の機能をC++で使うための関数
//
//  3.機能説明
//    Listや文字列List操作等を行う。
//
//  4.備考
//
/*----------------------------------------------------------------------------*/

using namespace std;

int g_Deb = 0;

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    クラス TList
//
//  4.機能説明
//	  void *Dataのリストデータを扱う
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TList::TList()
{
	Count = 0;
}
int TList::Add(void *Data)
{
	Items.push_back(Data);
	Count = Items.size();
	return Count;
}
void TList::Delete(int Idx)
{
	vector<void *>::iterator it = Items.begin();
	Items.erase(it+Idx);
	Count = Items.size();
}
void TList::Remove(void *Data)
{
	vector<void *>::iterator it = find(Items.begin(),Items.end(),Data);  // イテレータのインスタンス化
	if(it < Items.end()){
		Items.erase(it);
		Count = Items.size();
	}
}
void TList::Clear()
{
	Items.clear();
	Count = 0;
}
int TList::IndexOf(void *Data)
{
	vector<void *>::iterator it = find(Items.begin(),Items.end(),Data);  // イテレータのインスタンス化
	if(it < Items.end()){
		int Pos = it - Items.begin();
		return Pos;
	}
	return -1;
}
void TList::Exchange(int Idx1,int Idx2)
{
	void *Temp = Items[Idx1];
	Items[Idx1] = Items[Idx2];
	Items[Idx2] = Temp;
}
void TList::Insert(int Idx,void *Data)
{
	Items.insert(Items.begin()+Idx,Data);
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    クラス TStringList
//
//  4.機能説明
//	  文字列とvoid *Dataのリストデータを扱う
//	　名前順のSortや検索、読み込み等も行う
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TStringList::TStringList()
{
	SortMode = 0;
	Count = 0;
}
int TStringList::Add(string str)
{
	Strings.push_back(str);
	Objects.push_back(NULL);
	Count = Strings.size();
	return Count;
}
int TStringList::AddObject(string str,void *Data)
{
	Strings.push_back(str);
	Objects.push_back(Data);
	Count = Strings.size();
	return Count;
}
void TStringList::Delete(int Idx)
{
	vector<string>::iterator it = Strings.begin();
	vector<void *>::iterator it2 = Objects.begin();
	Strings.erase(it+Idx);
	Objects.erase(it2+Idx);
	Count = Strings.size();
}
void TStringList::Remove(string str)
{
	vector<string>::iterator it = find(Strings.begin(),Strings.end(),str);  // イテレータのインスタンス化
	vector<void *>::iterator it2 = Objects.begin();
	if(it < Strings.end()){
		it2 += it-Strings.begin();
		Strings.erase(it);
		Objects.erase(it2);
		Count = Strings.size();
	}
}
void TStringList::Clear()
{
	Strings.clear();
	Objects.clear();
	Count = 0;
}
int TStringList::IndexOf(string str)
{
	vector<string>::iterator it = find(Strings.begin(),Strings.end(),str);  // イテレータのインスタンス化
	if(it < Strings.end()){
		int Pos = it - Strings.begin();
		return Pos;
	}
	return -1;
}
int TStringList::IndexOfObject(void *Data)
{
	vector<void *>::iterator it = find(Objects.begin(),Objects.end(),Data);  // イテレータのインスタンス化
	if(it < Objects.end()){
		int Pos = it - Objects.begin();
		return Pos;
	}
	return -1;
}
//
// 一致する文字列を検索する。Sort済であること。
//
int TStringList::Find(string str,int &Idx)
{
	vector<string>::iterator it = lower_bound( Strings.begin(), Strings.end(), str );
	if(it < Strings.end()){
		int Pos = it - Strings.begin();
		if(Strings[Pos] == str){ // 一致があった場合
			Idx = Pos;
			return true;
		}
	}
	Idx = -1;
	return false;
}
//
// 大文字・小文字と無関係に一致する文字列を検索する。
// Tblをすべて小文字とした上でSort済であること。
//
int TStringList::FindIC(string str,int &Idx)
{
	str = LowerCase(str);
	vector<string>::iterator it = lower_bound( Strings.begin(), Strings.end(), str );
	if(it < Strings.end()){
		int Pos = it - Strings.begin();
		if(Strings[Pos] == str){ // 一致があった場合
			Idx = Pos;
			return true;
		}
	}
	Idx = -1;
	return false;
}
void TStringList::Exchange(int Idx1,int Idx2)
{
	string tempStr;
	void *tempData;

	tempStr = Strings[Idx1];
	tempData = Objects[Idx1];
	Strings[Idx1] = Strings[Idx2];
	Objects[Idx1] = Objects[Idx2];
	Strings[Idx2] = tempStr;
	Objects[Idx2] = tempData;
	SortMode = 0;
}
void TStringList::Insert(int Idx,string str)
{
	Strings.insert(Strings.begin()+Idx,str);
	Objects.insert(Objects.begin()+Idx,NULL);
	SortMode = 0;
	Count = Strings.size();
}
void TStringList::InsertObject(int Idx,string str,void *Data)
{
	Strings.insert(Strings.begin()+Idx,str);
	Objects.insert(Objects.begin()+Idx,Data);
	SortMode = 0;
	Count = Strings.size();
}
void TStringList::Assign(TStringList *SrcStrList)
{
	Strings.assign(SrcStrList->Strings.begin(),SrcStrList->Strings.end());
	Objects.assign(SrcStrList->Objects.begin(),SrcStrList->Objects.end());
	SortMode = SrcStrList->SortMode;
	Count = SrcStrList->Count;
}

//
// ファイルから複数行のテキストを読み込む。
// UTF-8データの読み込みに使用する。ストリームでstringに読み込む形とする。
//
void TStringList::LoadFromFile(string FileName)
{
	ifstream ifs(FileName.c_str());
	string buf;

	while(ifs && getline(ifs, buf)){
		Add(buf);
	}
	SortMode = 0;
}
//
//	文字列の並べ替えを行う。Objects[]の並べ替えも必要なので
//	文字列の最後に\t数値をつけてからSortして戻す。最大 255*255 = 65025まで対応
//
void TStringList::Sort(bool ChangeToLowerCaseF)
{
	int i;
	unsigned char c[4];
	string s,s2;
	int Idx;
	long *TempTbl;

	TempTbl = new long[Strings.size()];
	c[0] = '\t';
	c[3] = 0;
	if(!ChangeToLowerCaseF){
		SortMode = 1;
		for(i = 0 ; i < (int)Strings.size() ; i++){
			c[1] = (unsigned char)(1+(i/255));
			c[2] = (unsigned char)(1+(i%255));
			Strings[i] += (char *)c;
			TempTbl[i] = (long)Objects[i];
		}
	}else{
		SortMode = 2;
		for(i = 0 ; i < (int)Strings.size() ; i++){
			c[1] = (unsigned char)(1+(i/255));
			c[2] = (unsigned char)(1+(i%255));
			Strings[i] = LowerCase(Strings[i]) + (char *)c;
			TempTbl[i] = (long)Objects[i];
		}
	}
	sort( Strings.begin(),Strings.end());
	for(i = 0 ; i < (int)Strings.size() ; i++){
		s = Strings[i];
		s2 = s.substr(s.length()-3);
		Strings[i].erase(s.length()-3);
		Idx = (s2[1] - 1)*255 + (s2[2] - 1);
		Objects[i] = (void *)TempTbl[Idx];
	}
	delete[] TempTbl;
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  Trim
//　　　
//  2.パラメタ説明
//	  str 元の文字列
//
//  3.概要
//    前後の空白とコントロール文字を削除する
//
//  4.機能説明
//
//  5.戻り値
//    前後の空白とコントロール文字を削除した文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string Trim(string str)
{
	int i;

	for(i = 0 ; i < (int)str.length() ; i++){
		if((unsigned char)str[i] > ' '){
			if(i != 0){
				str.erase(0,i);
			}
			break;
		}
	}
	for(i = str.length()-1 ; i >= 0 ; i--){
		if((unsigned char)str[i] > ' '){
			str.erase(i+1);
			break;
		}
	}
	if(i == -1){
		return "";
	}
	return str;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  TrimRigth
//　　　
//  2.パラメタ説明
//	  str 元の文字列
//
//  3.概要
//    後の空白とコントロール文字を削除する
//
//  4.機能説明
//
//  5.戻り値
//    後の空白とコントロール文字を削除した文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TrimRight(string str)
{
	int i;

	for(i = str.length()-1 ; i >= 0 ; i--){
		if((unsigned char)str[i] > ' '){
			str.erase(i+1);
			break;
		}
	}
	if(i == -1){
		return "";
	}
	return str;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  IntToStr
//　　　
//  2.パラメタ説明
//	  int Data データ
//
//  3.概要
//    整数 Dataを文字列に変換する。
//
//  4.機能説明
//
//  5.戻り値
//    変換した文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string IntToStr(int Data)
{
	char cs[100];
	sprintf(cs,"%d",Data);

	return cs;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  FloatToStr
//　　　
//  2.パラメタ説明
//	  double Data データ
//
//  3.概要
//    実数 Dataを文字列に変換する。
//
//  4.機能説明
//
//  5.戻り値
//    変換した文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string FloatToStr(double Data)
{
	char cs[100];
	sprintf(cs,"%f",Data);

	return cs;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  LowerCase
//　　　
//  2.パラメタ説明
//	  string str 変換元データ
//
//  3.概要
//    1バイト文字の大文字を小文字に変換する。
//
//  4.機能説明
//
//  5.戻り値
//    変換した文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string LowerCase(string str)
{
	char c;
	string s2 = str;

	for(int i = 0 ; i < (int)str.length() ; i++){
		c = str[i];
		s2[i] = (char)tolower(c);
	}
	return s2;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  ファイルコピー
//　　　
//  2.パラメタ説明
//	  string SrcFileName 複写元ファイル名
//	  string DestFileName 複写先ファイル名
//
//  3.概要
//    ファイルを複写する
//
//  4.機能説明
//
//  5.戻り値
//    エラーの場合は０を成功の場合は１を返す。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
int CopyFile(const char *SrcFileName,const char *DestFileName)
{
	FILE *fp1,*fp2;
	char buff[1000];
	int len;

	fp1 = fopen(SrcFileName,"rb");
	if(fp1 == NULL){
		return 0;
	}
	fp2 = fopen(DestFileName,"wb");
	if(fp2 == NULL){
		fclose(fp1);
		return 0;
	}
	while(true){
		len = fread(buff,sizeof(buff),1,fp1);
		if(len <= 0){
			break;
		}
		len = fwrite(buff,len,1,fp1);
		if(len <= 0){
			fclose(fp1);
			fclose(fp2);
			return 0;
		}
	}
	fclose(fp1);
	fclose(fp2);
	return 1;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//	  ファイル存在確認
//　　　
//  2.パラメタ説明
//	  string SrcFileName ファイル名
//
//  3.概要
//    ファイルが存在するかどうかを調べる
//
//  4.機能説明
//
//  5.戻り値
//    存在していれば trueをそうでなければ falseを返す。
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
bool FileExists(string SrcFileName)
{
	FILE *fp1;

	fp1 = fopen(SrcFileName.c_str(),"rb");
	if(fp1 == NULL){
		return false;
	}
	fclose(fp1);
	return true;
}


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

#include <vector>
#include <string>
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

#ifndef VCL_LibH
#define VCL_LibH
//---------------------------------------------------------------------------
//
//	VCL相当のC++で使うためのライブラリ
//	必要に応じてメソッドを追加して行く。
//
//---------------------------------------------------------------------------
// リスト
class TList {
private:
public:
	std::vector<void *> Items;
	int Count;	// 常に操作のたびに更新する。

	TList();
	int Add(void *Data);
	void Delete(int Idx);
	void Remove(void *Data);
	void Clear();
	int IndexOf(void *Data);
	void Exchange(int Idx1,int Idx2);
	void Insert(int Idx,void *Data);

};
// 文字列リスト。
class TStringList {
private:
	int SortMode;
public:
	std::vector<string> Strings;
	std::vector<void *> Objects;
	int Count;	// 常に操作のたびに更新する。

	TStringList();
	int Add(string Str);
	int AddObject(string Str,void *Data);
	void Delete(int Idx);
	void Remove(string Str);
	void Clear();
	int IndexOf(string Str);
	int IndexOfObject(void *Data);
	int Find(string str,int &Idx);
	int FindIC(string str,int &Idx);

	void LoadFromFile(string FileName);
	void Sort(bool ChangeToLowerCaseF);
	void Exchange(int Idx1,int Idx2);
	void Insert(int Idx,string str);
	void InsertObject(int Idx,string str,void *Data);
	void Assign(TStringList *SrcStrList);

};

//
// 関数
//
string Trim(string str);
string TrimRight(string str);
string IntToStr(int Data);
string FloatToStr(double Data);
string LowerCase(string str);

int CopyFile(const char *SrcFileName,const char *DestFileName);
bool FileExists(string SrcFileName);

#endif

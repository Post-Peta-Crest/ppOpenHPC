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
//    パス４
//
//  作成者
//
/*----------------------------------------------------------------------------*/

#include "TuneRegion.h"

//
// 生成
//
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    aTokenList  トークンリスト
//    aValDataList  変数データリスト
//    aTuneRegionList 生成されるTuneRegionの格納先リスト
//
//  3.概要
//    パス４クラス生成。変数の初期化と、引数データの複写を行う。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TPass4::TPass4(TList *aTokenList,TList *aValDataList,TList *aTuneRegionList)
{
	string DirName;
	string fname;

	TokenList = aTokenList;
	ValDataList = aValDataList;
	TuneRegionList = aTuneRegionList;

	OAT_ValList = new TStringList;

	fname = MainF->SrcFileNameList.Strings[0];
	DirName = fname.substr(0,fname.rfind("/")+1);
	fname = fname.substr(fname.rfind("/")+1);
	if(DirName == ""){
		DirName = "./";
	}
//	fname = DirName + "\\OAT/OAT_"+ExtractFileName(fname);
	fname = DirName + "/OAT/OAT_" + fname;
}
//
// 破棄
//
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    パス４クラス破棄
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
TPass4::~TPass4()
{
	delete OAT_ValList;
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    スクリプトを実行してチューニングリージョンを生成し、その解析を行う。
//    解析結果は、各TTuneRegionクラスのデータとして保持される。
//    スクリプトを実行するエンジンとしての位置づけとなる。
//
//  4.機能説明
//    スクリプトを実行してチューニングリージョンを生成し、その解析を行う。
//    解析結果は、各TTuneRegionクラスのデータとして保持される。
//    複数のTrueRegionに対応するために、RegionごとのクラスとしてTuneRegionを生成
//    する形を取る。
//    スクリプトの解析も、Pass4で済ませ、Pass5では、TuneRegionクラスを参照してソ
//    ースを生成する。
//
//  5.戻り値
//    なし
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TPass4::Exec()
{
	int i,j;
    TToken *Token;
    TScript *Script;
	string s;
    TTuneRegion *TuneRegion;

    //
    // トークンのLoop
    //
    for(i = 0 ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
		if(Token->Script != NULL){
        	Script = (TScript *)Token->Script;
            if(Script->ScType == sct_command){
				//
				// ABCScript のコマンドが出力される。
				// ここで、OAT_xxx = の値の所得も行う。
                // 現状では、定数のみが使用可能。（変数や式はエラー）
                //
                s = Script->TokStrList->Strings[0];
				if(s.find("=") != string::npos){
					string ValName,ConstStr;
					long Data;

					ValName = Trim(s.substr(0,s.find("=")-1));
					ConstStr = Trim(s.substr(s.find("=")+1,s.length()));
					try{
                    	Data = atoi(ConstStr.c_str());
                    }catch(...){
//						MainF->err("OAT変数に整数定数以外が設定されています。 ["+s+"]");
						MainF->ErrMessage(i,"OAT variable is not const int. ["+s+"]");
						return;
                    }
                    // 変数に追加 ２回設定もOKとした。2010/08/26
					if(OAT_ValList->IndexOf(ValName) != -1){
//				     	MainF->err("OAT変数が２度設定されています。 ["+s+"]");
//                    	return;
                    }else{
                        OAT_ValList->AddObject(ValName,(void *)Data);
						MainF->print("OAT variable: "+ s);
					}
				}
				for(int j = 0 ; j < Token->Indent ; j++){
					s = "  "+s;
				}
			}else if(Script->ScRegion == scr_start){ 	// リージョン開始
				TuneRegion = new TTuneRegion(this,i); 	// リージョン生成
				Script->TuneRegion = TuneRegion;
				TuneRegionList->Add(TuneRegion);    	// リストに追加
            	// 基本パラメタ名表示を追加 2009/03/05
				s = "";
				for(int j = 0 ; j < TuneRegion->BaseValList->Count ; j++){
					if(s != ""){
						s += ",";
					}
					s += TuneRegion->BaseValList->Strings[j];

				}
				MainF->print("Base Paramater (BPset): "+ s);
			}
 		}
    }
    //
    // TuneRegionListをTuneRegion->Number順に並べる。
    //
    for(i = 0 ; i < TuneRegionList->Count ; i++){
	    for(j = i+1 ; j < TuneRegionList->Count ; j++){
			if(((TTuneRegion *)TuneRegionList->Items[i])->Number >
					((TTuneRegion *)TuneRegionList->Items[j])->Number){
				TuneRegionList->Exchange(i,j);
            }
        }
    }
}



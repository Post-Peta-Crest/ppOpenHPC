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

#include "Visual.h"
#ifdef _WIN32
	#include <dir.h>
#endif
#include <sys/stat.h>

TVisualDM::TVisualDM()
{

}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    ビジュアル化のためのファイルの生成を行う。これをメインとして、関数を呼び出
//    す。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TVisualDM::Exec()
{
	int i;
	string fname;
    TTuneRegion *TuneRegion;

	fname = MainF->SrcFileNameList.Strings[0];
	DirName = fname.substr(0,fname.rfind("/"));
	if(DirName == ""){
		DirName = "./";
	}
	TuneRegionList = MainF->TuneRegionList;
	MakeIndexHTML();

	ResetLogDataFile(NULL);	// 参照するLogデータをクリアする。
	for(i = 0 ; i < TuneRegionList->Count ; i++){
		TuneRegion = (TTuneRegion *)TuneRegionList->Items[i];
		ResetLogDataFile(TuneRegion);	// 参照するLogデータをクリアする。
		MakeResultHTML(TuneRegion);
	}

	CopyDataForHTML();	// HTML表示に必要なデータを複写する。
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TuneRegion
//
//  3.概要
//    参照するLogデータをクリアする。
//    各TuneRegionに対してのダミーのヘッダを持ったファイルが作成される。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TVisualDM::ResetLogDataFile(TTuneRegion *TuneRegion)
{
	int i;
	string DestFileName;
    FILE *fp;

   	if(TuneRegion == NULL){
		DestFileName = DirName + "/OAT/OATLog.dat";
	}else{
		DestFileName = DirName + "/OAT/OATLog_"+TuneRegion->Name+".dat";
	}
	fp = fopen(DestFileName.c_str(),"wt");
	if(fp == NULL){
		MainF->ErrMessage(-1,"File Create Err "+ DestFileName);
        return;
    }
   	if(TuneRegion == NULL){
	    for(i = 0 ; i < TuneRegionList->Count ; i++){
		    fprintf(fp,"0\n");
        }
    }else{
	    fprintf(fp,"1,NumProces (DummyData)\n");
    	fprintf(fp,"100,StartTuneSize (DummyData)\n");
	    fprintf(fp,"100,EndTuneSize (DummyData)\n");
    	fprintf(fp,"100,SampDist (DummyData)\n");
	    fprintf(fp,"-1,EndOfHeader\n");
    }
    fclose(fp);
}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    HTML表示に必要なデータを複写する。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TVisualDM::CopyDataForHTML()
{
	string SrcFileName,DestFileName;
    string SrcPath;

	SrcPath = "./OAT_data/";

	DestFileName = DirName + "/OAT/vlapplet.jar";
//	SrcFileName = SrcPath + ExtractFileName(DestFileName);
	SrcFileName = SrcPath + DestFileName.substr(DestFileName.rfind("/")+1);
	if(CopyFile(SrcFileName.c_str(),DestFileName.c_str()) == 0){
     	MainF->ErrMessage(-1,"File Copy Err "+ DestFileName);
        return;
    }
	DestFileName = DirName + "/OAT/bar0-";
//	if(!ForceDirectories(DestFileName)){

#ifdef _WIN32
	if(!mkdir(DestFileName.c_str())){
#else
	if(!mkdir(DestFileName.c_str(),S_IROTH | S_IRUSR | S_IRGRP | S_IWOTH | S_IWUSR | S_IWGRP )){
#endif
		MainF->ErrMessage(-1,"File Create Err "+ DestFileName);
        return;
    }
	DestFileName = DirName + "/OAT/bar0-/00.bin";
//	SrcFileName = SrcPath + "bar0-/"+ ExtractFileName(DestFileName);
	SrcFileName = SrcPath + "bar0-/"+ DestFileName.substr(DestFileName.rfind("/")+1);
	if(CopyFile(SrcFileName.c_str(),DestFileName.c_str()) == 0){
		MainF->ErrMessage(-1,"File Copy Err "+ DestFileName);
		return;
	}

	DestFileName = DirName + "/OAT/gtable-";
#ifdef _WIN32
	if(!mkdir(DestFileName.c_str())){
#else
	if(!mkdir(DestFileName.c_str(),S_IROTH | S_IRUSR | S_IRGRP | S_IWOTH | S_IWUSR | S_IWGRP)){
#endif
		MainF->ErrMessage(-1,"File Create Err "+ DestFileName);
		return;
	}
	DestFileName = DirName + "/OAT/gtable-/00.bin";
	SrcFileName = SrcPath + "bar0-/"+ DestFileName.substr(DestFileName.rfind("/")+1);
	if(CopyFile(SrcFileName.c_str(),DestFileName.c_str()) == 0){
     	MainF->ErrMessage(-1,"File Copy Err "+ DestFileName);
		return;
	}

}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//
//  3.概要
//    OAT_Index.htmlを作成する。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TVisualDM::MakeIndexHTML()
{
	FILE *fp;
	string FileName,s;
	string SrcFileName;
	TTuneRegion *TuneRegion;
	int i;
	TList *TokenList;
	TToken *Token;
	int RegionIdx = 0;

	TokenList = MainF->TokenList;

	FileName = DirName + "/OAT/OATLog.html";
	fp = fopen(FileName.c_str(),"wt");
	if(fp == NULL){
		MainF->ErrMessage(-1,"File Open Err "+ FileName);
		return;
	}
	//
	// HTMLファイルを出力する。
    //
    fprintf(fp,"<html>\n");
    fprintf(fp,"<head>\n");
	fprintf(fp,"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=shift_jis\">\n");
	fprintf(fp,"<meta name=\"GENERATOR\" content=\"OAT 1.0\">\n");
	fprintf(fp,"<title>OAT</title>\n");
    fprintf(fp,"</head>\n");

    fprintf(fp,"<body onLoad = \"JSStart()\" >\n");
	fprintf(fp,"<h1 align = \"center\"><b>OAT</b>\n");
	fprintf(fp,"</h1>\n");

    //
    // ファイル名
    //
	SrcFileName = MainF->SrcFileNameList.Strings[0];
	s = "SrcFile = " + DirName +"/"+ SrcFileName.substr(SrcFileName.rfind("/")+1);
	s = StrToHTMLStr(s);
    fprintf(fp,"<h3 align = \"center\"><b>%s</b>\n",s.c_str());
    fprintf(fp,"</h3>\n");

    //
    // 実行状態の表を出力
    //
    fprintf(fp,"<table border=\"1\" width=\"100%%\" height=\"%d\">\n",16*(TuneRegionList->Count+1));
    fprintf(fp,"<tr>\n");
   	fprintf(fp,"<td width=\"35%%\" height=\"14\">Auto tuning region name</td>\n");
    fprintf(fp,"<td width=\"30%%\" height=\"14\">Execute status</td>\n");
   	fprintf(fp,"<td width=\"25%%\" height=\"14\">Link to result page</td>\n");
    fprintf(fp,"</tr>\n");

    //
    // 各TuneRegionの名前・状態・リンクを出力
    //
    for(i = 0 ; i < TuneRegionList->Count ; i++){
		TuneRegion = (TTuneRegion *)TuneRegionList->Items[i];

	    fprintf(fp,"<tr>\n");
//    <td width="46%" height="16"><a href="#1" name="0">matmul1 [Select1.f :45- 56]</a></td>
    	fprintf(fp,"<td width=\"35%%\" height=\"16\"><a href=\"#%d\">%s</td>\n",i,TuneRegion->Name.c_str());
	    fprintf(fp,"<td width=\"30%%\" height=\"16\">\n");
		fprintf(fp,"<applet\n");
	    fprintf(fp,"codebase = \"./\"\n");
	    fprintf(fp,"code = \"vlapplet.Applet1.class\"\n");
	    fprintf(fp,"archive = \"vlapplet.jar\"\n");

	    fprintf(fp,"name = \"bar%d\"\n",i);
	    fprintf(fp,"width = \"100%%\"\n");
	    fprintf(fp,"height = \"100%%\"\n");
	    fprintf(fp,"alt = \"Java Applet\">\n");
	    fprintf(fp,"<param name=\"FolderName\" value=\"bar0-\">\n");
//	    fprintf(fp,"<param name=\"BackColor\" value=\"16777215\">\n");
		if(i == 0){ // Table読込みは、最初のAppletのみ
			fprintf(fp,"<param name=\"FILENAME\" value=\"OATLog.dat\">\n");
		}
	    fprintf(fp,"</applet></td>\n");

	    fprintf(fp,"<td width=\"25%%\" height=\"16\" align=\"center\">\n");
	    fprintf(fp,"<p align=\"center\">");
		fprintf(fp,"<a href=\"./OATLog_%s.html\" target=\"_blank\">Result</a></td>\n",
					TuneRegion->Name.c_str());
		fprintf(fp,"</tr>\n");
	}
    fprintf(fp,"</table>\n");
    fprintf(fp,"\n");
//  <p>


	//
    // JavaScript部分を出力
    //
    fprintf(fp,"<script type=\"text/javascript\">\n");
    fprintf(fp,"<!--\n");
    fprintf(fp,"var id = -1;\n");
    for(i = 0 ; i < TuneRegionList->Count ; i++){
	    fprintf(fp,"var id%d = -1;\n",i);
	}

    fprintf(fp,"function JSStart() {\n");
    fprintf(fp,"if(id == -1){\n");
    fprintf(fp,"id = document.bar0.getId(\"Table@d\");\n");
    for(i = 0 ; i < TuneRegionList->Count ; i++){
	    fprintf(fp,"id%d = document.bar%d.getId(\"@p\");\n",i,i);
	}
    fprintf(fp,"}\n");
    for(i = 0 ; i < TuneRegionList->Count ; i++){
	    fprintf(fp,"bar%d.put(id%d,document.bar0.get(id,%d)/100.0);\n",i,i,i);
    }
    fprintf(fp,"window.setTimeout(\"JSStart()\", 1000);\n");
    fprintf(fp,"}\n");
    fprintf(fp,"// -->\n");
    fprintf(fp,"</script>\n");
    fprintf(fp,"<noscript>\n");
    fprintf(fp,"JavaScript対応ブラウザで表示してください。</p>\n");
    fprintf(fp,"<p></noscript>\n");
    fprintf(fp,"<br>\n");


	//
    // ソースコード＋反転＋Ｌｉｎｋを出力
    // また、スペースや<>"を置換して出力する。
    // さらにリージョンの深さに対応したインデントを付加する。
    //
	string ss;
    TScript *Script;
    RegionIndent = 0;
    int RegionIdxStack[1024];

    ss = "";
    for(i = 0 ; i < TokenList->Count ; i++){
		Token = (TToken *)TokenList->Items[i];
        s = Token->OrgStr;
        if(Token->TokId == tid_null){

        }else if(Token->TokId == tid_LineEnd){
			if(ss != ""){
	            for(int ii = 0 ; ii < RegionIndent ; ii++){
				    fprintf(fp,"&nbsp&nbsp");	// インデント+2
        	    }
		    	fprintf(fp,"%s<br>\n",ss.c_str());
            }
            ss = "";
        }else if(Token->Script != NULL){
        	Script = (TScript *)Token->Script;
	       	if(Script->ScRegion == scr_start){
				TuneRegion = (TTuneRegion *)TuneRegionList->Items[RegionIdx];
		        ss += "<i><a name=\""+IntToStr(RegionIdx)+"\">";
				ss += "<a href=\"./OATLog_"+TuneRegion->Name+".html\"  target=\"_blank\"" ;
				ss += "<font color=\"#0000FF\">"+StrToHTMLStr(s)+"</font>";
		        ss += "</i></a>";
	            for(int ii = 0 ; ii < RegionIndent ; ii++){
				    fprintf(fp,"&nbsp&nbsp");	// インデント+2
        	    }
			    fprintf(fp,"%s<br>\n",ss.c_str());
                ss = "";
    			RegionIdxStack[RegionIndent] = RegionIdx;
			    RegionIdx++;
    			RegionIndent++;
	       	}else if(Script->ScRegion == scr_end){
    			RegionIndent--;
#if 0
		        ss += "<i><font color=\"#0000FF\">"+StrToHTMLStr(s)+"</font></i>";
#else
				TuneRegion = (TTuneRegion *)TuneRegionList->Items[RegionIdxStack[RegionIndent]];
		        ss += "<i><a name=\""+IntToStr(RegionIdx)+"\">";
				ss += "<a href=\"./OATLog_"+TuneRegion->Name+".html\"  target=\"_blank\"" ;
				ss += "<font color=\"#0000FF\">"+StrToHTMLStr(s)+"</font>";
				ss += "</i></a>";
#endif

	       	}else if(Script->ScRegion == scr_substart){
		        ss += "<i><font color=\"#008000\">"+StrToHTMLStr(s)+"</font></i>";

	            for(int ii = 0 ; ii < RegionIndent ; ii++){
				    fprintf(fp,"&nbsp&nbsp");	// インデント+2
        	    }
			    fprintf(fp,"%s<br>\n",ss.c_str());
                ss = "";

    			RegionIndent++;
	       	}else if(Script->ScRegion == scr_subend){
		        ss += "<i><font color=\"#008000\">"+StrToHTMLStr(s)+"</font></i>";
    			RegionIndent--;
            }else{
		        ss += "<i><font color=\"#000080\">"+StrToHTMLStr(s)+"</font></i>";
//		        ss += "<i>"+StrToHTMLStr(s)+"</i>";
            }
		}else{
	        ss += StrToHTMLStr(s);
/*
			if(Token->TokId == tid_END){	// Endの後は、改行追加
				ss += "<br>";
			}
*/
		}
	}
    fprintf(fp,"</body>\n");
    fprintf(fp,"</html>\n");

    fclose(fp);
}
/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    TuneRegion  対象チューニングリージョンクラス
//
//  3.概要
//    各TuneRegionごとの結果ページを作成する。
//
//  4.機能説明
//
//  5.戻り値
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
void TVisualDM::MakeResultHTML(TTuneRegion *TuneRegion)
{
	FILE *fp;
	string FileName;
	string s;
    int i,c;
    TSubRegion *SubRegion;

	FileName = DirName + "/OAT/OATLog_"+TuneRegion->Name+".html";
	fp = fopen(FileName.c_str(),"wt");
	if(fp == NULL){
     	MainF->ErrMessage(-1,"File Open Err "+ FileName);
        return;
    }
    //
    // HTMLファイルを出力する。
    //
    fprintf(fp,"<html>\n");
    fprintf(fp,"<head>\n");
	fprintf(fp,"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=shift_jis\">\n");
	fprintf(fp,"<meta name=\"GENERATOR\" content=\"OAT 1.0\">\n");
	fprintf(fp,"<title>OAT_%s Result</title>\n",TuneRegion->Name.c_str());
	fprintf(fp,"</head>\n");

	fprintf(fp,"<body>\n");
	fprintf(fp,"<h1 align = \"center\"><b>OAT_%s&nbsp;Result</b>\n",TuneRegion->Name.c_str());
	fprintf(fp,"</h1>\n");

    //
    // Applet部分を出力する。
    // 各ページごとに参照データが異なる。
    // データファイルは、空白で作成されているので、読込みエラーにはならない。
    //
    fprintf(fp,"<p align = \"center\">\n");
    fprintf(fp,"<applet\n");
    fprintf(fp,"codebase = \"./\"\n");
    fprintf(fp,"code = \"vlapplet.Applet1.class\"\n");
    fprintf(fp,"archive = \"vlapplet.jar\"\n");

    fprintf(fp,"name = \"gtable\"\n");
    fprintf(fp,"width = \"640\"\n");
    fprintf(fp,"height = \"510\"\n");
    fprintf(fp,"alt = \"Javaアプレットです。\">\n");
    fprintf(fp,"<param name=\"FolderName\" value=\"gtable-\">\n");
	fprintf(fp,"<param name=\"FILENAME\" value=\"OATLog_%s.dat\">\n",TuneRegion->Name.c_str());

    fprintf(fp,"<param name=\"LSMFILENAME\" value=\"%s_%c_LSM.dat\">\n",
    	TuneRegion->Name.c_str(),TuneRegion->TuneGroupName[1]
    );

    fprintf(fp,"<param name=\"CaseCount\" value=\"%d\">\n",TuneRegion->CaseCount);
	//
	// SubRegionごとの個数（前のＩｄｘを含んだ個数）をセット
    //
    fprintf(fp,"<param name=\"SubCount\" value=\"%d\">\n",TuneRegion->SubRegionList->Count);
    s = "";
    c = 0;
	for(i = 0 ; i < TuneRegion->SubRegionList->Count ; i++){
   		SubRegion = (TSubRegion *)TuneRegion->SubRegionList->Items[i];
		c += SubRegion->CaseCount;
		if(s != ""){
        	s += ",";
        }
        s += IntToStr(c);
    }
    fprintf(fp,"<param name=\"SubCaseIdx\" value=\"%s\">\n",s.c_str());
    fprintf(fp,"</applet>\n");
    fprintf(fp,"</p>\n");

	//
    // データ一覧表示ボタン
    //
    fprintf(fp,"<form>\n");
//    fprintf(fp,"<p align=\"right\">\n");
//    fprintf(fp,"<INPUT TYPE=\"button\" VALUE=\"結果データ 一覧表示\"\n");
    fprintf(fp,"<p align=\"center\">\n");
    fprintf(fp,"<INPUT TYPE=\"button\" VALUE=\" Result data list   \"\n");
    fprintf(fp,"  onClick=\"makeNewWindow()\">\n");
    fprintf(fp,"</p>\n");
    fprintf(fp,"</form>\n");


    //
    // Appletから Nの開始、終了、Stepの３データを所得する。
    // このデータとCase数から計算した値とデータ行数が一致しない場合は、
    // エラー表示（データが正しくありません。）が行われる。
    //

    fprintf(fp,"<script type=\"text/javascript\">\n");
    fprintf(fp,"<!--\n");

    //
    // Table部分の表示
    // RowCount数だけのLoopでの表示となる。
	//
    fprintf(fp,"var nw = null;\n");

    fprintf(fp,"function makeNewWindow() {\n");
    //
	// Check Applet Enable
    //
    fprintf(fp,"  if(typeof(document.applets[0]) == typeof(undefined)){\n");
    fprintf(fp,"    alert(\"JavaAppletが起動していません。\");\n");
    fprintf(fp,"    return;\n");
    fprintf(fp,"  }\n");
	//
    // Open New Window
    //
    fprintf(fp,"  if(!nw || nw.closed) {\n");
	fprintf(fp,"    nw = window.open(\"\",\"\",\"WIDTH=500,HEIGHT=400,resizable,scrollbars\");\n");

    //
	// 新しいウィンドウのデータを出力
    //
    fprintf(fp,"    nw.document.writeln(\"<html>\");\n");
    fprintf(fp,"    nw.document.writeln(\"<head>\");\n");
	s = "<meta http-equiv=\\\"Content-Type\\\" content=\\\"text/html; charset=shift_jis\\\">";
    fprintf(fp,"    nw.document.writeln(\"%s\");\n",s.c_str());
	s = "<meta name=\\\"GENERATOR\\\" content=\\\"OAT 1.0\\\">";
	fprintf(fp,"    nw.document.writeln(\"%s\");\n",s.c_str());
	s ="<title>OAT_"+TuneRegion->Name+" Result</title>";
	fprintf(fp,"    nw.document.writeln(\"%s\");\n",s.c_str());
	fprintf(fp,"    nw.document.writeln(\"</head>\");\n");
	fprintf(fp,"    nw.document.writeln(\"<body>\");\n");
	s = "<h1 align = \\\"center\\\"><b>OAT_"+TuneRegion->Name+"&nbsp;Result</b>";
	fprintf(fp,"    nw.document.writeln(\"%s\");\n",s.c_str());
    fprintf(fp,"    nw.document.writeln(\"</h1>\");\n");

	    //
	    // テーブル内容を出力
    	//
	    fprintf(fp,"    var ValId = document.gtable.getId(\"d\");\n");
	    fprintf(fp,"    var DataCount = parseInt(document.gtable.getCount(ValId));\n");
    	fprintf(fp,"    var RowCount = (DataCount/2)-5;\n"); // 先頭５行はヘッダ部分
	    fprintf(fp,"    var NumProcs = parseInt(document.gtable.get(ValId,0));\n");
    	fprintf(fp,"    var StartTuneSize = parseInt(document.gtable.get(ValId,1));\n");
		fprintf(fp,"    var EndTuneSize = parseInt(document.gtable.get(ValId,2));\n");
	    fprintf(fp,"    var SampDist = parseInt(document.gtable.get(ValId,3));\n");

	    fprintf(fp,"    nw.document.writeln(\"<table border=\\\"2\\\" width=\\\"100%%\\\">\");\n");
	    fprintf(fp,"    nw.document.writeln(\"<tr>\");\n");
	    fprintf(fp,"    nw.document.writeln(\"<td width=\\\"10%%\\\">DataNo</td>\");\n");
    	fprintf(fp,"    nw.document.writeln(\"<td width=\\\"10%%\\\">N</td>\");\n");
	    fprintf(fp,"    nw.document.writeln(\"<td width=\\\"10%%\\\">CaseNo</td>\");\n");
    	fprintf(fp,"    nw.document.writeln(\"<td width=\\\"25%%\\\">CaseInfo</td>\");\n");
	    fprintf(fp,"    nw.document.writeln(\"<td width=\\\"45%%\\\">Result</td>\");\n");
    	fprintf(fp,"    nw.document.writeln(\"</tr>\");\n");

	    fprintf(fp,"    for(i = 0 ; i < RowCount ; i++){\n");
	    fprintf(fp,"      nw.document.writeln(\"\");\n");
	    fprintf(fp,"      i0 = i+1;\n");	//
	    fprintf(fp,"      n = Math.floor(i / %d)*SampDist+StartTuneSize;\n",TuneRegion->CaseCount);
	    fprintf(fp,"      i1 = (i %% %d)+1;\n",TuneRegion->CaseCount);
	    fprintf(fp,"      d = document.gtable.get(ValId,i+5);\n");

	    fprintf(fp,"      CaseInfo = \"\";\n");
        if(TuneRegion->SubRegionList->Count != 0){
			int CaseCount;

		    fprintf(fp,"i2 = 1; i3 = i1-1\n");
			for(int k = 0 ; k < TuneRegion->SubRegionList->Count-1 ; k++){
	    		SubRegion = (TSubRegion *)TuneRegion->SubRegionList->Items[k];
				CaseCount = SubRegion->CaseCount;
			    fprintf(fp,"      if(i3 >= %d){ i2++; i3 -= %d}\n",CaseCount,CaseCount);
            }
		    fprintf(fp,"      CaseInfo += \"SubRegion=\"+i2;\n");
			//
            // 各サブリージョンごとの varid()のIdxを出力する。
            // VaridListのFromToの値となる。
            // i3 にサブリージョン内での idx値が入っているので、それを使用する。
            // 例えば varid(i) from 1 to 4 varid (j) from 1 to 3 ならば
            //  i = xx Math.Floor( i3 /jCount ) % iCount; となる。
            //
			for(int k = 0 ; k < TuneRegion->SubRegionList->Count ; k++){
	    		SubRegion = (TSubRegion *)TuneRegion->SubRegionList->Items[k];
                CaseCount = SubRegion->CaseCount;
				if(SubRegion->variedCount != 0){
				    fprintf(fp,"      if(i2 == %d){\n",k+1);
					for(int kk = 0 ; kk < SubRegion->variedCount ; kk++){
						string fmt;
						string VName =SubRegion->variedValName[kk]; 	// 変数名 （必須）
			/************************************************************************/
			//
			//Kogakuin Irie
			//実数とstep機能の対応のための変更
			//変更前の部分をコメントアウト
			//
						//int VStart = SubRegion->variedFromValue[kk];        // 開始値
						//int VEnd = SubRegion->variedToValue[kk];			// 終了値
						float VStart = SubRegion->variedFromValue[kk];        // 開始値
						float VEnd = SubRegion->variedToValue[kk];			// 終了値
						float VStep = SubRegion->variedStepValue[kk];			// 増分値（追加処理）
    	                int VCount;

        	            //VCount = VEnd - VStart+1;
        	            VCount = (int)((VEnd - VStart) / VStep + 1);
			//
			//ここまで
			//
			/************************************************************************/
            	        CaseCount /= VCount;

					    fprintf(fp,"        i4 = Math.floor(i3 / %d ) %% %d + %d;\n",
                        	CaseCount,VCount,(int)VStart);
				    	fprintf(fp,"        CaseInfo += \",%s=\"+i4;\n",VName.c_str());

	                }
				    fprintf(fp,"    }\n");
                }
            }
        }else{  // SubRegionがない場合の CaseInfoの出力
			int CaseCount;

            CaseCount = TuneRegion->CaseCount;
			fprintf(fp,"      i3 = i1-1\n");
			for(int kk = 0 ; kk < TuneRegion->variedCount ; kk++){
				string fmt;
				string VName = TuneRegion->variedValName[kk]; 	// 変数名 （必須）
		/***************************************************************/
		//
		//Kogakuin Irie
		//実数とstep機能の対応のための変更
		//変更前の部分をコメントアウト
		//
				//int VStart = TuneRegion->variedFromValue[kk];       // 開始値
				//int VEnd = TuneRegion->variedToValue[kk];			// 終了値
				float VStart = TuneRegion->variedFromValue[kk];       // 開始値
				float VEnd = TuneRegion->variedToValue[kk];			// 終了値
				float VStep = TuneRegion->variedStepValue[kk];			// 増分値（追加処理）
                int VCount;

   	            //VCount = VEnd - VStart+1;
        	    VCount = (int)((VEnd - VStart) / VStep + 1);
		//
		//ここまで
		//
		/***************************************************************/
       	        CaseCount /= VCount;

			    fprintf(fp,"      i4 = Math.floor(i3 / %d ) %% %d + %d;\n",
                    	CaseCount,VCount,(int)VStart);
                if(kk == 0){
			    	fprintf(fp,"      CaseInfo += \"%s=\"+i4;\n",VName.c_str());
                }else{
			    	fprintf(fp,"      CaseInfo += \",%s=\"+i4;\n",VName.c_str());
                }
	        }
        }
		s = "\"<tr>"
			"<td width=\\\"10%%\\\">\"+i0+\"</td>"
       		"<td width=\\\"10%%\\\">\"+n+\"</td>"
       		"<td width=\\\"10%%\\\">\"+i1+\"</td>"
       		"<td width=\\\"25%%\\\">\"+CaseInfo+\"</td>"
       		"<td width=\\\"45%%\\\">\"+d+\"</td>"
       		"</tr>\"";

	    fprintf(fp,"      nw.document.writeln(%s);\n",s.c_str());

	    fprintf(fp,"    }\n");
   		fprintf(fp,"    nw.document.writeln(\"</table>\");\n");

    fprintf(fp,"    nw.document.writeln(\"</body>\");\n");
    fprintf(fp,"    nw.document.writeln(\"</html>\");\n");

	fprintf(fp,"    nw.document.close();\n");
	fprintf(fp,"  }else if(nw){\n");
	fprintf(fp,"    nw.focus();\n");
	fprintf(fp,"  }\n");
    fprintf(fp,"}\n");
    fprintf(fp,"// -->\n");
	fprintf(fp,"</script>\n");
    fprintf(fp,"<noscript>\n");
    fprintf(fp,"JavaScript対応ブラウザで表示してください。\n");
    fprintf(fp,"</noscript>\n");
    fprintf(fp,"<br>\n");
    fprintf(fp,"</body>\n");

    fclose(fp);

}

/*----------------------------------------------------------------------------*/
//
//  1.日本語名
//
//  2.パラメタ説明
//    s   元文字列
//
//  3.概要
//    文字列の空白や特殊文字を HTML形式で表示可能な文字列に変換する。
//
//  4.機能説明
//
//  5.戻り値
//    変換後文字列
//
//  6.備考
//
/*----------------------------------------------------------------------------*/
string TVisualDM::StrToHTMLStr(string s)
{
	int i;
    char c;
    string rStr = "";

    for(i = 1 ; i <= (int)s.length() ; i++){
		c = s[i];
		switch(c){
        case '\n':
//			rStr += "<br>";
        	break;
        case '\r':
			break;
        case ' ':
			rStr += "&nbsp;";
        	break;
        case '<':
			rStr += "&lt;";
        	break;
        case '>':
			rStr += "&gt;";
        	break;
        case '"':
			rStr += "&quot;";
        	break;
        default:
        	rStr += c;
        }
    }
    return rStr;
}



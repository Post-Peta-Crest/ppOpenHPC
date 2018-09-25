#ifndef mainH
#define mainH

#include "VCL_Lib.h"
#include <stdio.h>
#include <string.h>

/*----------------------------------------------------------------------------*/
//  1.日本語名
//
//  2.概要
//    メインフォームクラス
//
//  3.機能説明
//    プログラムのメイン。ソースファイル名の所得やメッセージの表示、各パスの呼び
//    出しを行う。
//
//  4.備考
//
/*----------------------------------------------------------------------------*/
class TMainF
{
private:	// ユーザー宣言
	FILE *LogFp;
public:		// ユーザー宣言
	bool ErrF;
	bool CloseReqF;
	bool EndF;
	bool VisualF;						// Viusalzationを行う（HTML+データ出力)
										// かのフラグ
	bool DebugF;						// Debug出力ON/OFF (OFF時は、コメントに
										// してSpeedUp)
	bool NoMPIF;						// -NoMPI指定（MPI関連のコードをSkip）
	string TimeFunc;					// -timeで指定する時刻関数
										// MPI_Wtime() OAT_Wtime() MyTime()
	string my_timer_start;				// -stime_and_etime で指定する測定関数名
	string my_timer_stop;				// -stime_and_etime で指定する測定関数名

	bool EECntlF;						// -eectrlのフラグ(ON時は、コントロール部分を置き換え）

	string cc_option_str;				// -cc=で指定するコンパイルオプション

	string SrcFileName;					// スクリプト実行を行うソースファイル名
	string DirName;
	TList *TokenList;					// トークンリスト (Pass1で生成）
	TList *ValDataList;					// 変数リスト(Pass2で生成）
	TList *TuneRegionList;				// チューニングリージョンリスト(Pass4で
										// 生成)
	int CharMaxLen;						// 格納文字列の最大長さ(TRの名前を全て入
										// れた長さ）

	TStringList *TokStrList;

	TMainF();
	~TMainF();
	void start(int argc,char* argv[]);
	void ShowMessage(string s);
	void print(string s);
	void printNoLog(string s);
	void err(string s);
};

extern TMainF *MainF;
//void DP(AnsiString s);	// Debug用

#endif

#ifndef mainH
#define mainH

#include "VCL_Lib.h"
#include <stdio.h>
#include <string.h>

/*----------------------------------------------------------------------------*/
//  1.���ܸ�̾
//
//  2.����
//    �ᥤ��ե����९�饹
//
//  3.��ǽ����
//    �ץ����Υᥤ�󡣥������ե�����̾�ν������å�������ɽ�����ƥѥ��θƤ�
//    �Ф���Ԥ���
//
//  4.����
//
/*----------------------------------------------------------------------------*/
class TMainF
{
private:	// �桼�������
	FILE *LogFp;
public:		// �桼�������
	bool ErrF;
	bool CloseReqF;
	bool EndF;
	bool VisualF;						// Viusalzation��Ԥ���HTML+�ǡ�������)
										// ���Υե饰
	bool DebugF;						// Debug����ON/OFF (OFF���ϡ������Ȥ�
										// ����SpeedUp)
	bool NoMPIF;						// -NoMPI�����MPI��Ϣ�Υ����ɤ�Skip��
	string TimeFunc;					// -time�ǻ��ꤹ�����ؿ�
										// MPI_Wtime() OAT_Wtime() MyTime()
	string my_timer_start;				// -stime_and_etime �ǻ��ꤹ��¬��ؿ�̾
	string my_timer_stop;				// -stime_and_etime �ǻ��ꤹ��¬��ؿ�̾

	bool EECntlF;						// -eectrl�Υե饰(ON���ϡ�����ȥ�����ʬ���֤�������

	string cc_option_str;				// -cc=�ǻ��ꤹ�륳��ѥ��륪�ץ����

	string SrcFileName;					// ������ץȼ¹Ԥ�Ԥ��������ե�����̾
	string DirName;
	TList *TokenList;					// �ȡ�����ꥹ�� (Pass1��������
	TList *ValDataList;					// �ѿ��ꥹ��(Pass2��������
	TList *TuneRegionList;				// ���塼�˥󥰥꡼�����ꥹ��(Pass4��
										// ����)
	int CharMaxLen;						// ��Ǽʸ����κ���Ĺ��(TR��̾����������
										// �줿Ĺ����

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
//void DP(AnsiString s);	// Debug��

#endif

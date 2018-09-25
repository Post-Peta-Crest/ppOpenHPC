/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   ConvMain.h
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#ifndef FA40620_1E0E_430b_BB4A_45051EC37AE8
#define FA40620_1E0E_430b_BB4A_45051EC37AE8

#include "ConvHeader.h"

class CConvMain{
private:
	CConvMain();
public:
	static CConvMain* Instance(){
		static CConvMain oConv;
		return &oConv;
	}
	virtual ~CConvMain();

private:
	CAssyModel moAssyModel;

	CFileReader moReader;
	CFileWriter moWriter;

public:
	void FileRead_FISTR4(string filename, bool opflag);// �t�@�C������:FrontISTR v4 ���b�V��
	void FileWrite_MW3(string filename);  // �t�@�C���o��:MW3 ���b�V��

	bool setupAssyModel();// AssyModel�̏o�͏���(�f�[�^�ϊ�����)
};
#endif //include_guard



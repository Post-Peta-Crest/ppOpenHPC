/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   FileBase.h
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#ifndef A4265CE_2ED8_431a_80E5_91E17F79EBAA
#define A4265CE_2ED8_431a_80E5_91E17F79EBAA

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <utility>
#include <vector>
#include <algorithm>
using namespace std;

typedef vector<string> vstring;

#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>
#include <boost/xpressive/xpressive.hpp>
#include <boost/format.hpp>
using namespace boost;

#include "FrontISTR_Type.h"
#include "Message.h"

class CFileBase{
public:
	CFileBase();
	virtual ~CFileBase();

protected:
	vstring SplitToken(string sLine);// �g�[�N������
	void replaceComma(string& sLine);// ','comma �� ' '�X�y�[�X�u������

	bool existStr(const string& sLine, string str);// ������̑��ݗL��
	string getRearStr(string sLine, string symbol);// �L���ȍ~�̕�����擾
};
#endif //include_guard







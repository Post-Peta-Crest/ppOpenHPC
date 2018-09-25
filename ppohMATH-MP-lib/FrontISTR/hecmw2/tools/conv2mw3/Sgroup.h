/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   Sgroup.h
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#ifndef DC03BBA7_5463_4bc3_889E_E8202FDE0E98
#define DC03BBA7_5463_4bc3_889E_E8202FDE0E98

#include "Group.h"

class CSgroup:public CGroup{
public:
	CSgroup();
	virtual ~CSgroup();

private:
	vector<pair<size_t,size_t> > mvElemFace;//���͗p:�v�f�ԍ�-�Ǐ��ʔԍ��y�A(FrontISTR v4)


	vector<pair<size_t,size_t> > mvElemFaceMW3;//�v�f�ԍ�-�Ǐ��ʔԍ��y�A(MW3)
	vector<CNode*> mvNode;       //Sgroup�ɏ�������Node*
	map<size_t,size_t> mmNID2NIX;//Node_id��index

	vector<size_t> mvFaceNodeNum;//�ʂ̐ߓ_��
	vector<string> mvFaceType;   //�ʂ̃^�C�v
	vector<size_t> mvMW3FaceNum; //�ʔԍ�(MW3)
	vector<vector<size_t> > mvvBNodeNum;//�ʍ\����BNode�ԍ�(index�ԍ�)
	vector<vector<size_t> > mvvFaceNodeID;//�ʍ\����NodeID

public:
	void addElemFaceID(size_t nElemID, size_t nFaceN);

	//--
	// DATA�o�͏���
	//--
	bool setup(map<size_t,CNode*> mNode, map<size_t,CElement*> mElem);

	//--
	// �o��
	//--
	size_t getNumOfNode();
	CNode* getNode(size_t index);

	size_t getNumOfFace();
	size_t getElemID(size_t index);
	size_t getFaceN(size_t index);//�ʔԍ� ���͒l�̂܂�.

	size_t getFaceNodeNum(size_t index);
	string getFaceType(size_t index);
	size_t getConvMW3FaceN(size_t index);//FrontISTR��MW3 �ʔԍ�
	vector<size_t> getBNodeN_Face(size_t index);//�ʂ��\������BoundaryNode�ԍ�(mvNode��index�ԍ�)

	vector<size_t> getFaceNodeID(size_t index);//���f�[�^(���͒l)

	map<size_t,size_t> getNID2NIX(){ return mmNID2NIX;}
	
};
#endif //include_guard



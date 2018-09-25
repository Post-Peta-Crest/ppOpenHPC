/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   Lgroup.h
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#ifndef C83972A5_3871_4c48_85B0_C0145BD6E9BC
#define C83972A5_3871_4c48_85B0_C0145BD6E9BC

#include "Group.h"

class CLgroup:public CGroup{
public:
	CLgroup();
	virtual ~CLgroup();

private:
	vector<pair<size_t,size_t> > mvElemEdge;//���͗p:�v�f�ԍ�-�Ǐ��Ӕԍ��y�A(FrontISTR v4)

	vector<CNode*>  mvNode;       //Lgroup�ɏ�������Node*
	map<size_t, size_t> mmNID2NIX;//Node_id��index

	vector<string> mvEdgeType;
	vector<size_t> mvMW3EdgeNum;
	vector<vector<size_t> > mvvBNodeNum;//�Ӎ\����BNode�ԍ�(index�ԍ�)

public:
	void addElemEdgeID(size_t nElemID, size_t nEdgeN);

	size_t getNumOfNode();
	size_t getNumOfEdge();
	CNode* getNode(size_t index);

	string getEdgeType(size_t iedge);
	size_t getElemID(size_t iedge);
	size_t getConvMW3EdgeN(size_t iedge);

	vector<size_t> getBNode_Edge(size_t iedge);

	//--
	// DATA�o�͏���
	//--
	bool setup(map<size_t,CNode*> mNode, map<size_t,CElement*> mElem);
};
#endif//include guard


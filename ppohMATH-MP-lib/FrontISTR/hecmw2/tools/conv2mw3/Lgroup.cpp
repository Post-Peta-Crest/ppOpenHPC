/*
 ----------------------------------------------------------
|
| Software Name :conv2mw3 Ver 0.1 beta
|
|   Lgroup.cpp
|
|                     Written by T.Takeda,    2012/06/01
|                                Y.Sato,      2012/06/01
|
|   Contact address : IIS, The University of Tokyo CISS
|
 ----------------------------------------------------------
*/
#include "Lgroup.h"

CLgroup::CLgroup()
{
}
CLgroup::~CLgroup()
{
}

void CLgroup::addElemEdgeID(size_t nElemID, size_t nEdgeN)
{
	pair<size_t,size_t> prElemEdge;

	prElemEdge.first= nElemID;
	prElemEdge.second=nEdgeN;

	mvElemEdge.push_back(prElemEdge);
}

//--
// DATA�o�͏���
//--
bool CLgroup::setup(map<size_t,CNode*> mNode, map<size_t,CElement*> mElem)
{
	vector<size_t> vNID;//�ߓ_ID

	for(size_t i=0; i < mvElemEdge.size(); i++){
		size_t nEID = mvElemEdge[i].first; //FrontISTR �v�fID
		size_t nEdge= mvElemEdge[i].second;//FrontISTR �ʔԍ�
		
		CElement *pElem= mElem[nEID];
		vector<CNode*> vNode= pElem->getFistrEdgeNode(nEdge);//FrontISTR �Ӕԍ��̐ߓ_�Q

		for(size_t ii=0; ii < vNode.size(); ii++){
			vNID.push_back( vNode[ii]->getID() );
		};
	};
	sort(vNID.begin(), vNID.end());
  vector<size_t>::iterator new_end = unique(vNID.begin(), vNID.end());
  vNID.erase(new_end, vNID.end());

	//�o�͗p Node* �z��
	for(size_t i=0; i < vNID.size(); i++){
		size_t nNID= vNID[i];
		mvNode.push_back( mNode[nNID] );
	};
	//�o�͗p NodeID��index
	for(size_t i=0; i < mvNode.size(); i++){
		CNode *pNode=mvNode[i];
		mmNID2NIX[pNode->getID()]=i;
	};
	//�ӂ̃^�C�v
	for(size_t i=0; i < mvElemEdge.size(); i++){
		size_t nEID = mvElemEdge[i].first; //FrontISTR �v�fID
		size_t nEdge= mvElemEdge[i].second;//FrontISTR �Ӕԍ�

		CElement *pElem= mElem[nEID];
		mvEdgeType.push_back( pElem->getEdgeType(nEdge) );
	};

	//�v�f�Ӕԍ�(MW3)
	for(size_t i=0; i < mvElemEdge.size(); i++){
		size_t nEID = mvElemEdge[i].first;
		size_t nEdge= mvElemEdge[i].second;//FrontISTR �Ӕԍ�

		CElement *pElem=mElem[nEID];

		size_t nMW3EdgeN= pElem->getMW3EdgeNum(nEdge);
		mvMW3EdgeNum.push_back(nMW3EdgeN);
	};

	//�Ӎ\����BNode�ԍ�(index�ԍ�)
	for(size_t i=0; i < mvElemEdge.size(); i++){
		size_t nEID = mvElemEdge[i].first;
		size_t nEdge= mvElemEdge[i].second;//FrontISTR �Ӕԍ�

		CElement *pElem=mElem[nEID];

		vector<size_t> vIndex;
		vector<CNode*> vNode=pElem->getFistrEdgeNode(nEdge);
		for(size_t ii=0; ii < vNode.size(); ii++){
			CNode *pNode= vNode[ii];
			vIndex.push_back( mmNID2NIX[pNode->getID()] );
		};
		mvvBNodeNum.push_back( vIndex );
	};

	return true;
}


size_t CLgroup::getNumOfNode()
{
	return mvNode.size();
}
size_t CLgroup::getNumOfEdge()
{
	return mvElemEdge.size();
}
CNode* CLgroup::getNode(size_t index)
{
	return mvNode[index];
}
string CLgroup::getEdgeType(size_t iedge)
{
	return mvEdgeType[iedge];
}
size_t CLgroup::getElemID(size_t iedge)
{
	return mvElemEdge[iedge].first;
}
size_t CLgroup::getConvMW3EdgeN(size_t iedge)
{
	return mvMW3EdgeNum[iedge];
}
vector<size_t> CLgroup::getBNode_Edge(size_t iedge)
{
	return mvvBNodeNum[iedge];
}



